{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Maybe (fromJust)
import qualified Data.Char as Char
import qualified Data.Text as T
import Hakyll hiding (renderTagList)
import Hakyll.Core.Compiler.Internal (compilerTellDependencies)
import Hakyll.Core.Dependencies
import System.FilePath (replaceExtension, takeExtension)
import Text.Blaze ((!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Pandoc as P
import Text.Pandoc.Highlighting (Style, haddock, styleToCss)
import Text.Pandoc.Walk (walk)

toLower :: String -> String
toLower = fmap Char.toLower

------------------------------------------------------------------------

linkToTag :: T.Text -> P.Inline
linkToTag name =
  let desc = "All pages tagged '" `T.append` name `T.append` "'"
      file = "/tags/" `T.append` T.toLower name `T.append` ".html"
    in P.Link ("", [], []) [P.Str name] (file, desc)

-- Transform zk references to .md files to .html files.
--
-- TODO: Filter out references to private notes.
fixupNoteRefs :: Item String -> Compiler (Item String)
fixupNoteRefs = pure . fmap (withUrls go)
 where
  go :: String -> String
  go url
    | isZkRef url = replaceExtension url ".html"
    | otherwise = url

  -- Returns true if the URL is a reference to another zk note.
  isZkRef :: String -> Bool
  isZkRef ('#' : _) = False
  isZkRef url =
    let ext = takeExtension url
      in not (isExternal url) && (ext == "" || ext == ".md")

-- Implements support for tags using Bear's multi-word tag syntax.
--
-- See:
--   • https://github.com/zk-org/zk/blob/dev/docs/notes/tags.md>
--   • https://github.com/zk-org/zk/blob/v0.15.1/internal/adapter/markdown/extensions/tag.go#L79
inlineBearTags :: [P.Inline] -> [P.Inline]
inlineBearTags (i@(P.Str (T.stripPrefix "#" -> Just tagRst)) : ix) =
  case takeTagElems (P.Str tagRst : ix) of
    Nothing -> i : inlineBearTags ix
    Just el ->
      let (tag, rst) = splitTag $ T.unwords el
          numElement = (length el - 1) * 2 -- count P.Space too
       in [linkToTag tag, P.Str rst] ++ inlineBearTags (drop numElement ix)
  where
    takeTagElems :: [P.Inline] -> Maybe [T.Text]
    takeTagElems (P.Str str : xs)
      | T.elem '#' str = Just [str]
      | otherwise = (str :) <$> takeTagElems xs
    takeTagElems (P.Space : xs) = takeTagElems xs
    takeTagElems _ = Nothing

    splitTag :: T.Text -> (T.Text, T.Text)
    splitTag t = splitAtEx (fromJust $ T.findIndex (== '#') t) t

    -- Like T.splitAt because exclude the seperator in 'snd'.
    splitAtEx :: Int -> T.Text -> (T.Text, T.Text)
    splitAtEx n t = let (b, a) = T.splitAt n t in (b, T.drop 1 a)
inlineBearTags (i : ix) = i : inlineBearTags ix
inlineBearTags [] = []

pandocCodeStyle :: Style
pandocCodeStyle = haddock

pandocCompilerZk :: Compiler (Item String)
pandocCompilerZk =
  cached "pandocCompilerZk" $
    (pandocCompilerZk' >>= fixupNoteRefs)
  where
    pandocCompilerZk' :: Compiler (Item String)
    pandocCompilerZk' =
      pandocCompilerWithTransform
       defaultHakyllReaderOptions
         { P.readerStripComments = True }
       defaultHakyllWriterOptions
         { P.writerHighlightStyle = Just pandocCodeStyle }
       (walk transform)

    transform :: P.Block -> P.Block
    transform = walk inlineBearTags

------------------------------------------------------------------------

getMetadataItems :: Pattern -> Compiler [Item Metadata]
getMetadataItems pattern = map (uncurry Item) <$> getAllMetadata pattern

-- Custom version of Hakyll's renderTagList.
-- TODO: Maybe produce a Context here.
renderTagList :: Tags -> Compiler String
renderTagList = renderTags makeLink concat
  where
    makeLink tag url count _minCount _maxCount =
      renderHtml . H.li
        $ H.a
          ! A.href (H.toValue url)
          ! A.class_ "tag"
          ! A.title ("Navigate posts by tag '" <> H.stringValue tag <> "'")
        $ H.toHtml (tag ++ " (" ++ show count ++ ")")

------------------------------------------------------------------------

config :: Configuration
config = defaultConfiguration
  { deployCommand = "./deploy.sh" }

main :: IO ()
main = hakyllWith config $ do
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
    create ["css/syntax.css"] $ do
        route idRoute
        compile $
          makeItem $ (compressCss $ styleToCss pandocCodeStyle)

    tags <- buildTags "notes/*" (fromCapture "tags/*.html" . toLower)
    create ["sidebar"] $ do
      deps <- makePatternDependency KindMetadata (fromGlob "notes/*")
      compile $ do
        -- XXX: renderTagList/renderTags does not tell dependencies itself.
        -- It cannot do so properly as it does not receive a 'Pattern' and
        -- can thus not express the need for rebuilds on newly added pages.
        --
        -- https://github.com/jaspervdj/hakyll/blob/v4.16.7.1/lib/Hakyll/Web/Tags.hs#L183
        compilerTellDependencies [deps]

        allTagsCtx <- constField "allTags" <$> renderTagList tags
        makeItem []
          >>= loadAndApplyTemplate "templates/sidebar.html" allTagsCtx

    match "index.md" $ do
      route $ setExtension "html"
      compile $ do
        notes   <- getMetadataItems (fromGlob "notes/*") >>= recentFirst
        sidebar <- constField "sidebar" <$> loadBody "sidebar"

        pandocCompilerZk
            >>= loadAndApplyTemplate
                    "templates/index.html"
                    (notesField "notes" (take 5 notes) <> defaultContext)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" (sidebar <> noteCtx)
            >>= relativizeUrls

    match "notes/*" $ do
      route $ setExtension "html"
      compile $ do
        sidebar <- constField "sidebar" <$> loadBody "sidebar"
        let postTags = tagsField "tags" tags

        pandocCompilerZk
          >>= loadAndApplyTemplate "templates/note.html" (postTags <> noteCtx)
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/default.html" (sidebar <> noteCtx)
          >>= relativizeUrls

    tagsRules tags $ \tagStr tagsPattern -> do
      route idRoute
      compile $ do
        sidebar <- constField "sidebar" <$> loadBody "sidebar"
        let pageTitle = constField "title" tagStr

        notes <- getMetadataItems tagsPattern >>= recentFirst
        let baseCtx = pageTitle <> sidebar <> defaultContext
            listCtx = pageTitle
                <> notesField "notes" notes
                <> defaultContext

        makeItem []
          >>= loadAndApplyTemplate "templates/notes.html" listCtx
          >>= loadAndApplyTemplate "templates/default.html" baseCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

dateCtx :: Context b
dateCtx =
  dateField "date" "%B %e, %Y"
    -- See https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/time#valid_datetime_values
    <> dateField "datetime" "%F"

noteCtx :: Context String
noteCtx = dateCtx <> defaultContext

notesField :: String -> [Item a] -> Context b
notesField name notes =
  listField name (dateCtx <> urlField "url" <> metadataField) (pure notes)
    <> titleField "title"
