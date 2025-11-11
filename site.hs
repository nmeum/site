{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import qualified Data.Char as Char
import qualified Data.Text as T
import Hakyll hiding (renderTagList, tagsRules)
import Hakyll.Core.Compiler.Internal (compilerTellDependencies)
import Hakyll.Core.Dependencies
import System.FilePath (replaceExtension, takeExtension)
import Text.Blaze ((!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Pandoc as P
import Text.Pandoc.Walk (walk)

toLower :: String -> String
toLower = fmap Char.toLower

getMetadataItems :: [Identifier] -> Compiler [Item Metadata]
getMetadataItems identifiers = do
  mds <- mapM getMetadata identifiers
  pure $ zipWith Item identifiers mds

------------------------------------------------------------------------

linkToTag :: T.Text -> P.Inline
linkToTag name =
  let desc = "All pages tagged '" `T.append` name `T.append` "'"
      file = "/tags/" `T.append` T.toLower name `T.append` ".html"
    in P.Link ("", [], []) [P.Str name] (file, desc)

-- Transform zk references to .md files to .html files.
fixupNoteRefs :: Item String -> Compiler (Item String)
fixupNoteRefs = pure . fmap (withUrls go)
 where
  go :: String -> String
  go url
    | isZkRef url = replaceExtension url ".html"
    | otherwise = url

  -- Returns true if the URL is a reference to another zk note.
  isZkRef :: String -> Bool
  isZkRef url =
    let ext = takeExtension url
      in not (isExternal url) && (ext == "" || ext == ".md")

pandocCompilerZk :: Compiler (Item String)
pandocCompilerZk =
  cached "pandocCompilerZk" $
    pandocCompilerWithTransform
      defaultHakyllReaderOptions
      defaultHakyllWriterOptions
      (walk transform)
  where
    transform :: P.Block -> P.Block
    transform = walk transformInlines

    -- TODO: rewrite this using a fold
    transformInlines :: [P.Inline] -> [P.Inline]
    transformInlines (i@(P.Str text) : ix) =
      -- Implements support for tags using Bear's multi-word tag syntax.
      --
      -- See:
      --   • https://github.com/zk-org/zk/blob/dev/docs/notes/tags.md>
      --   • https://github.com/zk-org/zk/blob/v0.15.1/internal/adapter/markdown/extensions/tag.go#L79
      case T.uncons text of
        -- TODO: Might be easier to just use a regex here.
        Just ('#', xs) ->
          let tag = T.takeWhile (/= '#') xs
              rst = T.drop (T.length tag + 1) xs
           in [linkToTag tag, P.Str rst] ++ transformInlines ix
        _ -> i : transformInlines ix
    transformInlines (i : ix) = i : transformInlines ix
    transformInlines [] = []

------------------------------------------------------------------------

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

-- Custom variant of tagsRules which doesn't introduce any dependencies.
tagsRules :: Tags -> (String -> [Identifier] -> Rules ()) -> Rules ()
tagsRules tags rules =
  forM_ (tagsMap tags) $ \(tag, identifiers) ->
    create [tagsMakeId tags tag] $
      rules tag identifiers

------------------------------------------------------------------------

notesField :: String -> [Item a] -> Context b
notesField name notes =
  listField name (dateCtx <> urlField "url" <> metadataField) (pure notes)
    <> titleField "title"

config :: Configuration
config = defaultConfiguration
  { deployCommand = "./deploy.sh" }

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    tags <- buildTags "notes/*" (fromCapture "tags/*.html" . toLower)
    create ["sidebar"] $ do
      deps <- makePatternDependency KindMetadata (fromGlob "notes/*")
      compile $ do
        -- XXX: renderTagList/renderTags does not tell dependencies itself.
        -- https://github.com/jaspervdj/hakyll/blob/v4.16.7.1/lib/Hakyll/Web/Tags.hs#L183
        compilerTellDependencies [deps]

        allTagsCtx <- constField "allTags" <$> renderTagList tags
        makeItem []
          >>= loadAndApplyTemplate "templates/sidebar.html" allTagsCtx

    match "index.html" $ do
      route idRoute
      compile $ do
        sidebar <- constField "sidebar" <$> loadBody "sidebar"
        getResourceBody
            >>= loadAndApplyTemplate "templates/default.html" (sidebar <> noteCtx)
            >>= relativizeUrls

    match "notes/*" $ do
      route $ setExtension "html"
      compile $ do
        sidebar <- constField "sidebar" <$> loadBody "sidebar"
        let postTags = tagsField "tags" tags

        pandocCompilerZk
          >>= fixupNoteRefs
          >>= loadAndApplyTemplate "templates/note.html" (postTags <> noteCtx)
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/default.html" (sidebar <> noteCtx)
          >>= relativizeUrls

    tagsRules tags $ \tagStr tagIds -> do
      route idRoute
      compile $ do
        sidebar <- constField "sidebar" <$> loadBody "sidebar"
        let pageTitle = constField "title" tagStr

        notes <- getMetadataItems tagIds >>= recentFirst
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
