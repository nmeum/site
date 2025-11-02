{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.Text as T
import Data.Binary (Binary)
import qualified Data.Char as Char
import Data.Typeable (Typeable)
import Hakyll hiding (renderTagList)
import Hakyll.Core.Compiler.Internal
  ( compilerAsk,
    compilerStore,
    compilerTellCacheHits,
    compilerTellDependencies,
    compilerUnderlying,
    compilerUnsafeIO,
  )
import qualified Hakyll.Core.Store as Store
import System.Environment (getProgName)
import System.FilePath (takeExtension, replaceExtension)
import Text.Pandoc.Walk (walk)
import qualified Text.Pandoc as P
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze ((!))

toLower :: String -> String
toLower = fmap Char.toLower

--------------------------------------------------------------------------------

-- This is a hack around the existing 'cached' function [1]. While the cache is
-- largely undocumented [2], the existing function only works for resources
-- (see 'Hakyll.Core.Provider') that are file-backed. This is due to the fact
-- that 'resourceModified' is only defined for such resources. The 'cached'
-- function uses 'resourceModified' for cache invalidation, it invalidates the
-- cache if the resource was modified).
--
-- In our case, we want to cache a resource that is not file-backed.
-- Specifically, the sidebar resource. Instead of relying on 'resourceModified'
-- for cache invalidation we use the provided key for this purpose. That is,
-- the resource is rebuild if an entry with the given key doesn't exist in the
-- cache. In the case of the sidebar, the key corresponds to the sidebar items
-- (i.e. the tags).
--
-- Ideally, we would want to express this in the dependency tree. That is, we
-- want to tell Hakyll “don't rebuild the sidebar unless the tags changed”.
-- However, we cann't do that because Hakyll—like make(1)—can only express
-- dependencies on file content not metadata content [2]. To workaround that
-- limitation, we use the cache.
--
-- [1]: https://github.com/jaspervdj/hakyll/blob/v4.16.7.1/lib/Hakyll/Core/Compiler.hs#L149-185
-- [2]: https://github.com/jaspervdj/hakyll/issues/467
-- [3]: https://github.com/jaspervdj/hakyll/issues/383#issuecomment-150836917
cacheIfExists :: (Show k, Binary a, Typeable a) => k -> Compiler a -> Compiler a
cacheIfExists key compiler = do
    id'   <- compilerUnderlying <$> compilerAsk
    store <- compilerStore      <$> compilerAsk

    let k = [show key, show id']
        go = compiler >>= \v -> v <$ compilerUnsafeIO (Store.set store k v)
    compilerUnsafeIO (Store.get store k) >>= \r -> case r of
        -- found: report cache hit and return value
        Store.Found v   -> v <$ compilerTellCacheHits 1
        -- not found: unexpected, but recoverable
        Store.NotFound  -> go
        -- other results: unrecoverable error
        _               -> fail . error' =<< compilerUnsafeIO getProgName
  where
    error' progName =
        "Hakyll.Core.Compiler.cached: Cache corrupt! " ++
         "Try running: " ++ progName ++ " clean"

------------------------------------------------------------------------

linkToTag :: T.Text -> P.Inline
linkToTag name =
  let desc = "All pages tagged '" `T.append` name `T.append` "'"
      file = "tags/" `T.append` T.toLower name `T.append` ".html"
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
    transform block = walk transformInlines block

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

config :: Configuration
config = defaultConfiguration
  { deployCommand = "rsync --delete-excluded --checksum --progress -av _site/ \
                     \magnesium:/var/www/htdocs/notes.8pit.net/" }

-- Custom version of Hakyll's renderTagList.
-- TODO: Maybe produce a Context here.
renderTagList :: Tags -> Compiler (String)
renderTagList = renderTags makeLink concat
  where
    makeLink tag url count _minCount _maxCount =
      renderHtml . H.li
        $ H.a
          ! A.href (H.toValue url)
          ! A.class_ "tag"
          ! A.title ("Navigate posts by tag '" <> H.stringValue tag <> "'")
        $ H.toHtml (tag ++ " (" ++ show count ++ ")")

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Hakyll's dependency tracking does not work well with resources depending
    -- on metadata (such as tags or dates). To reduces unnecessary rebuilds
    -- because of post content (not metadata) changes, we build the sidebar
    -- separately with custom caching (see 'cacheIfExists') above.
    tags <- buildTags "notes/*" (fromCapture "tags/*.html" . toLower)
    create ["sidebar"] $ do
      deps <- makePatternDependency (fromGlob "notes/*")
      compile $ do
        -- XXX: renderTagList/renderTags does not tell dependencies itself.
        -- https://github.com/jaspervdj/hakyll/blob/v4.16.7.1/lib/Hakyll/Web/Tags.hs#L183
        compilerTellDependencies [deps]

        allTagsCtx <- constField "allTags" <$> renderTagList tags
        cacheIfExists (tagsMap tags) $ makeItem []
          >>= loadAndApplyTemplate "templates/sidebar.html" allTagsCtx

    match "index.html" $ do
      route idRoute
      compile $ do
        sidebar <- constField "sidebar" <$> loadBody "sidebar"
        (cached "index" $ getResourceBody)
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

    tagsRules tags $ \tagStr tagsPattern -> do
      route idRoute
      compile $ do
        sidebar <- constField "sidebar" <$> loadBody "sidebar"
        let pageTitle = constField "title" tagStr

        notes <- loadAll tagsPattern >>= recentFirst
        let baseCtx = pageTitle <> sidebar <> defaultContext
            listCtx = pageTitle
                <> listField "notes" noteCtx (return notes)
                <> defaultContext

        -- XXX: cache the notes.html file and leave it as is unless the metadata
        -- of the posts on this page changed (i.e., cache it if the body changed).
        notesMeta <- mapM getMetadata (map itemIdentifier notes)
        let notesItem =
              cacheIfExists notesMeta $
                makeItem [] >>= loadAndApplyTemplate "templates/notes.html" listCtx

        notesItem
          >>= loadAndApplyTemplate "templates/default.html" baseCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

noteCtx :: Context String
noteCtx =
    dateField "date" "%B %e, %Y"
      -- See https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/time#valid_datetime_values
      <> dateField "datetime" "%F"
      <> defaultContext
