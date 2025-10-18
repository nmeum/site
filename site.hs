{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Binary (Binary)
import qualified Data.Char as Char
import qualified Data.Map as Map
import Data.Typeable (Typeable)
import Hakyll
import Hakyll.Core.Compiler.Internal
  ( compilerAsk,
    compilerProvider,
    compilerStore,
    compilerTellCacheHits,
    compilerTellDependencies,
    compilerUnderlying,
    compilerUnsafeIO,
  )
import qualified Hakyll.Core.Store as Store
import System.Environment (getProgName)

toLower :: String -> String
toLower = fmap Char.toLower

captialize :: String -> String
captialize "" = ""
capitalize (x:xs) = Char.toUpper x : xs

------------------------------------------------------------------------

-- Taken from https://github.com/dwarfmaster/website/blob/dd437a175c3172bc4ee5c24a0e19b6e60ac8ed7d/site.hs#L12

getAllTags :: Tags -> Compiler [Item (String, [Identifier])]
getAllTags tags = pure . map mkItem $ tagsMap tags
  where
    mkItem :: (String, [Identifier]) -> Item (String, [Identifier])
    mkItem x@(t, _) = Item (tagsMakeId tags t) x

tagsCtx :: Map.Map String [Identifier] -> Context (String,[Identifier])
tagsCtx tmap = listFieldWith "notes" noteCtx getPosts
       <> metadataField
       <> urlField "url"
       <> pathField "path"
       <> field "title" (\(itemBody -> (t,_)) -> pure t)
       <> field "count" (\(itemBody -> (t,_)) -> pure (show $ countTag t tmap))
       <> missingField
 where
  getPosts :: Item (String,[Identifier]) -> Compiler [Item String]
  getPosts (itemBody -> (_,is)) = mapM load is

  countTag :: String -> Map.Map String [Identifier] -> Int
  countTag tag map = maybe 0 length $ Map.lookup tag map

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
    id'      <- compilerUnderlying <$> compilerAsk
    store    <- compilerStore      <$> compilerAsk
    provider <- compilerProvider   <$> compilerAsk

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

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Hakyll's dependency tracking does not work well with resources depending
    -- on metadata (such as tags, categories or dates). To reduces unnecessary
    -- rebuilds because of post content (not metadata) changes, we build the
    -- sidebar separately with custom caching (see 'cacheIfExists') above.
    tags <- buildTags "notes/*" (fromCapture "tags/*.html" . toLower)
    create ["sidebar"] $ do
      let tagAllCtx =
            listField
              "allTags"
              (tagsCtx $ Map.fromList (tagsMap tags))
              (getAllTags tags)

      deps <- makePatternDependency (fromGlob "notes/*")
      compile $ do
        compilerTellDependencies [deps]
        cacheIfExists (tagsMap tags) $ makeItem ""
          >>= loadAndApplyTemplate "templates/sidebar.html" tagAllCtx

    match "index.html" $ do
      route idRoute
      compile $ do
        sidebar <- constField "sidebar" <$> loadBody "sidebar"
        pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" (sidebar <> noteCtx)
          >>= relativizeUrls

    match "notes/*" $ do
      route $ setExtension "html"
      compile $ do
        sidebar <- constField "sidebar" <$> loadBody "sidebar"
        let postTags = tagsField "tags" tags

        pandocCompiler
          >>= loadAndApplyTemplate "templates/note.html" (postTags <> noteCtx)
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/default.html" (sidebar <> noteCtx)
          >>= relativizeUrls

    tagsRules tags $ \tagStr tagsPattern -> do
      route idRoute
      compile $ do
        sidebar <- constField "sidebar" <$> loadBody "sidebar"
        let pageTitle = constField "title" ("Tag: " ++ capitalize tagStr)

        notes <- loadAll tagsPattern >>= recentFirst
        let baseCtx = pageTitle <> sidebar <> defaultContext
            listCtx = pageTitle
                <> listField "notes" noteCtx (return notes)
                <> defaultContext

        notesMeta <- mapM getMetadata (map itemIdentifier notes)
        cacheIfExists notesMeta $ makeItem ""
          >>= loadAndApplyTemplate "templates/notes.html" listCtx
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
