--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
import           Data.Monoid (mappend)
import           Hakyll
import Data.Char (toUpper)
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Maybe (fromJust)
import qualified Data.Map as Map

captialize :: String -> String
captialize "" = ""
capitalize (x:xs) = toUpper x : xs

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
       <> titleField "title"
       <> field "count" (\(itemBody -> (t,_)) -> pure $ show $ countTag t tmap)
       <> missingField
 where
  getPosts :: Item (String,[Identifier]) -> Compiler [Item String]
  getPosts (itemBody -> (_,is)) = mapM load is

  countTag :: String -> Map.Map String [Identifier] -> Int
  countTag tag map = maybe 0 length $ Map.lookup tag map

--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- One major limitations wrt. heavy relies on tags is that Hakyll doesn't
    -- support dependencies on metadata of a file, rules always depend on the
    -- entire file content. Therefore, if a post is changed, we need to rebuild
    -- the sidebar and all tag patches (so basically the whole site).
    --
    -- See: https://github.com/jaspervdj/hakyll/issues/383#issuecomment-150836917
    tags <- buildTags "notes/*" (fromCapture "tags/*.html")
    let tagAllCtx = listField "allTags" (tagsCtx $ Map.fromList (tagsMap tags)) (getAllTags tags)

    match "notes/*" $ do
      route $ setExtension "html"
      compile $ do
        pandocCompiler
          >>= loadAndApplyTemplate "templates/note.html" ((tagsField "tags" tags) <> noteCtx)
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/default.html" (tagAllCtx <> noteCtx)
          >>= relativizeUrls

    tagsRules tags $ \tagStr tagsPattern -> do
      route idRoute
      compile $ do
        notes <- loadAll tagsPattern >>= recentFirst
        let notesCtx =
              constField "title" ("Tag: " ++ capitalize tagStr)
                <> constField "tag" (capitalize tagStr)
                <> listField "notes" noteCtx (return notes)
                <> defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/notes.html" notesCtx
          >>= loadAndApplyTemplate "templates/default.html" (tagAllCtx <> notesCtx)
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

noteCtx :: Context String
noteCtx =
    dateField "date" "%B %e, %Y"
      -- See https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/time#valid_datetime_values
      <> dateField "datetime" "%F"
      <> defaultContext
