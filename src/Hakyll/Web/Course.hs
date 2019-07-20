{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | XXX comment me

module Hakyll.Web.Course where

import           Control.Arrow   ((&&&))
import           Control.Monad
import           Data.List       (sortBy)
import           Data.List.Split (splitOn)
import           Data.Maybe      (fromMaybe)
import           Data.Monoid     ((<>))
import           Data.Ord        (comparing)
import           Hakyll

sectionCtx sect = constField "secName" sect <> defaultContext

navCompiler :: String -> Compiler (Item String)
navCompiler sect = do
  sectItem <- load (fromFilePath $ sect ++ ".md")
  loadAndApplyTemplate "templates/nav.html" (sectionCtx sect) sectItem

sectionCompiler :: String -> Compiler (Item String)
sectionCompiler sect = do
  case splitOn "-" sect of
    [sectName, "table"] -> do
      sectionTemplate <- loadBody (fromFilePath $ "templates/" ++ sectName ++ ".html")
      rows <- loadAll (fromGlob $ sectName ++ "/*")
      sortedRows <- sortByM sortField rows
      let tableSectionCtx =
            listField sectName defaultContext (return sortedRows) <> sectionCtx sectName
      sectionContent <- load (fromFilePath $ sectName ++ ".md")
      applyTemplate sectionTemplate tableSectionCtx sectionContent

    _ -> do
      sectionTemplate <- loadBody "templates/section.html"
      sectionContent  <- load (fromFilePath $ sect ++ ".md")
      applyTemplate sectionTemplate (sectionCtx sect) sectionContent

  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f = fmap (map fst . sortBy (comparing snd))
              . mapM (\a -> (a,) <$> f a)

    sortField :: Item String -> Compiler String
    sortField item = do
      md <- getMetadata (itemIdentifier item)
      return $ fromMaybe "" (lookupString "sort" md)

indexCompiler :: [String] -> Compiler (Item String)
indexCompiler sectionNames = do
  index <- load "index.md"

  page <- loadAndApplyTemplate "templates/index.html" ctx (index :: Item String)
  makeItem $ itemBody page
  where
    ctx = listField "nav" defaultContext (mapM navCompiler sectionNames)
       <> listField "sections" defaultContext (mapM sectionCompiler sectionNames)
       <> defaultContext

-- | XXX comment me
createIndex :: [String] -> Rules ()
createIndex sections = create ["index.html"] $ do
  route idRoute
  compile $ indexCompiler sections

-- | XXX comment me
standardRules :: [Pattern] -> Rules ()
standardRules staticContent = do
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    match "templates/*" $ compile templateCompiler

    match "*.md"    $ compile pandocCompiler
    match "**/*.md" $ compile pandocCompiler

    matchAny staticContent $ do
      route idRoute
      compile copyFileCompiler

  where
    matchAny :: [Pattern] -> Rules () -> Rules ()
    matchAny pats rules = mapM_ (`match` rules) pats
