{-# LANGUAGE OverloadedStrings #-}

-- | XXX comment me

module Hakyll.Web.Course
       ( standardRules, createIndex
       )
       where

import           Data.Monoid ((<>))
import           Hakyll

sectionCtx sect = constField "secName" sect <> defaultContext

navCompiler :: String -> Compiler (Item String)
navCompiler sect = do
  sectItem <- load (fromFilePath $ sect ++ ".md")
  loadAndApplyTemplate "templates/nav.html" (sectionCtx sect) sectItem

sectionCompiler :: String -> Compiler (Item String)
sectionCompiler sect = do
  sectionTemplate <- loadBody "templates/section.html"
  sectionContent  <- load (fromFilePath $ sect ++ ".md")
  applyTemplate sectionTemplate (sectionCtx sect) sectionContent

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

    match "*.md" $ compile pandocCompiler

    matchAny staticContent $ do
      route idRoute
      compile copyFileCompiler

  where
    matchAny :: [Pattern] -> Rules () -> Rules ()
    matchAny pats rules = mapM_ (`match` rules) pats
