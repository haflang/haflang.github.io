--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import Text.Pandoc.Highlighting (Style, haddock, styleToCss)
import Text.Pandoc.Options

--------------------------------------------------------------------------------
siteConfig :: Configuration
siteConfig =
  defaultConfiguration
    { destinationDirectory = "docs"
    }

main :: IO ()
main = hakyllWith siteConfig $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "images/*/*" $ do
    route idRoute
    compile copyFileCompiler

  match "js/*" $ do
    route idRoute
    compile copyFileCompiler

  match "data/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "*.org" $ do
    route $ setExtension "html"
    compile $ do
      ctxt <- getBaseCtxt
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" ctxt
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ do
      ctxt <- getBaseCtxt
      let ctxt' = ctxt `mappend` postCtx
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" ctxt'
        >>= loadAndApplyTemplate "templates/default.html" ctxt'
        >>= relativizeUrls

  match "workshops/*" $ do
    route idRoute
    compile $ do
      ctxt <- getBaseCtxt
      getResourceBody
        >>= applyAsTemplate ctxt
        >>= loadAndApplyTemplate "templates/default.html" ctxt
        >>= relativizeUrls

  match (fromList ["index.html", "people.html", "about.html", "history.html"]) $ do
    route idRoute
    compile $ do
      ctxt <- getBaseCtxt
      getResourceBody
        >>= applyAsTemplate ctxt
        >>= loadAndApplyTemplate "templates/default.html" ctxt
        >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

  create ["css/syntax.css"] $ do
    route idRoute
    compile $ do
      makeItem $ styleToCss pandocCodeStyle

getBaseCtxt :: Compiler (Context String)
getBaseCtxt = do
  posts <- getMatches "posts/*"
  let postIds = [Item id "" | id <- posts]
  workshops <- getMatches "workshops/*"
  let workshopIds = [Item id "" | id <- workshops]
  return $ listField "posts" postCtx (return postIds) `mappend`
           listField "workshops" defaultContext (return workshopIds) `mappend`
           defaultContext

pandocCompiler' :: Compiler (Item String)
pandocCompiler' =
  pandocCompilerWith
    pandocMathReaderOptions
    pandocMathWriterOptions {writerHighlightStyle = Just pandocCodeStyle}

pandocCodeStyle = haddock

pandocMathReaderOptions :: ReaderOptions
pandocMathReaderOptions =
  defaultHakyllReaderOptions
    { readerExtensions = extensionsFromList [Ext_raw_html]
    }

pandocMathWriterOptions :: WriterOptions
pandocMathWriterOptions =
  defaultHakyllWriterOptions
    { -- writerExtensions = extensions
      writerHTMLMathMethod = MathJax ""
    }

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext
