--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.List              (isSuffixOf)
import Data.Monoid            (mappend)
import System.FilePath        (joinPath, splitPath)
import System.FilePath.Posix  (takeBaseName, (</>))

import Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    -- Copy images
    match "resources/images/*" $ do
        route   rootRoute
        compile copyFileCompiler

    -- Preprocess Sass and compress CSS
    match "resources/styles/all.scss" $ do
        route   $ rootRoute `composeRoutes` setExtension "css"
        compile $ sassCompiler

    -- Render posts
    match "posts/*" $ do
        route   $ rootRoute `composeRoutes` postRoute
        compile $ pandocCompiler
              >>= saveSnapshot         "content"
              >>= loadAndApplyTemplate "templates/post.html"    postContext
              >>= loadAndApplyTemplate "templates/default.html" postContext
              >>= relativizeUrls
              >>= removeIndexFromUrls

    -- Render homepage
    match "index.html" $ do
        route   idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            let indexContext = listField "posts" postContext (return posts)
                     `mappend` defaultContext
            getResourceBody
                >>= applyAsTemplate                               indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls
                >>= removeIndexFromUrls

    -- Render Atom feed
    create ["atom.xml"] $ do
        route   idRoute
        compile $ do
            let feedContext = postContext `mappend` bodyField "description"
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            renderAtom feedConfiguration feedContext posts
                >>= removeIndexFromUrls

    -- Read templates
    match ("partials/*" .||. "templates/*") $ compile templateCompiler

--------------------------------------------------------------------------------
-- Compilers
--------------------------------------------------------------------------------
sassCompiler :: Compiler (Item String)
sassCompiler = getResourceString
           >>= withItemBody (unixFilter "sass" options)
           >>= return . fmap compressCss
  where
    options = ["-s", "--scss", "-t", "compressed"]

removeIndexFromUrls :: Item String -> Compiler (Item String)
removeIndexFromUrls = return . fmap (withUrls clean)
  where
    index = "index.html"
    clean url | index `isSuffixOf` url = take (length url - length index) url
              | otherwise              = url

--------------------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------------------
postContext :: Context String
postContext = dateField "date" "%B %e, %Y" `mappend` defaultContext

--------------------------------------------------------------------------------
-- Routes
--------------------------------------------------------------------------------
rootRoute :: Routes
rootRoute = customRoute removeTopDirectory
  where
    removeTopDirectory = joinPath . tail . splitPath . toFilePath

postRoute :: Routes
postRoute = customRoute removeDateAndFolderize
  where
    removeDateAndFolderize id = drop 11 (takeBaseName (toFilePath id))
                            </> "index.html"

--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "haxorize.com"
    , feedDescription = "The personal blog of Nick Bourgeois"
    , feedAuthorName  = "Nick Bourgeois"
    , feedAuthorEmail = "nick@haxorize.com"
    , feedRoot        = "http://haxorize.com"
    }
