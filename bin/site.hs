--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import System.FilePath
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
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postContext
            >>= loadAndApplyTemplate "templates/default.html" postContext
            >>= relativizeUrls

    -- Render homepage
    match "index.html" $ do
        route   idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexContext =
                    listField "posts" postContext (return posts) `mappend`
                    constField "title" "Home" `mappend`
                    defaultContext
            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls

    -- Render RSS feed
    create ["atom.xml"] $ do
        route   idRoute
        compile $ do
            let feedContext = 
                    postContext `mappend`
                    constField "description" "This is the post description"
            posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
            renderAtom feedConfiguration feedContext posts

    -- Read templates
    match ("partials/*" .||. "templates/*") $ compile templateCompiler


-- Compilers
--------------------------------------------------------------------------------
sassCompiler :: Compiler (Item String)
sassCompiler = getResourceString
    >>= withItemBody (unixFilter "sass" ["-s", "--scss"])
    >>= return . fmap compressCss


-- Contexts
--------------------------------------------------------------------------------
postContext :: Context String
postContext = dateField "date" "%B %e, %Y" `mappend` defaultContext


-- Routes
--------------------------------------------------------------------------------
rootRoute :: Routes
rootRoute = customRoute removeTopDirectory

removeTopDirectory :: Identifier -> FilePath
removeTopDirectory = joinPath . tail . splitPath . toFilePath


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