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
        compile $ getResourceString
            >>= withItemBody (unixFilter "sass" ["-s", "--scss"])
            >>= return . fmap compressCss

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

    -- Render static pages
    --match (fromList ["about.rst", "contact.markdown"]) $ do
    --    route   $ setExtension "html"
    --    compile $ pandocCompiler
    --        >>= loadAndApplyTemplate "templates/default.html" defaultContext
    --        >>= relativizeUrls

    -- Render post archive
    --create ["archive.html"] $ do
    --    route   idRoute
    --    compile $ do
    --        posts <- recentFirst =<< loadAll "posts/*"
    --        let archiveContext =
    --                listField "posts" postContext (return posts) `mappend`
    --                constField "title" "Archives" `mappend`
    --                defaultContext

    --        makeItem ""
    --            >>= loadAndApplyTemplate "templates/archive.html" archiveContext
    --            >>= loadAndApplyTemplate "templates/default.html" archiveContext
    --            >>= relativizeUrls

    -- Read templates
    match ("partials/*" .||. "templates/*") $ compile templateCompiler


-- Contexts
--------------------------------------------------------------------------------
postContext :: Context String
postContext =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext


-- Routes
--------------------------------------------------------------------------------
rootRoute :: Routes
rootRoute = customRoute removeTopDirectory

removeTopDirectory :: Identifier -> FilePath
removeTopDirectory = joinPath . tail . splitPath . toFilePath