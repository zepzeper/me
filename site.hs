{-# LANGUAGE OverloadedStrings #-}
import           Data.List (intercalate)
import           Hakyll
import           Text.Pandoc.Highlighting (styleToCss, pygments, zenburn)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "about.markdown" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "contact.markdown" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "projects/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/project.html"    projectCtx
            >>= loadAndApplyTemplate "templates/default.html" projectCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archives"            <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["projects.html"] $ do
        route idRoute
        compile $ do
            projects <- recentFirst =<< loadAll "projects/*"
            let projectsCtx =
                    listField "projects" projectCtx (return projects) <>
                    constField "title" "Projects"               <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/projects.html" projectsCtx
                >>= loadAndApplyTemplate "templates/default.html" projectsCtx
                >>= relativizeUrls

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
            renderRss feedConfig postCtx posts

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
            renderAtom feedConfig postCtx posts

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- take 2 <$> (recentFirst =<< loadAll "posts/*")
            projects <- take 2 <$> (recentFirst =<< loadAll "projects/*")
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    listField "projects" projectCtx (return projects) <>
                    constField "title" "Home"                <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


    create ["css/syntax.css"] $ do
        route idRoute
        compile $ makeItem $
            intercalate "\n"
            [ "@import \"syntax-light.css\" all and (prefers-color-scheme: light);",
              "@import \"syntax-dark.css\" all and (prefers-color-scheme: dark);",
              ""
            ]

    create ["css/syntax-light.css"] $ do
        route idRoute
        compile $ makeItem $ styleToCss pygments

    create ["css/syntax-dark.css"] $ do
        route idRoute
        compile $ makeItem $ styleToCss zenburn


config :: Configuration
config = defaultConfiguration
    { previewPort          = 5000 }

--------------------------------------------------------------------------------
feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "Krugten.org"
    , feedDescription  = "Blog about software development, technology, and programming"
    , feedAuthorName   = "Wouter van Krugten"
    , feedAuthorEmail  = "woutervk98@proton.me"
    , feedRoot         = "https://krugten.org"
    }

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

projectCtx :: Context String
projectCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext
