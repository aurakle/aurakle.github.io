--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

--------------------------------------------------------------------------------

config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

main :: IO ()
main = hakyllWith config $ do
    match ("images/**" .||. "js/**") $ do
        route idRoute
        compile copyFileCompiler

    match "css/**" $ do
        route idRoute
        compile compressCssCompiler

    match "*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "projects/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/project.html" projectCtx
            >>= loadAndApplyTemplate "templates/default.html" projectCtx
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["css/default.css"] $ do
        route idRoute
        compile $ do
            cssFiles <- loadAll "css/default/*"
            let styleCtx = listField "items" defaultContext (return cssFiles)

            makeItem ""
                >>= loadAndApplyTemplate "templates/concat.txt" styleCtx

    create ["projects.html"] $ do
        route idRoute
        compile $ do
            projects <- loadAll "projects/*"
            let projectsCtx =
                    listField "projects" projectCtx (return projects) `mappend`
                    constField "title" "Projects"                     `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/projects.html" projectsCtx
                >>= loadAndApplyTemplate "templates/default.html"  projectsCtx
                >>= relativizeUrls

    create ["writings.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let writingsCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Writings"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/writings.html" writingsCtx
                >>= loadAndApplyTemplate "templates/default.html"  writingsCtx
                >>= relativizeUrls

    -- match "index.html" $ do
    --     route idRoute
    --     compile $ do
    --         getResourceBody
    --             >>= applyAsTemplate defaultContext
    --             >>= loadAndApplyTemplate "templates/default.html" defaultContext
    --             >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------

-- myPandocCompiler :: Compiler (Item String)
-- myPandocCompiler =
--     pandocCompilerWithTransformM defaultHakyllReaderOptions myWriter
--     (usingSidenotes)

projectCtx :: Context String
projectCtx =
    defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"   `mappend`
    teaserField "teaser" "content" `mappend`
    defaultContext
