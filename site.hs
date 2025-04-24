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

    -- build up tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx =
                    constField "title" title                                `mappend`
                    listField "posts" (postCtxWithTags tags) (return posts) `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html"     ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            let ctx = postCtxWithTags tags

            pandocCompiler
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/post.html"    ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
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
                    listField "posts" (postCtxWithTags tags) (return posts) `mappend`
                    constField "title" "Writings"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/writings.html" writingsCtx
                >>= loadAndApplyTemplate "templates/default.html"  writingsCtx
                >>= relativizeUrls

    -- GH Pages stupid
    create [".nojekyll"] $ do
        route idRoute
        compile $ do
            makeItem ""
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------

projectCtx :: Context String
projectCtx =
    defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"   `mappend`
    teaserField "teaser" "content" `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags =
    tagsField "processedTags" tags `mappend`
    postCtx
