--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>), mconcat)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" (postCtxWithTags tags) (return posts)
                 <> constField "title" "Archives"
                 <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" (postCtxWithTags tags) (return posts)
                 <> constField "title" "Home"
                 <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx =
                    constField "title" title
                 <> listField "posts" (postCtxWithTags tags) (return posts)
                 <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    createFeed "atom.xml" renderAtom
    createFeed "rss.xml"  renderRss

--------------------------------------------------------------------------------
createFeed :: Identifier ->
              (FeedConfiguration -> Context String -> [Item String] -> Compiler (Item String))
              -> Rules ()
createFeed path renderXXX = create [ path ] $ do
   route idRoute
   compile $ do
        let feedCtx = bodyField "description" <> defaultContext
        posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
        renderXXX myFeedConfig feedCtx posts

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = mconcat
    [ dateField "date" "%Y-%m-%d"
    , tagsField "tags" tags
    , defaultContext
    ]

myFeedConfig :: FeedConfiguration
myFeedConfig = FeedConfiguration
    { feedTitle       = "Blog Title"
    , feedDescription = "Blog Description"
    , feedAuthorName  = "John Doe"
    , feedAuthorEmail = "johndoe@gmail.com"
    , feedRoot        = "http://johndoe.github.io/"
    }
