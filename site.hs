{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid ((<>))
import Hakyll

siteTitle  = "Applicative Hazards"
siteDesc   = "Stories on functional programming and the real world"
siteUrl    = "http://clnx.github.io"
siteAuthor = "Călin Ardelean"
siteEmail  = "calinucs@gmail.com"

config :: Configuration
config = defaultConfiguration
  { deployCommand = "rsync -r _site/* ../clnx.github.io/" }

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle       = siteTitle
  , feedDescription = siteDesc
  , feedAuthorName  = siteAuthor
  , feedAuthorEmail = siteEmail
  , feedRoot        = siteUrl
  }

mainCtx :: Context String
mainCtx =
  constField "siteTitle"  siteTitle  <>
  constField "siteUrl"    siteUrl    <>
  constField "siteDesc"   siteDesc   <>
  constField "siteAuthor" siteAuthor <>
  constField "siteEmail"  siteEmail  <>
  defaultContext

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> mainCtx

postTagsCtx :: Tags -> Context String
postTagsCtx tags = tagsField "tags" tags <> postCtx

main :: IO ()
main = hakyllWith config $ do
  match "templates/*" $ compile templateCompiler

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  tags <- buildTags "posts/*" (fromCapture "tags/*.html")
  let cxtWithTags = postTagsCtx tags

  tagsRules tags $ \tag pat -> do
    let title = "Posts tagged \"" <> tag <> "\""
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pat
      let ctx = constField "title" title <>
                listField "posts" postCtx (return posts) <>
                mainCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
      let (postsNew, postsOld) = splitAt 1 posts
      let indexCtx = constField "title" "Home" <>
                     listField "postsNew" postCtx (return postsNew) <>
                     listField "postsOld" postCtx (return postsOld) <>
                     field "tags" (\_ -> renderTagList tags)  <>
                     mainCtx
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html"    cxtWithTags
      >>= loadAndApplyTemplate "templates/default.html" cxtWithTags
      >>= relativizeUrls

  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx <> bodyField "description"
      posts <- fmap (take 100) . recentFirst =<<
        loadAllSnapshots "posts/*" "content"
      renderRss feedConfig feedCtx posts

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx <> bodyField "description"
      posts <- fmap (take 100) . recentFirst =<<
        loadAllSnapshots "posts/*" "content"
      renderAtom feedConfig feedCtx posts
