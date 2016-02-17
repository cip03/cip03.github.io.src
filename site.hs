{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid ((<>))
import Hakyll
import Text.Pandoc.Options

siteTitle  = "Applicative Hazards"
siteDesc   = "Stories on functional programming and the real world"
siteUrl    = "http://blog.cip03.xyz"
siteAuthor = "Călin Ardelean"
siteEmail  = "calinucs@gmail.com"

config :: Configuration
config = defaultConfiguration
  { deployCommand = "rsync -r _site/* ../cip03.github.io/" }

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

main :: IO ()
main = hakyllWith config $ do
  match "templates/*" $ compile templateCompiler

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "CNAME" $ do
    route   idRoute
    compile copyFileCompiler

  tags <- buildTags "posts/*" (fromCapture "tags/*.html")
  let postTagsCtx = tagsField "tags" tags <> postCtx

  tagsRules tags $ \tag pat -> do
    let title = "Posts tagged \"" <> tag <> "\""
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pat
      let ctx = constField "title" title
             <> listField "posts" postCtx (return posts)
             <> mainCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
      let (postsNew, postsOld) = splitAt 1 posts
      let indexCtx = constField "title" "Home"
                  <> listField "postsNew" postTagsCtx (return postsNew)
                  <> listField "postsOld" postTagsCtx (return postsOld)
                  <> field "tags" (\_ -> renderTagList tags)
                  <> mainCtx
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompilerWith
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions
          { writerSectionDivs = True
          , writerHTMLMathMethod = MathML Nothing }
          {-, writerHTMLMathMethod = MathJax "" -}
          {-, WebTeX "http://chart.apis.google.com/chart?cht=tx&chl=" -}
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html"    postTagsCtx
      >>= loadAndApplyTemplate "templates/default.html" postTagsCtx
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
