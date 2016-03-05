{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid ((<>))
import Hakyll
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Parsec
import Control.Monad (void)

siteTitle  = "Functional Transfixity"
siteDesc   = "Stories on types, functional programming and the real world"
siteUrl    = "http://blog.mmn80.xyz"
siteAuthor = "Călin Ardelean"
siteEmail  = "mmn80cpu@gmail.com"

config :: Configuration
config = defaultConfiguration
  { deployCommand = "rsync -r _site/* ../mmn80.github.io/" }

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle       = siteTitle
  , feedDescription = siteDesc
  , feedAuthorName  = siteAuthor
  , feedAuthorEmail = siteEmail
  , feedRoot        = siteUrl
  }

mainCtx :: Context String
mainCtx = constField "siteTitle"  siteTitle
       <> constField "siteUrl"    siteUrl
       <> constField "siteDesc"   siteDesc
       <> constField "siteAuthor" siteAuthor
       <> constField "siteEmail"  siteEmail
       <> defaultContext

postCtx :: Context String
postCtx = dateField "date" "%F"
       <> dateField "dateLong" "%B %e, %Y"
       <> mainCtx

elementParser :: Int -> Parsec String u String
elementParser lvl = do
  try $ count lvl spc
  let ind = replicate lvl ' '
  textParser ind <|> do
    tag <- many1 letter
    attrs <- manyTill attrParser (try eof <|> void newline)
    kids <- many $ elementParser $ lvl + 2
    return $ ind ++ "<" ++ tag ++ (if null attrs then "" else " ") ++ unwords attrs ++
      (if all null kids then " />"
       else ">\n" ++ unlines kids ++ ind ++ "</" ++ tag ++ ">")

textParser :: String -> Parsec String u String
textParser ind = do
  try $ char '@'
  txt <- manyTill anyChar newline
  return $ ind ++ txt

attrParser :: Parsec String u String
attrParser = do
  spc
  attr <- many1 $ letter <|> char '-'
  char '='
  val <- try (sep >> manyTill anyChar sep) <|> many (noneOf [' ', '\n'])
  return $ attr ++ "=" ++ case val of
    '"':_ -> val
    _     -> '"':val ++ "\""

spc :: Parsec String u Char
spc = char ' '

sep :: Parsec String u Char
sep = char '"'

svgLight2Xml :: String -> String
svgLight2Xml str = either show id $ parse (elementParser 0) "" str

pandocFilter :: Pandoc -> Pandoc
pandocFilter (Pandoc meta bs) = Pandoc meta (doBlock <$> bs)
  where doBlock (CodeBlock (aId, aCls, aAttr) str) =
          if "svg-light" `elem` aCls
          then RawBlock "html" $ svgLight2Xml str
          else CodeBlock (aId, aCls, aAttr) str
        doBlock b = b

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

  let comPandoc = pandocCompilerWithTransform
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions
          { writerSectionDivs = True
          , writerHTMLMathMethod = MathML Nothing }
          {-, writerHTMLMathMethod = MathJax "" -}
          {-, WebTeX "http://chart.apis.google.com/chart?cht=tx&chl=" -}
        pandocFilter

  let comPost it =
            loadAndApplyTemplate "templates/post.html"    postTagsCtx it
        >>= loadAndApplyTemplate "templates/default.html" postTagsCtx
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ comPandoc >>= saveSnapshot "content" >>= comPost

  match "drafts/*" $ do
    route $ setExtension "html"
    compile $ comPandoc >>= comPost

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
