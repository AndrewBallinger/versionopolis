{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude
import GHC.Generics
import Control.Arrow (left, right, first)
import Control.Monad
import System.Environment (getArgs)
import System.Directory (removeDirectoryRecursive, createDirectory, doesDirectoryExist)
import System.File.Tree (getDirectory, copyTo_)
import Data.HashMap.Strict (HashMap, keys, (!))
import Data.Text (Text)
import Data.Yaml as Y

import Text.XML.HXT.Core 

data Version = Version (HashMap String String) deriving (Show, Generic)
data Configuration = Configuration [Version] deriving (Show, Generic)

instance FromJSON Configuration
instance FromJSON Version
               
main :: IO ()
main = do
  args <- getArgs
  if length args /= 2 then
     putStrLn "Usage: versionopolis <configuration.yaml> <target.html>"
  else do
     let [configArg, htmlArg] = args
     config <- parseConfiguration (configArg)
     case config of
       Left e -> print e
       Right c -> do
         existence <- doesDirectoryExist "build"
         Control.Monad.when existence $ removeDirectoryRecursive "build"
         createDirectory "build"
         makeVersionDirectories c >> do
           putStrLn ("Building html files")
           doHtmlFile c htmlArg
           putStrLn ("Building css files")
           doStatic c
           return ()

parseConfiguration :: String -> IO (Either ParseException [HashMap String String])
parseConfiguration = Y.decodeFileEither

makeVersionDirectories :: [HashMap String String] -> IO ()
makeVersionDirectories config = sequence_ (createDirectory <$> ("build/" ++) <$> (! "key") <$> config)

doStatic :: [HashMap String String] -> IO ()
doStatic configuration = do
  existence <- doesDirectoryExist "static"
  Control.Monad.when existence $ sequence_ $ copyStaticForBuild <$> ((! "key") <$> configuration)
  return ()

copyStaticForBuild :: String -> IO ()
copyStaticForBuild build = do
  dir <- getDirectory "static"
  copyTo_ ("build/" ++ build ++ "/static") dir
  return ()

doHtmlFile :: [HashMap String String] -> String -> IO ()
doHtmlFile configuration htmlFile =
  let doc = readDocument [withValidate no, withWarnings no, withTrace 0, withParseHTML yes] htmlFile in
  do
    sequence_ (runX <$> (constructVersion doc htmlFile <$> configuration))
    return ()

constructVersion :: IOSArrow XmlTree XmlTree -> String -> HashMap String String -> IOSArrow XmlTree XmlTree
constructVersion doc filename version = traceMsg 1 ("Trying to process " ++ version ! "key")
  >>> foldl (>>>) doc (processTopDown <$> changesForVersion version)
  >>> writeDocument [withOutputHTML] ("build/" ++ (version ! "key") ++ filename)
  >>> traceMsg 1 ("Wrote " ++ (version ! "key"))

updateVersionWithKey :: HashMap String String -> String -> IOSArrow XmlTree XmlTree
updateVersionWithKey version key = traceMsg 1 ("Looking at " ++ key)
  >>> replaceWithText (version ! key)
  `Text.XML.HXT.Core.when` hasId key
  >>> traceMsg 1 ("Made a change for " ++ key)

changesForVersion :: HashMap String String -> [IOSArrow XmlTree XmlTree]
changesForVersion version = updateVersionWithKey version <$> keys version

hasId :: ArrowXml a => String -> a XmlTree XmlTree
hasId key =  hasAttr "id" >>> hasAttrValue "id" (== key)

replaceWithText :: ArrowXml a => String -> a XmlTree XmlTree
replaceWithText text = replaceChildren (txt text)
