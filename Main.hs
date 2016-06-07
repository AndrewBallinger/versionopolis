{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude
import GHC.Generics
import Control.Monad (when)
import System.Environment (getArgs)
import System.Directory (removeDirectoryRecursive, createDirectory, doesDirectoryExist)
import System.File.Tree (getDirectory, copyTo_)
import Data.HashMap.Strict (HashMap, keys, (!))
import Data.Yaml as Y

import Text.XML.HXT.Core 

data Version = Version (HashMap String String) deriving (Show, Generic)
data Configuration = Configuration [Version] deriving (Show, Generic)

instance FromJSON Configuration
instance FromJSON Version
               
main :: IO ()
main = do
  args <- getArgs
  if length args /= 1 then
     putStrLn "Usage: versionopolis <configuration.yaml>"
  else do
     let [configArg] = args
     config <- parseConfiguration configArg
     case config of
       Left e -> print e
       Right c -> do
         existence <- doesDirectoryExist "build"
         Control.Monad.when existence $ removeDirectoryRecursive "build"
         createDirectory "build"
         do
           putStrLn "Building HTML files"
           doHtmlFile c
           putStrLn "Building static files"
           doStatic 
           return ()

parseConfiguration :: String -> IO (Either ParseException [HashMap String String])
parseConfiguration = Y.decodeFileEither

doStatic :: IO ()
doStatic = do
  existence <- doesDirectoryExist "static"
  Control.Monad.when existence copyStaticForBuild
  return ()

copyStaticForBuild :: IO ()
copyStaticForBuild = do
  dir <- getDirectory "static"
  copyTo_ "build/static" dir
  return ()

doHtmlFile :: [HashMap String String] -> IO ()
doHtmlFile configuration =
  let doc = readDocument [withValidate no, withWarnings no, withTrace 0, withParseHTML yes] "index.html" in
  sequence_ (runX <$> (constructVersion doc <$> configuration)) 

constructVersion :: IOSArrow XmlTree XmlTree -> HashMap String String -> IOSArrow XmlTree XmlTree
constructVersion doc version = traceMsg 1 ("Trying to process " ++ version ! "key")
  >>> foldl (>>>) doc (processTopDown <$> changesForVersion version)
  >>> writeDocument [withOutputHTML] ("build/" ++ (version ! "key"))
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
