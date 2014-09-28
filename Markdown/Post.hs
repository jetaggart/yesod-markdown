{-# LANGUAGE OverloadedStrings #-}

module Markdown.Post where

import Prelude
import Import (Html)
import System.Directory
import Data.List
import Text.Blaze.Html (toHtml)

import Markdown.Types

type Content = Html 
data Post = Post PostId Content

findPosts :: IO [PostId]
findPosts = do
  ps <- postFiles
  return $ map extractPostName ps

  where
    extractPostName p = (splitName . partitionString) p
    splitName (fileName, _) = fileName
    partitionString p = splitAt (index p) p
    index p = findIndexOf $ findIndex (\a -> a == '.') p

    findIndexOf (Just index) = index
    findIndexOf Nothing = -1


findPost :: PostId -> IO Post
findPost postId = do
  pd <- postsDir
  let file = pd ++ "/" ++ postId ++ ".md"

  contents <- readFile file
  let html = toHtml contents
  
  return (Post postId html)

markdownFile :: FilePath -> Bool
markdownFile = isInfixOf ".md"

postsDir :: IO FilePath
postsDir = do
  directory <- getCurrentDirectory
  return $ directory ++ "/posts"

postFiles :: IO [String]
postFiles = do
  posts <- getDirectoryContents =<< postsDir
  return $ filter markdownFile posts
