module Markdown.Post where

import Prelude
import System.Directory
import Data.List
import Data.String

import Markdown.Types



data Post = Post PageName deriving (Show)

findPosts :: IO [Post]
findPosts = do
  directory <- getCurrentDirectory
  let postDirectory = directory ++ "/posts"

  posts <- getDirectoryContents postDirectory

  return $ (map createPost . filter markdownFile) posts
  where
    createPost p = Post $ (splitName . partitionString) p
    splitName (fileName, _) = fileName
    partitionString p = splitAt (index p) p
    index p = findIndexOf $ findIndex (\a -> a == '.') p

    findIndexOf (Just index) = index
    findIndexOf Nothing = 0
                   


    markdownFile = isInfixOf ".md"

                             
  
  

