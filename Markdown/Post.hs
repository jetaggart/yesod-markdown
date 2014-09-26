module Markdown.Post where

import Prelude
import Markdown.Types

data Post = Post PageName

findPosts :: IO [Post]
findPosts = return [(Post "name")]
