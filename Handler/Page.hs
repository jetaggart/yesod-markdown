module Handler.Page where

import Import
import Markdown.Post

getPageR :: String -> Handler Html
getPageR postName = do
  (Post _ content) <- liftIO $ findPost postName
  return content
  
