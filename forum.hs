{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import GHC.Generics
import Data.Aeson
import Data.List
import Data.Maybe
import Data.IORef
import Control.Monad.IO.Class
import Network.Wai.Handler.Warp
import Servant

type CommentId = Int

data Comment = Comment {
    commentId :: CommentId,
    commentText :: String,
    commentUpvotes :: Int
} deriving (Generic, Show)

instance ToJSON Comment

type CommentList = [Comment]

newtype AppState = AppState {
    comments :: IORef CommentList
}

type API = "comments" :> Get '[JSON] CommentList
       :<|> "comments" :> ReqBody '[JSON] String :> Post '[JSON] Comment
       :<|> "comments" :> Capture "id" CommentId :> "upvote" :> PostNoContent '[JSON] NoContent

api :: Proxy API
api = Proxy

getComments :: AppState -> Handler CommentList
getComments state = liftIO $ readIORef (comments state)

addComment :: AppState -> String -> Handler Comment
addComment state text = do
    commentList <- liftIO $ readIORef (comments state)
    let newCommentId = if null commentList then 1 else commentId (last commentList) + 1
        newComment = Comment newCommentId text 0
        newCommentList = newComment : commentList
    liftIO $ writeIORef (comments state) newCommentList
    return newComment

upvoteComment :: AppState -> CommentId -> Handler NoContent
upvoteComment state commentId = do
    commentList <- liftIO $ readIORef (comments state)
    let updatedCommentList = map (\comment -> if commentId == commentId comment then comment { commentUpvotes = commentUpvotes comment + 1 } else comment) commentList
    liftIO $ writeIORef (comments state) updatedCommentList
    return NoContent

app :: AppState -> Application
app state = serve api $
    getComments state :<|>
    addComment state :<|>
    upvoteComment state

main :: IO ()
main = do
    let initialState = AppState $ newIORef []
        settings = setPort 8080 $ setHost "localhost" defaultSettings
    runSettings settings $ app initialState