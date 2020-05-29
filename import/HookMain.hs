module Main where

import Data.Proxy

import GitLabHook
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.Server

main :: IO ()
main = run 4554 app

app :: Application
app = serve (Proxy :: Proxy Hook) $ hookServer handleEvent

handleEvent :: Event -> IO ()
handleEvent (PushEvent' e) = handlePush e
handleEvent _ = return ()

handlePush :: PushEvent -> IO ()
handlePush event = do
  undefined
