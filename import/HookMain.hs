module Main where

import GitLabHook

app :: App
app = server $ hookServer handlePush

handlePush :: PushEvent -> IO ()
handlePush event = do
  hi
