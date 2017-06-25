{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FFI where

import qualified Parser
import Types
import qualified Data.Text as Text

#if (defined (ghcjs_HOST_OS))
import qualified          GHCJS.Foreign as T
import qualified          GHCJS.Types as T
import qualified Data.JSString as T
-- extract the questions from HTML when running in the browser
initialQuestion :: IO QNode
initialQuestion = do
  (questionsString :: String) <- strip . T.unpack <$> js_getQuestions
  let Right initialNode = Parser.parseNodes questionsString
  return initialNode

foreign import javascript unsafe "document.getElementById('questions').value"
  js_getQuestions :: IO T.JSString

#else
import Questions

initialQuestion :: IO QNode
initialQuestion = return initialQNode
  where
  _ = Parser.parseNodes
  _ = strip
#endif

strip :: String -> String
strip = Text.unpack . Text.strip . Text.pack
