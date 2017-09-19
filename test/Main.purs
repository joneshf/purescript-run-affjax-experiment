module Test.Main where

import Prelude

import Control.Monad.Aff (Canceler, launchAff)
import Control.Monad.Aff.Console as Aff
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, message)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Network.HTTP.Affjax (AJAX)
import Run (runBase)
import Run.Affjax (get, runAjax)
import Simple.JSON (readJSON)

main :: forall e. Eff (ajax :: AJAX, console :: CONSOLE, exception :: EXCEPTION | e) (Canceler (ajax :: AJAX, console :: CONSOLE | e))
main = launchAff do
  res <- runBase $ runAjax $ get "https://httpbin.org/cookies"
  case res of
    Left error -> Aff.error (message error)
    Right { response } -> case runExcept $ readJSON response of
      Left errors -> do
        Aff.error (show errors)
        Aff.error ("Received " <> response)
      Right ({ origin } :: { origin :: String }) -> Aff.log origin
