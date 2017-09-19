module Run.Affjax
  ( AJAX
  , _ajax
  , get
  , post
  , post'
  , post_
  , post_'
  , put
  , put'
  , put_
  , put_'
  , delete
  , delete_
  , patch
  , patch'
  , patch_
  , patch_'
  , runAjax
  , runPure
  )
  where

import Control.Applicative (pure)
import Control.Bind ((<=<))
import Control.Category (id)
import Control.Monad.Eff.Exception (Error)
import Control.Semigroupoid ((<<<))
import Data.Either (Either(..), either)
import Data.Function (($))
import Data.Functor.Variant (FProxy, on)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(SProxy))
import Data.Unit (Unit)
import Network.HTTP.Affjax (AffjaxResponse, URL, defaultRequest)
import Network.HTTP.Affjax as Affjax
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.AffjaxF (AffjaxFP(..))
import Network.HTTP.AffjaxF as AffjaxF
import Run (BaseAff, Run, interpret, liftEffect, peel, send)

type AJAX req res = FProxy (AffjaxFP req res)

_ajax :: SProxy "ajax"
_ajax = SProxy

delete
  :: forall r req res
  . Requestable req
  => Respondable res
  => URL
  -> Run (ajax :: AJAX Unit res | r) (Either Error (AffjaxResponse res))
delete url = liftEffect _ajax $ AffjaxF.delete url

delete_
  :: forall r req res
  . Requestable req
  => Respondable res
  => URL
  -> Run (ajax :: AJAX Unit Unit | r) (Either Error (AffjaxResponse Unit))
delete_ url = liftEffect _ajax $ AffjaxF.delete_ url

get
  :: forall r res
  . Respondable res
  => URL
  -> Run (ajax :: AJAX Unit res | r) (Either Error (AffjaxResponse res))
get url = liftEffect _ajax $ AffjaxF.get url

affjaxFPatch
  :: forall req res
  . URL
  -> Maybe req
  -> AffjaxFP req res (Either Error (AffjaxResponse res))
affjaxFPatch url req =
  AffjaxFP (defaultRequest { content = req, method = Left PATCH, url = url }) id

patch
  :: forall r req res
  . Requestable req
  => Respondable res
  => URL
  -> req
  -> Run (ajax :: AJAX req res | r) (Either Error (AffjaxResponse res))
patch url req =
  liftEffect _ajax $ affjaxFPatch url (Just req)

patch'
  :: forall r req res
  . Requestable req
  => Respondable res
  => URL
  -> Maybe req
  -> Run (ajax :: AJAX req res | r) (Either Error (AffjaxResponse res))
patch' url req = liftEffect _ajax $ affjaxFPatch url req

patch_
  :: forall r req res
  . Requestable req
  => Respondable res
  => URL
  -> req
  -> Run (ajax :: AJAX req Unit | r) (Either Error (AffjaxResponse Unit))
patch_ url req = liftEffect _ajax $ affjaxFPatch url (Just req)

patch_'
  :: forall r req res
  . Requestable req
  => Respondable res
  => URL
  -> Maybe req
  -> Run (ajax :: AJAX req Unit | r) (Either Error (AffjaxResponse Unit))
patch_' url req = liftEffect _ajax $ affjaxFPatch url req

post
  :: forall r req res
  . Requestable req
  => Respondable res
  => URL
  -> req
  -> Run (ajax :: AJAX req res | r) (Either Error (AffjaxResponse res))
post url req = liftEffect _ajax $ AffjaxF.post url req

post'
  :: forall r req res
  . Requestable req
  => Respondable res
  => URL
  -> Maybe req
  -> Run (ajax :: AJAX req res | r) (Either Error (AffjaxResponse res))
post' url req = liftEffect _ajax $ AffjaxF.post' url req

post_
  :: forall r req res
  . Requestable req
  => Respondable res
  => URL
  -> req
  -> Run (ajax :: AJAX req Unit | r) (Either Error (AffjaxResponse Unit))
post_ url req = liftEffect _ajax $ AffjaxF.post_ url req

post_'
  :: forall r req res
  . Requestable req
  => Respondable res
  => URL
  -> Maybe req
  -> Run (ajax :: AJAX req Unit | r) (Either Error (AffjaxResponse Unit))
post_' url req = liftEffect _ajax $ AffjaxF.post_' url req

put
  :: forall r req res
  . Requestable req
  => Respondable res
  => URL
  -> req
  -> Run (ajax :: AJAX req res | r) (Either Error (AffjaxResponse res))
put url req = liftEffect _ajax $ AffjaxF.put url req

put'
  :: forall r req res
  . Requestable req
  => Respondable res
  => URL
  -> Maybe req
  -> Run (ajax :: AJAX req res | r) (Either Error (AffjaxResponse res))
put' url req = liftEffect _ajax $ AffjaxF.put' url req

put_
  :: forall r req res
  . Requestable req
  => Respondable res
  => URL
  -> req
  -> Run (ajax :: AJAX req Unit | r) (Either Error (AffjaxResponse Unit))
put_ url req = liftEffect _ajax $ AffjaxF.put_ url req

put_'
  :: forall r req res
  . Requestable req
  => Respondable res
  => URL
  -> Maybe req
  -> Run (ajax :: AJAX req Unit | r) (Either Error (AffjaxResponse Unit))
put_' url req = liftEffect _ajax $ AffjaxF.put_' url req

runAjax
  :: forall a e r req res
  . Requestable req
  => Respondable res
  => Run (ajax :: AJAX req res | r) a
  -> Run (base :: BaseAff (ajax :: Affjax.AJAX | e) | r) a
runAjax = interpret _ajax AffjaxF.eval

runPure
  :: forall a r req res
  . ( AffjaxFP req res (Run (ajax :: AJAX req res | r) a)
    -> Run (ajax :: AJAX req res | r) a
    )
  -> Run (ajax :: AJAX req res | r) a
  -> Run r a
runPure f =
  either (on _ajax (runPure f <<< f) (runPure f <=< send)) pure <<< peel
