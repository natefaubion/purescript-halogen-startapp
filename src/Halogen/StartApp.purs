module Halogen.StartApp
  ( Html
  , Update
  , App
  , AppQuery
  , AppEvent(..)
  , AppAff
  , send
  , snapshot
  , restore
  , simpleApp
  , runSimpleApp
  , interpretedApp
  , runInterpretedApp
  , module Halogen.Aff
  , module Halogen.StartApp.Cmd
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff as Aff
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Eff.Ref (Ref)
import Control.Monad.Eff.Ref as Ref

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isNothing)

import DOM.HTML.Types (HTMLElement)

import Halogen as H
import Halogen.Aff (awaitBody, selectElement, runHalogenAff, HalogenIO, HalogenEffects)
import Halogen.HTML.Core as HC
import Halogen.Query.EventSource (EventSource(..), SubscribeStatus(..), produce)
import Halogen.VDom.Driver (runUI)

import Halogen.StartApp.Cmd (Cmd, perform)
import Halogen.StartApp.Cmd as Cmd
import Halogen.StartApp.Internal.Raf (requestAnimationFrame)

type Html = HC.HTML Void

type Update model cmd msg =
  { model :: model
  , cmd :: Cmd cmd msg
  }

type App model cmd msg =
  { init :: Update model cmd msg
  , update :: msg -> model -> Update model cmd msg
  , view :: model -> Html msg
  }

type AppAff eff = Aff (HalogenEffects eff)

type AppDSL model msg eff =
  H.ComponentDSL (AppState model) (AppQuery model msg) (AppEvent model msg) (AppAff eff)

type AppHTML model msg =
  H.ComponentHTML (AppQuery model msg)

type AppState model =
  { model :: model
  }

data AppQuery model msg a
  = Initialize a
  | Snapshot (model -> a)
  | Restore model a
  | Send msg a
  | Flush a

data AppEvent model msg
  = Transition msg model

send :: forall model msg. msg -> AppQuery model msg Unit
send = H.action <<< Send

snapshot :: forall model msg. AppQuery model msg model
snapshot = H.request Snapshot

restore :: forall model msg. model -> AppQuery model msg Unit
restore = H.action <<< Restore

runSimpleApp
  :: forall model msg eff
   . App model (AppAff eff) msg
  -> HTMLElement
  -> AppAff eff (HalogenIO (AppQuery model msg) (AppEvent model msg) (AppAff eff))
runSimpleApp = runInterpretedApp id

runInterpretedApp
  :: forall model cmd msg eff
   . (cmd ~> AppAff eff)
  -> App model cmd msg
  -> HTMLElement
  -> AppAff eff (HalogenIO (AppQuery model msg) (AppEvent model msg) (AppAff eff))
runInterpretedApp interpret app el = do
  ref <- liftEff (Ref.newRef Nothing)
  runUI (interpretedApp interpret ref app) unit el

simpleApp
  :: forall model msg eff
   . Ref (Maybe model)
  -> App model (AppAff eff) msg
  -> H.Component HC.HTML (AppQuery model msg) Unit (AppEvent model msg) (AppAff eff)
simpleApp = interpretedApp id

interpretedApp
  :: forall model cmd msg eff
   . (cmd ~> AppAff eff)
  -> Ref (Maybe model)
  -> App model cmd msg
  -> H.Component HC.HTML (AppQuery model msg) Unit (AppEvent model msg) (AppAff eff)
interpretedApp interpret ref { init, update, view } =
  H.lifecycleComponent
    { render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    , receiver: const Nothing
    , initialState: \_ -> { model: init.model }
    }
  where
    render :: AppState model -> AppHTML model msg
    render state =
      H.action <<< Send <$> view state.model

    eval :: AppQuery model msg ~> AppDSL model msg eff
    eval = case _ of
      Initialize next ->
        execCmd init.cmd $> next
      Snapshot reply ->
        reply <$> latestModel
      Restore model next -> do
        liftEff (Ref.writeRef ref Nothing)
        H.modify _ { model = model } $> next
      Send msg next -> do
        model <- latestModel
        let
          res = update msg model
        H.raise (Transition msg res.model)
        queueRender res.model
        execCmd res.cmd
        pure next
      Flush next -> do
        mb <- liftEff (Ref.readRef ref <* Ref.writeRef ref Nothing)
        for_ mb \model ->
          H.modify _ { model = model }
        pure next

    queueRender model = do
      mb <- liftEff (Ref.readRef ref <* Ref.writeRef ref (Just model))
      when (isNothing mb) do
        launchProducer \emit ->
          requestAnimationFrame do
            emit (Left (Flush Done))

    execCmd cmd =
      for_ (Cmd.run (Cmd.hoist interpret cmd)) \cmd' ->
        launchProducer \emit -> void do
          Aff.runAff throwException (const (pure unit)) do
            res <- cmd'
            liftEff do
              emit (Left (Send res Done))

    latestModel = do
      mb <- liftEff (Ref.readRef ref)
      case mb of
        Nothing -> H.gets _.model
        Just model -> pure model

    launchProducer fn =
      H.subscribe
        (EventSource
          (pure
            { producer: produce fn
            , done: pure unit
            }))
