module Main where

import Prelude

import Control.Coroutine as Co
import Control.Monad.Aff (Aff, later')
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Now (now, NOW)
import Control.Monad.Free (Free, liftF, foldFree)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Trans.Class (lift)

import Data.DateTime.Instant (Instant)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.StartApp (HalogenEffects, Html, Update, AppEvent(..), perform, send, runInterpretedApp, runHalogenAff, awaitBody)

newtype User = User
  { name :: String
  , age :: Int
  }

instance showUser ∷ Show User where
  show (User user) =
    "User { name: " <> show user.name
      <> ", age: " <> show user.age
      <> " }"

type Model =
  { count :: Int
  , user :: Maybe User
  , instant :: Maybe Instant
  , log :: String
  }

initialModel :: Model
initialModel =
  { count: 0
  , user: Nothing
  , instant: Nothing
  , log: ""
  }

data Msg
  = Clicked
  | Response User Instant
  | Log String

update
  :: forall eff cmd
   . ( MonadEff (now :: NOW | eff) cmd
     , MonadAsk User cmd
     )
  => Msg
  -> Model
  -> Update Model cmd Msg
update msg model =
  case msg of
    Clicked ->
      let
        model' =
          model { count = model.count + 1 }

        cmd =
          perform do
            user <- ask
            instant <- liftEff now
            pure (Response user instant)

      in
        { model: model', cmd }

    Response user instant ->
      let
        model' =
          model
            { user = Just user
            , instant = Just instant
            }
      in
        { model: model', cmd: mempty }

    Log log ->
      let
        model' =
          model { log = log }

      in
        { model: model', cmd: mempty }

view :: Model -> Html Msg
view model =
  HH.div_
    [ HH.text "Click: "
    , HH.button
        [ HE.onClick \e -> Just Clicked ]
        [ HH.text (show model.count) ]
    , HH.p_
        [ case model.user of
            Nothing -> HH.text "No user"
            Just (User { name, age }) -> HH.text $ name <> " " <> show age
        ]
    , HH.p_
        [ case model.instant of
            Nothing -> HH.text "No instant"
            Just i -> HH.text (show i)
        ]
    , HH.p_
        [ HH.text model.log ]
    ]

---

data MyCmd eff a
  = AffCmd (Aff eff a)
  | AskCmd (User -> a)

newtype AppM eff a = AppM (Free (MyCmd eff) a)

derive newtype instance functorAppM :: Functor (AppM eff)
derive newtype instance applyAppM :: Apply (AppM eff)
derive newtype instance applicativeAppM :: Applicative (AppM eff)
derive newtype instance bindAppM :: Bind (AppM eff)
derive newtype instance monadAppM :: Monad (AppM eff)

instance monadAffAppM :: MonadAff eff (AppM eff) where
  liftAff aff = AppM (liftF (AffCmd aff))

instance monadEffAppM :: MonadEff eff (AppM eff) where
  liftEff eff = liftAff (liftEff eff)

instance monadAskAppM :: MonadAsk User (AppM eff) where
  ask = AppM (liftF (AskCmd id))

interpretAppM :: forall eff. User -> AppM eff ~> Aff eff
interpretAppM userEnv (AppM cmd) =
  foldFree
    case _ of
      AffCmd aff -> aff
      AskCmd run -> pure (run userEnv)
    cmd

main :: Eff (HalogenEffects (now :: NOW, console ∷ CONSOLE)) Unit
main = runHalogenAff do
  let
    user = User
      { name: "Harold"
      , age: 42
      }

  driver <-
    awaitBody >>=
      runInterpretedApp (interpretAppM user)
        { init:
            { model: initialModel
            , cmd: mempty
            }
        , update
        , view
        }

  let
    listener = do
      Transition input model ← Co.await
      case input of
        Clicked → lift $ log "Clicked"
        Response u i → lift $ log $ "Response " <> show u <> " " <> show i
        Log str → lift $ log $ "Log " <> show str
      listener

  driver.subscribe listener

  later' 3000 do
    driver.query (send (Log "Hello from outside"))
