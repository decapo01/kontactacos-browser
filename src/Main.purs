module Main where

import Prelude

import Components (contactTableComponent)
import Control.Monad.State (state)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen (HalogenQ(..), liftAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (enabled)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA (label)
import Halogen.VDom.Driver (runUI)
import Repo as Repo

type State = { enabled :: Boolean , items :: Array String }

data Action = Toggle

component :: forall q i o. H.Component HH.HTML q i o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { enabled: false, items: [] }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    label = if state.enabled then "On" else "Off"
  in
    HH.div
      []
      [ HH.ul_ $ (\i -> HH.li_ [HH.text i]) <$> state.items
      , HH.button
        [ HP.title label
        , HE.onClick \_ -> Just Toggle
        ]
        [ HH.text label ] 
      ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Toggle -> do
    itemsEither <- H.liftAff Repo.findAll
    case itemsEither of
      Left e -> H.modify_ \st -> st { enabled = not st.enabled, items = [e] }
      Right i -> H.modify_ \st -> st { enabled = not st.enabled, items = map (\item -> item.name ) i }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI contactTableComponent unit body
