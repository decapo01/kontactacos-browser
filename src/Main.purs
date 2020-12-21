module Main where

import Prelude

import AppM (Env, runAppM)
import Components (contactTableComponent)
import Control.Monad.State (state)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Console (log)
import Halogen (HalogenM, liftEffect)
import Halogen (HalogenQ(..), liftAff, liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (enabled)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA (label)
import Halogen.VDom.Driver (runUI)
import Navigate (navigate)
import Repo as Repo
import RouteItems (Route, routeCodec)
import Routes (Query(..))
import Routes as R
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)


type State = { enabled :: Boolean , items :: Array String }

data Action = Toggle

-- component :: forall q i o. H.Component HH.HTML q i o Aff
-- component =
--   H.mkComponent
--     { initialState
--     , render
--     , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
--     }

initialState :: forall i. i -> State
initialState _ = { enabled: false, items: [] }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    label = if state.enabled then "On" else "Off"
  in
    HH.div_
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


-- derive instance affNav :: Nav m 

autoLog :: forall a. (Show a) => a -> Effect Unit
autoLog i = log $ show i


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let env = {} :: Env
  let rootComponent = H.hoist (runAppM env) R.component 
  halogenIO <- runUI rootComponent {} body
  -- vliftEffect $ log $ show $ parse routeCodec
  -- case parse routeCodec of
  --   Right myRoute -> liftEffect $ log $ show myRoute
  --   Left _        -> liftEffect $ log "route error"
  void $ liftEffect $ matchesWith (parse routeCodec) \old new -> do
    log $ "new : " <> (show  new)
    log $ "old : " <> (show old)
    when (old /= Just new) do
      launchAff_ $ halogenIO.query $ H.tell $ Navigate new
