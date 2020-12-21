module Routes where

import Data.Generic.Rep
import Prelude
import RouteItems

import Components (contactTableComponent)
import Control.Monad.Reader (class MonadAsk, lift)
import Control.Monad.Trans.Class (lift)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Navigate (class Nav, navigate)
import Routing.Duplex as RD
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Hash (getHash)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (toEvent)


data Query a
  = Navigate Route a

type State =
  { route :: Maybe Route
  }

initialState :: State
initialState = { route: Nothing}

type OpaqueSlot slot = forall query. H.Slot query Void slot

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , new  :: OpaqueSlot Unit
  , edit :: OpaqueSlot Unit
  )

type UserEnv = {}


type EmptySlots = ()

component
  :: forall m
   . MonadAff m
  => Nav m
  -- => MonadAsk { userEnv :: UserEnv | r } m
  => H.Component HH.HTML Query {} Void m
component =
  H.mkComponent
  { initialState: \_ -> initialState
  , render: render
  , eval: H.mkEval $ H.defaultEval
    { handleQuery = handleQuery
    , handleAction = handleAction
    -- , receive = Just <<< Rec
    , initialize = Just Init
    }
  }
  where

    handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
    handleQuery = case _ of
      Navigate dest a -> do
        { route } <- H.get
        when (route /= Just dest) do
          liftEffect $ log $ "in handle query " <> (show dest)
          H.modify_ \st -> st { route = route }
        pure (Just a)

    handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
    handleAction = case _ of
      Init -> do
        hash <- H.liftEffect getHash
        H.lift $ log $ show hash
        initialRoute <- hush <<< (RD.parse routeCodec) <$> H.liftEffect getHash
        H.lift $ log $ show initialRoute
        navigate $ fromMaybe Home initialRoute
        let route = fromMaybe Home initialRoute
        H.modify_ $ \st -> st { route = Just route }
      Rec -> do
        -- hash <- H.liftEffect getHash
        -- H.lift $ log $ show hash
        -- initialRoute <- hush <<< (RD.parse routeCodec) <$> H.liftEffect getHash
        -- H.lift $ log $ show initialRoute
        -- navigate $ fromMaybe Home initialRoute
        -- let route = fromMaybe Home initialRoute
        -- H.modify_ $ \st -> st { route = Just route }
        H.modify_ $ \st -> st
      ShowEdit -> do
        H.liftEffect $ log "edit shown"
        H.modify_ $ \st -> st { route = Just $ Edit "1" }
      GoTo route e -> do
        liftEffect $ log ("go to " <> (show route))
        liftEffect $ preventDefault ( toEvent e )
        mRoute <- H.gets _.route
        when (mRoute /= Just route) $ navigate route
      NoAct ->
        H.modify_ $ \st -> st


    render :: State -> H.ComponentHTML Action ChildSlots m
    render { route } =
      case route of
        Just r -> 
          case r of
            Home -> do
              HH.slot (SProxy :: _ "home") unit contactTableComponent {} absurd
            New ->
              -- HH.slot (SProxy :: _ "new") unit contactTableComponent {} absurd
              HH.div_ [HH.text "New Contact"]
            Edit idStr ->
              -- HH.slot (SProxy :: _ "edit") unit contactTableComponent {} absurd
              HH.div_ [HH.text "Edit Contact"]
        Nothing ->
          HH.div_ [HH.text "Not Found!"]



-- render_ :: forall m. State -> H.ComponentHTML Action ChildSlots m
-- render_ s =
--   case s.route of
--     Just r -> 
--       case r of
--         Home ->
--           slot (SProxy :: _ "home") unit contactTableComponent unit absurd
--         New ->
--           slot (SProxy :: _ "new") unit contactTableComponent unit absurd
--         Edit idStr ->
--           slot (SProxy :: _ "edit") unit contactTableComponent unit absurd
--     Nothing ->
--       HH.div_ [HH.text "Not Found!"]