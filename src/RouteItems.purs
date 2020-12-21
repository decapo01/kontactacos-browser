module RouteItems where

import Data.Generic.Rep
import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routing.Duplex as RD
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic.Syntax ((/))
import Web.UIEvent.MouseEvent (MouseEvent)

data Route
  = Home
  | New
  | Edit String

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show r = case r of
    Home -> "the home route"
    New  -> "the new route"
    Edit id -> "the edit route" <> id

routeCodec :: RD.RouteDuplex' Route
routeCodec = RD.root $ G.sum
  { "Home": "contacts" / G.noArgs
  , "New":  "contacts" / "new" / G.noArgs
  , "Edit": "contacts" / RD.string RD.segment
  }


data Action
  = Init
  | Rec
  | ShowEdit
  | GoTo Route MouseEvent
  | NoAct

safeHref :: forall r i. Route -> HH.IProp ( href :: String | r) i
safeHref = HP.href <<< append "#" <<< RD.print routeCodec