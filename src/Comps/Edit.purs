module Comps.Edit where

import Prelude

import Contact (Contact)
import Control.Monad.ST.Internal (modify)
import Control.Monad.State (state)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, liftEffect, mkComponent, mkEval, modify_)
import Halogen.HTML (HTML, br_, form_, input, text, div_)
import Halogen.HTML.Properties (InputType(..), name, type_, value)
import Halogen.VDom.Types (VDom(..))
import Repo (findById)


type State = 
  { id :: String 
  , maybeContact :: Maybe Contact
  }

initialState :: String -> State
initialState id = { maybeContact: Nothing, id: id }

data Action
  = NoAction String

componentEdit
  :: forall q m
   . MonadAff m
  => String -> Component HTML q {} Void m
componentEdit id =
  mkComponent
  { initialState: \_ -> initialState id
  , render: render
  , eval: mkEval $ defaultEval 
    { handleAction = handleAction
    , initialize = Just $ NoAction id
    }
  }


itemOrEmpty :: forall a. Maybe a ->  (a -> String) -> String
itemOrEmpty item func =
  fromMaybe "" $ map func item

render :: forall m. MonadAff m => State -> ComponentHTML Action () m
render state =
  form_
  [ div_ [text "Name"]
  , input [type_ InputText, name "name", value $ itemOrEmpty state.maybeContact _.name ]
  , div_ [text "Email"]
  , input [type_ InputText, name "email", value $ itemOrEmpty state.maybeContact _.email ]
  , div_ [text "Phone Number"]
  , input [type_ InputText, name "phone", value $ itemOrEmpty state.maybeContact _.phone ]
  , br_
  , input [type_ InputSubmit]
  ]

handleAction :: forall m. MonadAff m => Action -> HalogenM State Action () Void m Unit
handleAction action =
  case action of
    NoAction id -> do
      res <- liftAff $ findById id
      case res of
        Left _ -> modify_ \st -> st
        Right c ->  
          modify_ \st -> st { maybeContact = c }