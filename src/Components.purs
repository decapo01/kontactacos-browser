module Components where

import Contact
import Halogen
import Prelude

import Data.Either (Either(..))
import Data.Functor as Functor
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen.HTML (HTML, a, span_, table_, tbody_, td_, text, th_, thead_, tr_, span_)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (href)
import Repo as Repo
import RouteItems (Action(..), Route(..), safeHref)



type ContactTableState = { contacts :: Array Contact }


initialContactTableState :: forall a. a -> ContactTableState
initialContactTableState _ = { contacts: [] }

data ContactTableAction 
  = NoAction

contactTableComponent :: forall q m. MonadAff m => Component HTML q {} Void m
contactTableComponent =
  mkComponent
  { initialState: initialContactTableState
  , render
  , eval: mkEval $ defaultEval { handleAction =  handle , initialize = Just NoAction }
  }
  where
    render :: ContactTableState -> ComponentHTML ContactTableAction () m
    render state =
      table_ 
      [ thead_
        [ tr_
          [ th_ [text "Name"]
          , th_ [text "Email"]
          , th_ [text "Phone"]
          , th_ [text "View"]
          , th_ [text "Edit"]
          ]
        ]
      , tbody_ $ Functor.map renderRow state.contacts 
      ]
    handle :: ContactTableAction -> HalogenM ContactTableState ContactTableAction () Void m Unit
    handle =
      case _ of
        NoAction -> do
          contactsEither <- liftAff Repo.findAll
          case contactsEither of
            Left _ -> modify_ \st -> st
            Right cs ->  modify_ \st -> st { contacts = cs }



renderRow :: forall m. Contact -> ComponentHTML ContactTableAction () m
renderRow c =
  tr_ 
  [ td_ [text c.name]
  , td_ [text c.email]
  , td_ [text c.phone]
  , td_
    [ a [safeHref New ] [text "View"]
    ]
  , td_
    [ a 
      [ href "#/contacts/1"
        -- safeHref $ Edit "1"
        -- onClick (Just <<< (GoTo $ Edit "1"))
      ] [text "Editt"]
    ]
  ]