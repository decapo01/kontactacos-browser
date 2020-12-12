module Components where

import Contact
import Halogen
import Halogen.HTML (HTML, td_, th_, text, table_, thead_, tr_, tbody_)
import Prelude

import Data.Either (Either(..))
import Data.Functor as Functor
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Repo as Repo

type ContactTableState = { contacts :: Array Contact }


initialContactTableState :: forall a. a -> ContactTableState
initialContactTableState _ = { contacts: [] }

data ContactTableAction = NoAction

contactTableComponent :: forall q i o m. Component HTML q i o Aff
contactTableComponent =
  mkComponent
  { initialState: initialContactTableState
  , render
  , eval: mkEval $ defaultEval { handleAction =  handle , initialize = Just NoAction }
  }
  where
    render :: forall m. ContactTableState -> ComponentHTML ContactTableAction () m
    render state =
      table_ 
      [ thead_
        [ tr_
          [ th_ [text "Name"]
          , th_ [text "Email"]
          , th_ [text "Phone"]
          ]
        ]
      , tbody_ $ Functor.map renderRow state.contacts 
      ]
    handle :: forall a. ContactTableAction -> HalogenM ContactTableState ContactTableAction () a Aff Unit
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
  ]