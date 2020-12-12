module Repo where

import Affjax
import Prelude

import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat as ResponseFormat
import Contact (Contact)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Argonaut.Decode.Decoders (decodeString)
import Data.Either (Either(..))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.NonEmpty (NonEmpty)
import Effect.Aff (Aff)
import Foreign (ForeignError(..))
import Simple.JSON as SimpleJSON

findAll :: Aff (Either String (Array Contact))
findAll = do
  items <- get ResponseFormat.json "http://localhost:9000/contacts"
  pure $ case items of
    Left e -> Left $ printError e
    Right i -> 
      case contactsFromJson i.body of
        Left e2 -> Left "Bad Serialization"
        Right r -> Right r


contactsFromString :: String -> Either (NonEmptyList ForeignError) (Array Contact)
contactsFromString s =
  SimpleJSON.readJSON s


contactsFromJson :: Json -> Either JsonDecodeError (Array Contact)
contactsFromJson j =
  decodeJson j