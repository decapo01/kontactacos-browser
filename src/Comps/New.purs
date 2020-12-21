module Comps.New where

import Control.Monad.Reader (class MonadAsk)
import Effect.Aff.Class (class MonadAff)
import Routes (Query(..))


data Action = NoAction

type State = {}

-- newComponent 
--   :: forall q o m r
--    . MonadAff m
--   => MonadAsk m
--   => Navigate m
--   => H.Component HH.HTML q {} o m
-- newComponent =  
