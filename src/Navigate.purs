module Navigate where

import Prelude
import RouteItems (Route)
import Halogen as H

class Monad m <= Nav m where
  navigate :: Route -> m Unit
  -- logout :: m Unit

instance navHalogenM :: Nav m => Nav (H.HalogenM st act slots msg m) where
  navigate = H.lift <<< navigate
  -- logout = lift logout