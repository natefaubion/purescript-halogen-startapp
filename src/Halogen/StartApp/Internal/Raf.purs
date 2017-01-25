module Halogen.StartApp.Internal.Raf where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import DOM (DOM)

foreign import requestAnimationFrame
  :: forall eff
   . Eff (dom :: DOM | eff) Unit
  -> Eff (dom :: DOM | eff) Unit
