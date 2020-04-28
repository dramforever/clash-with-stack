{-# OPTIONS -Wno-orphans #-}
module PlayClash.Utils where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as E

import GHC.Exts
import GHC.Stack

instance IsString PortName where
    fromString = PortName

instance IsList PortName where
    type Item PortName = PortName

    fromList = PortProduct ""

    toList = error "toList for PortName is not implemented"

enable :: _
    => Signal dom Bool
    -> (HiddenEnable dom => a)
    -> (HiddenEnable dom => a)
enable en a =
    hideEnable (\en0 -> exposeEnable a (E.enable en0 en))

unreachable :: HasCallStack => a
unreachable = withFrozenCallStack $ error "unreachable"
