module PlayClash.Ports where

import Clash.Prelude
import GHC.Exts

newtype WrappedPort
    = WrappedPort { port :: PortName }
    deriving (Show)

instance IsString WrappedPort where
    fromString = coerce PortName

instance IsList WrappedPort where
    type Item WrappedPort = WrappedPort

    fromList = coerce (PortProduct "")

    toList = error "toList is not available for WrappedPort"

ports :: [WrappedPort] -> [PortName]
ports = coerce
