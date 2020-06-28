{-# LANGUAGE TemplateHaskell #-}
module PlayClash.Utils.Intel50M where

import Clash.Prelude

$(createDomain vXilinxSystem
    { vName = "Intel50M"
    , vPeriod = hzToPeriod 50e6
    })
