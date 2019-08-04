module PlayClash.Circuit where

import Clash.Prelude

import PlayClash.Adder

{-# ANN chainAdder32 Synthesize
    { t_name = "chain_adder"
    , t_inputs =
        [ PortName "cin"
        , PortName "a"
        , PortName "b"
        ]
    , t_output =
        PortProduct "out"
            [ PortName "cout"
            , PortName "res" ]
    } #-}
{-# NOINLINE chainAdder32 #-}
chainAdder32 :: Bit -> Vec 32 Bit -> Vec 32 Bit -> (Bit, Vec 32 Bit)
chainAdder32 = chainAdder

refAdder :: KnownNat n => Bit -> Vec n Bit -> Vec n Bit -> (Bit, Vec n Bit)
refAdder cin a b = (cout, res)
    where cout :> res = bv2v $ (v2bv a `plus` v2bv b) + boolToBV (bitToBool cin)

{-# ANN refAdder32 Synthesize
    { t_name = "ref_adder"
    , t_inputs =
        [ PortName "cin"
        , PortName "a"
        , PortName "b"
        ]
    , t_output =
        PortProduct "out"
            [ PortName "cout"
            , PortName "res" ]
    } #-}
{-# NOINLINE refAdder32 #-}
refAdder32 :: Bit -> Vec 32 Bit -> Vec 32 Bit -> (Bit, Vec 32 Bit)
refAdder32 = refAdder
