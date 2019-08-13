module PlayClash.Adder where

import Clash.Prelude

halfAdder :: Bit -> Bit -> (Bit, Bit)
halfAdder a b = (a .&. b, a `xor` b)

{-# ANN fullAdder Synthesize
    { t_name = "full_adder"
    , t_inputs =
        [ PortName "cin"
        , PortName "a"
        , PortName "b"
        ]
    , t_output =
        PortProduct "out"
            [ PortName "cout"
            , PortName "res"
            ]
    } #-}
fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder ci a b = (c1 .|. c2, u2)
    where
        (c1, u1) = halfAdder a b
        (c2, u2) = halfAdder u1 ci

chainAdder :: KnownNat n => Bit -> Vec n Bit -> Vec n Bit -> (Bit, Vec n Bit)
chainAdder cin a b = mapAccumR go cin (zip a b)
    where
        go c (a1, b1) = fullAdder c a1 b1

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
    where cout :> res = bv2v $ (v2bv a `add` v2bv b) + boolToBV (bitToBool cin)

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
