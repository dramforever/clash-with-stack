module PlayClash.Adder where

import Clash.Prelude

halfAdder :: Bit -> Bit -> (Bit, Bit)
halfAdder a b = (a .&. b, a `xor` b)

fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder a b z = (c1 .|. c2, u2)
    where
        (c1, u1) = halfAdder a b
        (c2, u2) = halfAdder u1 z

chainAdder :: KnownNat n => Bit -> Vec n Bit -> Vec n Bit -> (Bit, Vec n Bit)
chainAdder cin a b = mapAccumR go cin (zip a b)
    where
        go c (a1, b1) = fullAdder c a1 b1
