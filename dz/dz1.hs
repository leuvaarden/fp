data Complex a = Comp a a deriving (Show, Eq)

realPart :: Complex a -> a
realPart (Comp a b) = a

imagPart :: Complex a -> a
imagPart (Comp a b) = b

-- Floating because we use sqrt
instance (Floating a => Num (Complex a)) where
    (Comp a1 b1) + (Comp a2 b2) = Comp (a1 Prelude.+ a2) (b1 Prelude.+ b2)
    (Comp a1 b1) - (Comp a2 b2) = Comp (a1 Prelude.- a2) (b1 Prelude.- b2)
    (Comp a1 b1) * (Comp a2 b2) = Comp (a1 Prelude.* a2 Prelude.- b1 Prelude.* b2) (a1 Prelude.* b2 Prelude.+ b1 Prelude.* a2)
    negate (Comp a1 b1) = Comp (Prelude.negate a1) (Prelude.negate b1)
    -- result of abs is float => use realPart to extract value
    abs (Comp a1 b1) = Comp (sqrt (a1 Prelude.* a1 Prelude.+ b1 Prelude.* b1)) 0
    signum (Comp a1 b1) = Comp (a1 / (realPart (abs (Comp a1 b1)))) (b1 / (realPart (abs (Comp a1 b1))))
    -- wasn't required but compiler raised a warning
    fromInteger n = Comp (fromInteger n) (fromInteger 0)
