newtype Fun a b = Fun {getFun :: a -> b}

-- mapping second type argument is a simple function composition
instance Functor (Fun a) where
    fmap fun1 (Fun fun2) = Fun (fun1 . fun2)

instance Applicative (Fun a) where
    -- constant function
    pure val = Fun (\x -> val)
    -- use function from second functor to compute second argument for the first functor
    (<*>) (Fun fun1) (Fun fun2) = Fun (\x -> fun1 x (fun2 x))
