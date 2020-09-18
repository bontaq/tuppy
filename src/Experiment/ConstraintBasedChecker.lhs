> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE DeriveAnyClass #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE DerivingStrategies #-}
> module ConstraintBasedChecker where

> import GHC.Generics hiding (from)
> import Data.Hashable
> import Data.Holmes

> data Value = V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9
>   deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
>   deriving anyclass (Hashable)

> instance Num Value where -- Syntactic sugar for numeric literals.
>   fromInteger = toEnum . pred . fromInteger

> definedConfig :: Config Holmes (Intersect Int)
> definedConfig = 1 `from` [ 1 .. 9 ]

> test :: Config Holmes (Intersect Value)
> test = let x = mempty
>            y = mempty
>        in using
>           [x, 1, y]

> constraints
>   :: forall m. MonadCell m
>   => [Prop m (Intersect Value)]
>   -> Prop m (Intersect Bool)
> constraints test = all' (.== 1) test

> final = test `whenever` constraints
