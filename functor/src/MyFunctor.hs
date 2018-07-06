module MyFunctor where

newtype Identity a =
  Identity a
  deriving (Eq, Show)

data Pair a =
  Pair a a
  deriving (Eq, Show)

data Two a b =
  Two a b
  deriving (Eq, Show)

data Three a b c =
  Three a b c
  deriving (Eq, Show)

data Three' a b =
  Three' a b b
  deriving (Eq, Show)

data Four a b c d =
  Four a b c d
  deriving (Eq, Show)

data Four' a b =
  Four' a a a b
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Functor (Two a) where
  fmap f (Two x y) = Two (x) (f y)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three (x) (y) (f z)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' (x) (f y) (f z)

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four (w) (x) (y) (f z)

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' (w) (x) (y) (f z)

