
data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg 

--------------------------------------------------

data PhhhbbtttEither b a =
    Left a
  | Right b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap _ (Main.Right b) = Main.Right b
  fmap f (Main.Left a) = Main.Left (f a)

instance Applicative (PhhhbbtttEither b) where
  pure = Main.Left
  Main.Right b <*> _ = Main.Right b
  Main.Left _ <*> Main.Right b = Main.Right b
  Main.Left f <*> Main.Left a = Main.Left (f a)

instance Monad (PhhhbbtttEither b) where
  return = pure
  Main.Right b >>= _  = Main.Right b
  Main.Left a >>= f = f a

--------------------------------------------------

data Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  return = pure
-- f a -> (a -> f b) -> f b  
  Identity a >>= f = f a










