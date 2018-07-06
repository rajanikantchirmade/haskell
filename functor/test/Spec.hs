import MyFunctor
import Test.QuickCheck

twoValGen :: Gen (Integer, Integer)
twoValGen = do
  a <- choose (0,999)
  x <- choose (0,999)
  return (a, x)

threeValGen :: Gen (Integer, Integer, Integer)
threeValGen = do
  a <- choose (0,999)
  x <- choose (0,999)
  y <- choose (0,999)
  return (a, x, y)

fourValGen :: Gen (Integer, Integer, Integer, Integer)
fourValGen = do
  a <- choose (0,999)
  x <- choose (0,999)
  y <- choose (0,999)
  z <- choose (0,999)
  return (a, x, y, z)

prop_functor_identity :: Property
prop_functor_identity =
  forAll twoValGen
  (\(a, x) -> (fmap (+a) (Identity x) == Identity ((+a) x)))

prop_functor_pair :: Property
prop_functor_pair =
  forAll twoValGen
  (\(a, x) -> (fmap (+a) (Pair x x) == Pair ((+a)x) ((+a)x)))

prop_functor_two :: Property
prop_functor_two =
  forAll threeValGen
  (\(a, x, y) -> (fmap (+a) (Two x y) == Two x ((+a)y)))

prop_functor_three :: Property
prop_functor_three =
  forAll fourValGen
  (\(a, x, y, z) -> (fmap (+a) (Three x y z) == Three x y ((+a)z)))

main :: IO ()
main = do
  verboseCheck prop_functor_identity
  verboseCheck prop_functor_pair
  verboseCheck prop_functor_two
  verboseCheck prop_functor_three

