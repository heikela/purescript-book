module Test.Main where

import Data.Foldable
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Hashable (hash, hashEqual)

main :: Eff (console :: CONSOLE) Unit
main = do
  logShow (hash 123)
  logShow (hash true)
  logShow (hash [1, 2, 3])
  logShow (hash "testing")
  logShow (hash 'a')
  logShow ("foo" `hashEqual` "foo")
  logShow ("foo" `hashEqual` "bar")

newtype Complex = Complex {
  real :: Number,
  imaginary :: Number
}

instance showComplex :: Show Complex where
  show (Complex {real, imaginary}) | imaginary >= 0.0 = (show real) <> " + " <> (show imaginary) <> "i"
  show (Complex {real, imaginary}) = (show real) <> " - " <> (show (-1.0 * imaginary)) <> "i"

instance eqComplex :: Eq Complex where
  eq (Complex {real: r1, imaginary: i1}) (Complex {real: r2, imaginary: i2}) = r1 == r2 && i1 == i2

data NonEmpty a = NonEmpty a (Array a)

ne1 = NonEmpty 0.0 []
ne2 = NonEmpty 0.0 [1.0, 2.0]

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty el1 arr1) (NonEmpty el2 arr2) = el1 == el2 && arr1 == arr2

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty el rest) (NonEmpty el2 rest2) = (NonEmpty el (rest <> [el2] <> rest2))

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty el arr) = show ([el] <> arr)

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty el arr) = (NonEmpty (f el) (map f arr))

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr f b (NonEmpty el arr) = f el (foldr f b arr)
  foldl f b (NonEmpty el arr) = foldl f (f b el) arr
  foldMap f (NonEmpty el arr) = (f el) <> (foldMap f arr)

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr f b (OneMore x xs) = f x (foldr f b xs)
  foldl f b (OneMore x xs) = foldl f (f b x) xs
  foldMap f (OneMore x xs) = (f x) <> (foldMap f xs)

data Extended a = Finite a | Infinite

instance showExtended :: Show a => Show (Extended a) where
  show (Finite a) = show a
  show Infinite = "infinite"

instance ordExtended :: Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite a) (Finite b) = compare a b

instance eqExtended :: Ord (Extended a) => Eq (Extended a) where
  eq a b = (compare a b) == EQ
