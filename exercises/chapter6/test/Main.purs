module Test.Main where

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

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty el1 arr1) (NonEmpty el2 arr2) = el1 == el2 && arr1 == arr2

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty el rest) (NonEmpty el2 rest2) = (NonEmpty el (rest <> [el2] <> rest2))

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty el arr) = show ([el] <> arr)

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty el arr) = (NonEmpty (f el) (map f arr))
  