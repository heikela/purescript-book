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
