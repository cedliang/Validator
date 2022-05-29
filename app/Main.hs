module Main where

import           Control.Exception (throwIO)
import           MoidValidator
import           Validatable

data TestException = Except0
                   | Except1
                   | Except2
  deriving (Show)

main :: IO ()
main = do
  r <- validate $ ioTrueMonoid <> ioTrueMonoid <> vGroup1 <> vGroup2
  case r of
    Success -> print r
    Fail e  -> putStrLn $ "Fail " ++ e

ioTrueMonoid :: Validator IO String
ioTrueMonoid = Val (print "0" >> pure True) "True Monoid Exception"

ioFalseMonoid :: Validator IO String
ioFalseMonoid = Val (print "1" >> pure False) "False Monoid Exception"

ioThrowMonoid :: Validator IO String
ioThrowMonoid =
  Val (throwIO $ userError "test") "Non-Terminating Monoid Exception"

vGroup1 :: Validator IO String
vGroup1 = ValGroup "vGroup1\n" (vGroup2 <> ioFalseMonoid <> ioTrueMonoid)

vGroup2 :: Validator IO String
vGroup2 = ValGroup
  "vGroup2\n"
  (vall
     " Test message"
     [print "0" >> pure True, print "0" >> pure True, print "1" >> pure False])