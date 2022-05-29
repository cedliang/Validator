module Main where

import           Control.Exception (throwIO)
import           Validator

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

ioTrueMonoid :: MValidator IO String
ioTrueMonoid = MVal (print "0" >> pure True) "True Monoid Exception"

ioTrue :: Validator IO String
ioTrue = Val (print "0" >> pure True) "True Monoid Exception"

ioFalseMonoid :: MValidator IO String
ioFalseMonoid = MVal (print "1" >> pure False) "False Monoid Exception"

ioThrowMonoid :: MValidator IO String
ioThrowMonoid =
  MVal (throwIO $ userError "test") "Non-Terminating Monoid Exception"

vGroup1 :: MValidator IO String
vGroup1 = MGroup "vGroup1\n" (vGroup2 <> ioFalseMonoid <> ioTrueMonoid)

vGroup2 :: MValidator IO String
vGroup2 = MGroup
  "vGroup2\n"
  (vall
     " Test message"
     [print "0" >> pure True, print "0" >> pure True, print "1" >> pure False])