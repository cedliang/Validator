module Main where

-- import           Validator
import           Control.Exception (throwIO)
import           Validator

-- import           Validator3
data TestException = Except0
                   | Except1
                   | Except2
  deriving (Show)

main :: IO ()
main = do
  r <- validate $ ioThrowSame
  print r

ioTrue :: Validator IO TestException
ioTrue = Val (print "0" >> pure True) Except0

ioFalse :: Validator IO TestException
ioFalse = Val (print "1" >> pure False) Except1

ioThrows :: Validator IO TestException
ioThrows = Val (throwIO $ userError "test") Except2

ioThrowSame :: Validator IO TestException
ioThrowSame = vall Except2 [print "0" >> pure True, print "1" >> pure False]