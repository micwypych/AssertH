{-# OPTIONS_GHC -XFlexibleInstances #-}
{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
module Test.AssertH where

import qualified Test.HUnit (Assertion, Test(..), assertEqual, assertBool, runTestTT)
import qualified Test.HUnit.Base (Counts)
import qualified Control.Monad.Writer (WriterT, tell, execWriter)
import qualified Control.Applicative (Applicative)
import qualified Control.Monad.Trans.Class (MonadTrans)
import qualified Data.Monoid (All)
import qualified Data.Functor.Identity (Identity)

class Assertion a where 
  assertTrue :: String -> Bool -> a

instance Assertion (IO ()) where 
  assertTrue = Test.HUnit.assertBool

newtype MockAssertion m a = MockAssertion 
--                              { message :: Control.Monad.Writer.WriterT (Data.Monoid.All,String) m a }
                              { runMessage :: Control.Monad.Writer.WriterT String m a }  
                              deriving (Control.Applicative.Applicative, Functor, Monad, Control.Monad.Trans.Class.MonadTrans)

instance Monad m => Assertion (MockAssertion m ()) where
--  assertTrue condition message = Control.Monad.Writer.tell (condition, message)
  assertTrue msg condition = MockAssertion $ do
                                  Control.Monad.Writer.tell msg
                                  return ()

assertion :: Assertion a => String -> Bool -> a
assertion msg condition = assertTrue msg condition

class AbstractIntegerAssert a where 
  isBetween :: (Show b, Num b, Eq b, Ord b, Assertion c) => a b -> b -> b-> c
  isEqualTo :: (Show b, Num b, Eq b, Ord b, Assertion c) => a b -> b -> c

data IntegerAssert intValue = IntegerAssert intValue

instance AbstractIntegerAssert IntegerAssert where
  isBetween (IntegerAssert value) start end = assertion msg condition
                      where
                        msg = show value ++ " is not within range [" ++ show start ++ ", " ++ show end ++ "]"
                        condition = start <= value && value <= end

  isEqualTo (IntegerAssert value) expected = assertion msg condition
                      where
                        msg = show value ++ " is not equal to " ++ show expected
                        condition = expected == value
  

assertThat :: Int -> IntegerAssert Int
assertThat value = IntegerAssert value

integerIsEqualToFailing :: MockAssertion Data.Functor.Identity.Identity ()
integerIsEqualToFailing = assertThat 2 `isEqualTo` 3

integerIsEqualToSuccess :: MockAssertion Data.Functor.Identity.Identity ()
integerIsEqualToSuccess = assertThat 2 `isEqualTo` 3


integerAssertOfDifferentValuesHasProperMessage :: Test.HUnit.Test 
integerAssertOfDifferentValuesHasProperMessage = Test.HUnit.TestCase $ do 
                                    let failedMessage = Control.Monad.Writer.execWriter $ runMessage integerIsEqualToFailing 
                                    Test.HUnit.assertEqual "" "2 is not equal to 3" failedMessage

allTests :: Test.HUnit.Test
allTests = Test.HUnit.TestList [integerAssertOfDifferentValuesHasProperMessage]

main :: IO Test.HUnit.Base.Counts
main = Test.HUnit.runTestTT allTests

                                                      

