{-# OPTIONS_GHC -XFlexibleInstances #-}
{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
module Test.AssertH where

import qualified Test.HUnit (Assertion, Test(..), assertEqual, assertBool, runTestTT)
import qualified Test.HUnit.Base (Counts)
import qualified Control.Monad.Writer (WriterT, tell, execWriter)
import qualified Control.Applicative (Applicative)
import qualified Control.Monad.Trans.Class (MonadTrans)
import qualified Data.Monoid (All(..))
import qualified Data.Functor.Identity (Identity)

class Assertion a where 
  assertTrue :: String -> Bool -> a

instance Assertion (IO ()) where 
  assertTrue = Test.HUnit.assertBool

newtype MockAssertion m a = MockAssertion 
                              { runMessage :: Control.Monad.Writer.WriterT (Data.Monoid.All,String) m a }
--                              { runMessage :: Control.Monad.Writer.WriterT String m a }  
                              deriving (Control.Applicative.Applicative, Functor, Monad, Control.Monad.Trans.Class.MonadTrans)

instance Monad m => Assertion (MockAssertion m ()) where
--  assertTrue condition message = Control.Monad.Writer.tell (condition, message)
  assertTrue msg condition = MockAssertion $ do
                                  Control.Monad.Writer.tell (Data.Monoid.All condition, msg)
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
                        condition = start <= value && value <= end
			msg | condition = ""
                            | otherwise = show value ++ " is not within range [" ++ show start ++ ", " ++ show end ++ "]"
                           

  isEqualTo (IntegerAssert value) expected = assertion msg condition
                      where
                        condition = expected == value
                        msg | condition = ""
                            | otherwise = show value ++ " is not equal to " ++ show expected  

assertThat :: Int -> IntegerAssert Int
assertThat value = IntegerAssert value


integerIsEqualToFailing :: MockAssertion Data.Functor.Identity.Identity ()
integerIsEqualToFailing = assertThat 2 `isEqualTo` 3

integerIsEqualToSuccess :: MockAssertion Data.Functor.Identity.Identity ()
integerIsEqualToSuccess = assertThat 11 `isEqualTo` 11

integerIsBetweenFailing :: MockAssertion Data.Functor.Identity.Identity ()
integerIsBetweenFailing = (assertThat 2 `isBetween` 0) 1

integerIsBetweenSuccess :: MockAssertion Data.Functor.Identity.Identity ()
integerIsBetweenSuccess = (assertThat 11 `isBetween` 10) 20


integerAssertOfDifferentValuesHasProperMessage :: Test.HUnit.Test 
integerAssertOfDifferentValuesHasProperMessage = Test.HUnit.TestCase $ do 
                                    let (condition, failedMessage) = Control.Monad.Writer.execWriter $ runMessage integerIsEqualToFailing 
                                    Test.HUnit.assertEqual "after failing test, the message should reflect inequality" "2 is not equal to 3" failedMessage
                                    Test.HUnit.assertBool "after failing test, condition should be false" (not $ Data.Monoid.getAll condition)  

integerAssertOfEqualValuesHasProperMessage :: Test.HUnit.Test
integerAssertOfEqualValuesHasProperMessage = Test.HUnit.TestCase $ do 
                                    let (condition, successMessage) = Control.Monad.Writer.execWriter $ runMessage integerIsEqualToSuccess
                                    Test.HUnit.assertEqual "after successful test, the message should be empty" "" successMessage
                                    Test.HUnit.assertBool "after successful test, condition should be true" (Data.Monoid.getAll condition)

integerAssertOfNotInRangeValueHasProperMessage :: Test.HUnit.Test 
integerAssertOfNotInRangeValueHasProperMessage = Test.HUnit.TestCase $ do 
                                    let (condition, failedMessage) = Control.Monad.Writer.execWriter $ runMessage integerIsBetweenFailing 
                                    Test.HUnit.assertEqual "after failing test, the message should reflect inequality" "2 is not within range [0, 1]" failedMessage
                                    Test.HUnit.assertBool "after failing test, condition should be false" (not $ Data.Monoid.getAll condition)  

integerAssertOfInRangeValueHasProperMessage :: Test.HUnit.Test
integerAssertOfInRangeValueHasProperMessage = Test.HUnit.TestCase $ do 
                                    let (condition, successMessage) = Control.Monad.Writer.execWriter $ runMessage integerIsBetweenSuccess
                                    Test.HUnit.assertEqual "after successful test, the message should be empty" "" successMessage
                                    Test.HUnit.assertBool "after successful test, condition should be true" (Data.Monoid.getAll condition)

allTests :: Test.HUnit.Test
allTests = Test.HUnit.TestList [integerAssertOfDifferentValuesHasProperMessage, integerAssertOfEqualValuesHasProperMessage, integerAssertOfNotInRangeValueHasProperMessage, integerAssertOfInRangeValueHasProperMessage]

main :: IO Test.HUnit.Base.Counts
main = Test.HUnit.runTestTT allTests

                                                      

