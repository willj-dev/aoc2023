module Day01Spec where

import Test.Hspec

import Day01

spec :: Spec
spec = do
  it "should read first and last digits in a string" $ do
    firstLastDigit "1abc2"        `shouldBe` (1, 2)
    firstLastDigit "pqr3stu8vwx"  `shouldBe` (3, 8)
    firstLastDigit "a1b2c3d4e5f"  `shouldBe` (1, 5)
    firstLastDigit "treb7uchet"   `shouldBe` (7, 7)

  it "should read first and last digits in a string, part two" $ do
    firstLastDigit' "two1nine"          `shouldBe` (2, 9)
    firstLastDigit' "eightwothree"      `shouldBe` (8, 3)
    firstLastDigit' "abcone2threexyz"   `shouldBe` (1, 3)
    firstLastDigit' "xtwone3four"       `shouldBe` (2, 4)
    firstLastDigit' "4nineeightseven2"  `shouldBe` (4, 2)
    firstLastDigit' "zoneight234"       `shouldBe` (1, 4)
    firstLastDigit' "7pqrstsixteen"     `shouldBe` (7, 6)
    
  it "should convert a tens and a ones digit to a single number" $ do
    twoDigits (1, 2) `shouldBe` 12
    twoDigits (3, 8) `shouldBe` 38
    twoDigits (1, 5) `shouldBe` 15
    twoDigits (7, 7) `shouldBe` 77