import Data.Time.Clock
import Data.Time.Calendar

taxRate :: Float
taxRate = 0.065

digitize :: Integer -> [Integer]
digitize 0 = []
digitize x = digitize (x `div` 10) ++ [x `mod` 10]

cardType :: Integer -> String
cardType x | head (digitize x) == 4 = "Visa"
           | head (digitize x) == 5 = "MasterCard"
           | head (digitize x) == 6 = "Discover"
           | otherwise = "Not accepted"

pastDate :: Integer -> Int -> Int -> IO Bool
pastDate y m d = do
  d1 <- fmap utctDay getCurrentTime
  return ((fromGregorian y m d) < d1)

calcTax :: Float -> Float
calcTax i = i * taxRate

--verifyCard :: Integer -> Integer -> Int -> Int -> Bool
--verifyCard cardNum y m d = ((cardType cardNum) /= "Not accepted")
--                           && (pastDate y m d)

verifyCard :: Integer -> Integer -> Int -> Int -> IO Bool
verifyCard cardNum y m d = do
  exp <- pastDate y m d
  return (((cardType cardNum) /= "Not accepted") && not(exp))

checkOut :: Float -> IO ()
checkOut num = do
  putStrLn "Enter cost: "
  cost <- getLine
  let amount = (read cost :: Float)
  if amount /= 0
    then do putStrLn (show amount)
            checkOut (amount + num)
  else putStrLn (show (amount + (num +(calcTax (amount + num)))))
