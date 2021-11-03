module Binary where

data Binary
  = End
  | Binary :. Bit
  deriving Show

data Bit = Zero | One
  deriving Show

infixl 6 :.

succBinary :: Binary -> Binary
succBinary End = End :. One
succBinary (End :. One) = End :. One :. Zero 
succBinary (End :. Zero) = End :. One
succBinary (a :. Zero) = a :. One
succBinary (a :. One) = succBinary a :. Zero

flipBinary :: Binary -> Binary -> Binary 
flipBinary End temp = temp
flipBinary(a :. b) temp = flipBinary a (temp :. b)

integerToBinary :: Integer -> Binary
integerToBinary a = flipBinary (helper a End) End
 where 
     helper :: Integer -> Binary -> Binary
     helper 0 result = result
     helper num result = if even num then helper (num `div` 2) (result :. Zero) else helper (num `div` 2) (result :. One)

binaryToInteger :: Binary -> Integer
binaryToInteger arg = helper arg 0 0
 where
     helper :: Binary -> Integer -> Integer -> Integer 
     helper End _ result = result
     helper (a :. One) powc result = helper a (powc + 1) (2 ^ powc + result)
     helper (a :. Zero) powc result = helper a (powc + 1) result

hasLeadingZero :: Binary -> Bool
hasLeadingZero (End :. Zero) = True
hasLeadingZero (End :. One) = False
hasLeadingZero End = False
hasLeadingZero (a :. _) = hasLeadingZero a 

isEnd :: Binary -> Bool
isEnd End = True
isEnd _ = False

canonicalise :: Binary -> Binary
canonicalise num = flipBinary (helper (flipBinary num End)) End
  where
    helper :: Binary -> Binary 
    helper(b :. Zero) = helper b
    helper (b :. One) = b :. One
    helper End = End

isZero :: Bit -> Bool 
isZero Zero = True 
isZero _ = False 

addBinary :: Binary -> Binary -> Binary
addBinary arg args = flipBinary (helper arg args Zero End) End
 where
  helper :: Binary -> Binary -> Bit -> Binary -> Binary
   --Both ended:
  helper End End carry result 
    | isZero carry = result
    | otherwise = result :. One
  --first ended:
  helper End (a :. b) carry result 
   | isZero carry = helper End a Zero (result :. b)
   | isZero b = helper a End Zero (result :. One)
   | otherwise = helper End a One (result :. Zero)
   --second ended
  helper (a :. b) End  carry result 
   | isZero carry = helper a End Zero (result :. b)
   | isZero b = helper a End Zero (result :. One)
   | otherwise = helper a End One (result :. Zero)
   --two ones at the end:
  helper (a :. One) (b :. One) carry result = helper a b One (result :. carry)
  --One and Zero
  helper (a :. One) (b :. Zero) carry result
   | isZero carry = helper a b Zero (result :. One)
   | otherwise = helper a b One (result :. Zero)
  --Zero One
  helper (a :. Zero) (b :. One) carry result
   | isZero carry = helper a b Zero (result :. One)
   | otherwise = helper a b One (result :. Zero)
  --Zero Zero
  helper (a :. Zero) (b :. Zero) carry result = helper a b Zero (result :. carry)
