module Binary where

data Binary
  = End
  | Binary :. Bit
  deriving (Show)

data Bit = Zero | One
  deriving (Show)

infixl 6 :.

succBinary :: Binary -> Binary
succBinary End = End :. One 
succBinary (a :. Zero) = a :. One
succBinary (a :. One) = succBinary a :. Zero

integerToBinary :: Integer -> Binary
integerToBinary 0 = End:.Zero
integerToBinary 1 = End:.One
integerToBinary a = 
    if rem a 2 == 1
        then integerToBinary (quot a 2) :. One
        else integerToBinary (quot a 2) :. Zero 

binaryToInteger :: Binary -> Integer
binaryToInteger End = 0
binaryToInteger (a :. Zero) = 2*binaryToInteger a +0
binaryToInteger (a :. One) = 2*binaryToInteger a + 1

hasLeadingZero :: Binary -> Bool
hasLeadingZero (End :. One)= False
hasLeadingZero (End :. Zero)=True
hasLeadingZero (a :. _) = hasLeadingZero a

isEnd :: Binary -> Bool
isEnd End = True
isEnd (End :. _) = False

canonicalise :: Binary -> Binary
canonicalise End = End
canonicalise a = 
    if hasLeadingZero a
        then integerToBinary (binaryToInteger a)
        else a

addBinary :: Binary -> Binary -> Binary
addBinary End End = End
addBinary End a = a
addBinary a End = a
addBinary (a:.Zero) (b:.Zero) = addBinary a b :. Zero
addBinary (a:.One) (b:.Zero) = addBinary a b :. One
addBinary (a:.Zero) (b:.One) = addBinary a b :. One
addBinary (a:.One) (b:.One) = addBinary (a:.One) b:.Zero