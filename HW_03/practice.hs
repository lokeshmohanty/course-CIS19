data IntList = Empty | Cons Int IntList
  deriving Show

absAll :: IntList -> IntList
absAll (Cons x xs) = Cons (abs x) (absAll xs)

data NonEmptyList a = NEL a [a]

nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

listToNel :: [a] -> Maybe (NonEmptyList a)
listToNel [] = Nothing
listToNel (x:xs) = Just $ NEL x xs

-- headNEL :: Maybe (NonEmptyList a) -> a
-- headNEL Nothing = 0
-- headNEL (NEL x _) = x
--
-- tailNEL :: NonEmptyList a -> [a]
-- tailNEL (NEL _ xs) = xs

-- main = headNEL $ listToNel [1, 2, 3, 4]

-- Read World Haskell

data BookInfo = Book Int String [String]
  deriving Show

type CustomerID = Int
type Address = [String]
data Customer = Customer {
    customerID      :: CustomerID,
    customerName    :: String,
    customerAddress :: Address
  } deriving (Show)

data Customer' = Customer' Int String [String]
  deriving Show

customerID' :: Customer' -> Int
customerID' (Customer' id _ _) = id

customerName' :: Customer' -> String
customerName' (Customer' _ name  _) = name 

customerAddress' :: Customer' -> [String]
customerAddress' (Customer' _ _ address) = address
