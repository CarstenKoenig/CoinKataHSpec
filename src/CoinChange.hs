module CoinChange where

data Pieces
   = Cent
   | TwoCent
   | FiveCent
   | TenCent
   | TwentyCent
   | FiftyCent
   | Euro
   | TwoEuro
   deriving (Show, Eq, Ord)

pieces :: [Pieces]
pieces = [TwoEuro, Euro, FiftyCent, TwentyCent, TenCent, FiveCent, TwoCent, Cent]

pieceValue :: Pieces -> Int
pieceValue Cent       = 1
pieceValue TwoCent    = 2
pieceValue FiveCent   = 5
pieceValue TenCent    = 10
pieceValue TwentyCent = 20
pieceValue FiftyCent  = 50
pieceValue Euro       = 100
pieceValue TwoEuro    = 200

changeFor :: Int -> [Pieces]
changeFor = change pieces
   where change [] amount
            | amount == 0            = []
            | otherwise              = error $ "don't know how to change " ++ show amount
         change cs@(c:cs') amount
            | pieceValue c <= amount = c:change cs (amount - pieceValue c)
            | otherwise              = change cs' amount

