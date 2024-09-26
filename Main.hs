module Main where

import Data.Function ((&))
import Data.Text (Text, pack, unpack)
import Data.Text.IO qualified as T
import Text.Printf (printf)

data Tree a
  = Leaf
  | Branch (Tree a) a (Tree a)
  deriving (Eq, Show)

tabs :: [Bool] -> Text
tabs flag = flag & reverse & concatMap transform & pack
 where
  transform x = if x then " │" else "  "

showTree :: Tree Text -> [Bool] -> Text
showTree Leaf _ = " ○\n" & pack
showTree (Branch kir val kan) b =
  printf
    "[%s]\n%s ├%s%s └%s"
    val
    (tabs b)
    (aux kir True b)
    (tabs b)
    (aux kan False b)
    & pack
 where
  aux lengan bool bs =
    if lengan == Leaf
      then "○\n"
      else showTree lengan (bool : bs) & unpack

showStringTree :: Tree Text -> Text
showStringTree = flip showTree []

tree1, tree2, tree3, tree4, tree5 :: Tree Text
tree1 = Leaf
tree2 = Branch Leaf (pack "Andi") Leaf
tree3 =
  Branch
    (Branch Leaf (pack "Budi") Leaf)
    (pack "Alisia")
    (Branch (Branch Leaf (pack "Diana") Leaf) (pack "Cecilia") Leaf)
tree4 =
  Branch
    ( Branch
        (Branch Leaf (pack "Aria") Leaf)
        (pack "Budi")
        (Branch Leaf (pack "Damar") Leaf)
    )
    (pack "Elisa")
    ( Branch
        ( Branch
            (Branch Leaf (pack "Faris") Leaf)
            (pack "Gina")
            Leaf
        )
        (pack "Hana")
        ( Branch
            (Branch Leaf (pack "Ira") Leaf)
            (pack "Jaka")
            Leaf
        )
    )
tree5 =
  Branch
    ( Branch
        ( Branch
            (Branch Leaf (pack "Aria") Leaf)
            (pack "Budi")
            ( Branch
                (Branch Leaf (pack "Citra") Leaf)
                (pack "Damar")
                Leaf
            )
        )
        (pack "Elisa")
        ( Branch
            (Branch Leaf (pack "Farah") Leaf)
            (pack "Gina")
            (Branch Leaf (pack "Hana") Leaf)
        )
    )
    (pack "Indra")
    ( Branch
        ( Branch
            ( Branch
                (Branch Leaf (pack "Jaka") Leaf)
                (pack "Kania")
                (Branch Leaf (pack "Lila") Leaf)
            )
            (pack "Maya")
            ( Branch
                (Branch Leaf (pack "Nina") Leaf)
                (pack "Oscar")
                Leaf
            )
        )
        (pack "Prita")
        ( Branch
            ( Branch
                (Branch Leaf (pack "Qori") Leaf)
                (pack "Rian")
                Leaf
            )
            (pack "Sari")
            ( Branch
                (Branch Leaf (pack "Tara") Leaf)
                (pack "Umar")
                Leaf
            )
        )
    )

consTree :: a -> Tree a
consTree v = Branch Leaf v Leaf

masukkanIntKeTree :: Tree Int -> Int -> Tree Int
masukkanIntKeTree Leaf num = consTree num
masukkanIntKeTree t@(Branch kir val kan) num
  | num < val = Branch (masukkanIntKeTree kir num) val kan
  | num > val = Branch kir val (masukkanIntKeTree kan num)
  | otherwise = t

iTree1, iTree2 :: Tree Int
iTree1 = Leaf
iTree2 = consTree 12

cekElemDiTree :: (Eq a) => a -> Tree a -> Bool
cekElemDiTree _ Leaf = False
cekElemDiTree trgt (Branch kir val kan) =
  cekElemDiTree trgt kir || trgt == val || cekElemDiTree trgt kan

main :: IO ()
main = do
  mapM_
    ( T.putStrLn
        . showStringTree
    )
    [ tree1
    , tree2
    , tree3
    -- , tree4
    -- , tree5
    ]
  masukkanIntKeTree iTree1 15 & print
  masukkanIntKeTree iTree2 15
    & flip masukkanIntKeTree 11
    & flip masukkanIntKeTree 8
    & flip masukkanIntKeTree 18
    & cekElemDiTree 11
    & print
