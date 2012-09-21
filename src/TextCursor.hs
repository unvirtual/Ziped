{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Cursor where

import Data.Maybe
import Control.Monad

data Cursor a b = Cursor { cursor :: (RList a, b, FList a)
                         , index :: Int } deriving Show
data Dir = Bwd | Fwd

class CCursor a b  where
    toCursor :: [a] -> Cursor a b
    fromCursor :: Cursor a b -> [a]
    move :: Dir -> Cursor a b -> Maybe (Cursor a b)
    insert :: a -> Dir -> Cursor a b -> Cursor a b
    delete :: Dir -> Cursor a b -> Maybe (Cursor a b)

    current :: Cursor a b -> b
    current (Cursor (xs, c, ys) i) = c

moveTo :: (CCursor a b) => Int -> Dir -> Cursor a b -> Maybe (Cursor a b)
moveTo 0 _ crs = Just crs
moveTo n dir crs = move dir crs >>= moveTo (n - 1) dir

moveToOrEnd :: (CCursor a b) => Int -> Cursor a b -> Cursor a b
moveToOrEnd n crs = go n (moveEnd Bwd crs)
     where go 0 crs = crs
           go n crs = maybe crs (go (n - 1)) (move Fwd crs)

moveEnd :: (CCursor a b) => Dir -> Cursor a b -> Cursor a b
moveEnd dir crs = maybe crs (moveEnd dir) (move dir crs)

doOrEnd :: (CCursor a b) => (Cursor a b -> Maybe (Cursor a b)) -> Cursor a b -> Cursor a b
doOrEnd f crs = fromMaybe crs (return crs >>= f)

instance Functor (Cursor a) where
    fmap f (Cursor (xs, lc, ys) i) = Cursor (xs, f lc, ys) i

data Mark = Mark deriving Show
instance CCursor Char Mark where
    toCursor str = Cursor (RTip, Mark, flist str) 0
    fromCursor (Cursor (xs, _, ys) _) = rtolist xs ++ ftolist ys

    move Fwd (Cursor (xs, c, y :->: ys) i) = Just $ Cursor (xs :<-: y, c, ys) (i+1)
    move Fwd _  = Nothing
    move Bwd (Cursor (xs :<-: x, c, ys) i) = Just $ Cursor (xs, c, x :->: ys) (i-1)
    move Bwd _  = Nothing

    insert char Fwd (Cursor (xs, c, ys) i) = Cursor (xs :<-: char, c, ys) (i + 1)
    insert char Bwd (Cursor (xs, c, ys) i) = Cursor (xs, c, char :->: ys) i

    delete Fwd (Cursor (xs, c, y :->: ys) i) = Just $ Cursor (xs, c, ys) i
    delete Fwd _ = Nothing
    delete Bwd (Cursor (xs :<-: x, c, ys) i) = Just $ Cursor (xs, c, ys) (i - 1)
    delete Bwd _ = Nothing

instance CCursor String (Cursor Char Mark) where
    toCursor (x:xs) = Cursor (RTip, toCursor x, flist xs) 0
    fromCursor (Cursor (xs, lc, ys) _) = rtolist xs ++ [fromCursor lc] ++ ftolist ys
    
    move Fwd (Cursor (xs, lc, y :->: ys) i) = Just $ Cursor (xs :<-: fromCursor lc, moveToOrEnd (index lc) $ toCursor y, ys) (i+1)
    move Fwd _ = Nothing
    move Bwd (Cursor (xs :<-: x, lc, ys) i) = Just $ Cursor (xs, moveToOrEnd (index lc) $ toCursor x, fromCursor lc :->: ys) (i-1)
    move Bwd _ = Nothing
    
    insert str n (Cursor (xs, lc, ys) i) = Cursor (xs :<-: fromCursor lc, toCursor str, ys) (i + 1)
    
    delete Fwd (Cursor (xs, c, y :->: ys) i) = Just $ Cursor (xs, c, ys) i
    delete Fwd _ = Nothing
    delete Bwd (Cursor (xs :<-: x, c, ys) i) = Just $ Cursor (xs, c, ys) (i - 1)
    delete Bwd _ = Nothing


type LineCursor = Cursor Char Mark
type TextCursor = Cursor String LineCursor
                   
lineCursor :: String -> Int -> LineCursor
lineCursor str n = moveToOrEnd n $ toCursor str 

textCursor :: [String] -> (Int, Int) -> TextCursor
textCursor xs pos = textMoveTo pos $ toCursor xs

textMoveTo :: (Int, Int) -> TextCursor -> TextCursor
textMoveTo (x,y) crs = fmap (moveToOrEnd x . moveEnd Bwd) selectedLine
    where selectedLine = moveToOrEnd y (moveEnd Bwd crs)
       
cursorPosition :: TextCursor -> (Int, Int)
cursorPosition crs = undefined
 
currentLine :: TextCursor -> LineCursor
currentLine = current
 
nextLine :: Dir -> TextCursor -> TextCursor
nextLine dir = doOrEnd (move dir)
      
nextChar :: Dir -> TextCursor -> TextCursor
nextChar dir = fmap (doOrEnd $ move dir)
  
insertChar :: Char -> Dir -> TextCursor -> TextCursor
insertChar char dir = fmap (insert char dir)
   
deleteChar :: Dir -> TextCursor -> TextCursor
deleteChar dir = fmap (doOrEnd $ delete dir)
   
endLine :: Dir -> TextCursor -> TextCursor
endLine dir  = moveEnd dir
 
insertLine :: Dir -> TextCursor -> TextCursor
insertLine Fwd = insert [] Fwd 
insertLine Bwd = insert [] Fwd . nextLine Bwd
 
yankLine :: Dir -> String -> TextCursor -> TextCursor
yankLine Fwd str = insert str Fwd 
yankLine Bwd str = insert str Fwd . nextLine Bwd
  
deleteLine :: Dir -> TextCursor -> TextCursor
deleteLine = doOrEnd . delete
  
endText :: Dir -> TextCursor -> TextCursor
endText = moveEnd

-- Forward and backward lists with conversions
data RList a = RTip | RList a :<-: a deriving (Show, Eq)
data FList a = FTip | a :->: FList a deriving (Show, Eq)

instance Functor RList where
    fmap f RTip = RTip
    fmap f (xs :<-: x) = fmap f xs :<-: f x

instance Functor FList where
    fmap f FTip = FTip
    fmap f (x :->: xs) = f x :->: fmap f xs

flist :: [a] -> FList a
flist = foldr (:->:) FTip 

rlist :: [a] -> RList a
rlist = foldl (:<-:) RTip 

flength :: FList a -> Int
flength FTip = 0
flength (x :->: xs) = 1 + flength xs
        
rlength :: RList a -> Int
rlength RTip = 0
rlength (xs :<-: x) = 1 + rlength xs
        
ftolist :: FList a -> [a]
ftolist FTip = []
ftolist (x :->: y) = x:ftolist y
 
rtolist :: RList a -> [a]
rtolist RTip = []
rtolist (x :<-: y) = rtolist x ++ [y]

