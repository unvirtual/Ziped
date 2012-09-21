module Buffer where

import Cursor
import Data.Maybe
import Control.Monad.State

type Point = (Int, Int)
data Move = MLeft | MRight | MUp | MDown
data Rect = Rect Point Int Int 

-- extend with tags, like truncated lines etc. 
data BufferView = BufferView { viewRect :: Rect
                             , viewVisible :: [String] }
instance Show Rect where
    show (Rect pt w h) = "Rect in " ++ show pt ++ ", width: " ++ show w ++ ", height: " ++ show h
                
instance Show BufferView where
    show (BufferView rect@(Rect pt w h) str) =
                (show rect ) ++ "\n"
             ++ "Content:\n"
             ++ (replicate w '-') ++ "\n"
             ++ (unlines $ str)
             ++ (replicate (h - length str) '\n')
             ++ (replicate w '-') ++ "\n"

data Buffer = Buffer { bufferCursor :: TextCursor
                     , bufferTitle :: String
                     , bufferView :: BufferView
                     } deriving Show

type BufferST m = StateT Buffer m

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start) . drop start

createView :: TextCursor -> Rect -> BufferView
createView crs rect@(Rect (x,y) w h) = BufferView rect lineSlice
    where str  = fromCursor crs
          textSlice = slice y (y+h) str
          lineSlice = map (slice x (x+w)) textSlice
 
buffer :: [String] -> String -> Point -> Int -> Int -> Buffer
buffer str title pt w h = Buffer crs title $ createView crs $ Rect pt w h
    where crs = textCursor str pt
 
bufferCursorPosition :: Buffer -> Point
bufferCursorPosition = cursorPosition . bufferCursor
 
modifyCursor :: (Monad m) => (TextCursor -> TextCursor) -> BufferST m ()
modifyCursor f = do
   st <- get
   let newcrs = f $ bufferCursor st
   let cpos = cursorPosition newcrs
   let rect = viewRect $ bufferView st
   if not $ pointInRect cpos rect
      then do let newrect = getRect rect cpos
              let newView = createView newcrs newrect
              put (st { bufferCursor = newcrs, bufferView = newView } )
      else put (st { bufferCursor = newcrs, bufferView = createView newcrs rect} )
 
resizeBuffer :: (Monad m) => Int -> Int -> BufferST m ()
resizeBuffer w h = do
   st <- get
   let (Rect pt _ _) = viewRect $ bufferView st
   let crs = bufferCursor st
   put $ st { bufferView = createView crs $ Rect pt w h }
   modifyCursor $ textMoveTo $ cursorPosition crs
 
inInterval :: Int -> (Int, Int) -> Bool
inInterval z (x,y) = z >= x && z < y
              
pointInRect :: Point -> Rect -> Bool
pointInRect (x,y) (Rect (rx, ry) w h) = inInterval x (rx, rx + w)  && inInterval y (ry, ry + h)
 
getRect :: Rect -> Point -> Rect
getRect (Rect (rx,ry) w h) (x,y) | x >= w + rx = Rect (x - w + 1, ry) w h
                                 | x < rx      = Rect (x, ry) w h
                                 | y >= h + ry = Rect (rx,  y - h + 1) w h
                                 | y < ry      = Rect (rx, y) w h
                                 | otherwise   = Rect (rx,ry) w h
                                         
resizeRect :: Rect -> Int -> Int -> Rect
resizeRect (Rect p _ _) w h = Rect p w h

