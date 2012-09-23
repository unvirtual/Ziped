{-# LANGUAGE FlexibleContexts #-}
module Buffer where

import Cursor
import Data.Maybe
import Control.Monad.State
import Control.Applicative
import System.IO

type Point = (Int, Int)
data Move = MLeft | MRight | MUp | MDown
data Rect = Rect { rectOrigin :: Point
                 , rectWidth ::  Int
                 , rectHeight :: Int }

instance Show Rect where
    show (Rect pt w h) = "Rect in " ++ show pt ++ ", width: " ++ show w ++ ", height: " ++ show h
                
data Buffer = Buffer { bufferCursor :: TextCursor
                     , bufferTitle :: String
                     , viewRect :: Rect
                     } deriving Show

type BufferST m = StateT Buffer m

type BufferView = [String]

emptyBuffer :: Buffer
emptyBuffer = buffer [[]] "" (0,0) 0 0

loadFile :: FilePath -> IO Buffer
loadFile path = do handle <- openFile path ReadMode 
                   contents <- hGetContents handle
                   let crs = textCursor (lines contents) (0,0)
                   return $ (\buf -> buf { bufferCursor = crs
                                         , bufferTitle = path }) emptyBuffer
                   
bufCursorPos :: Buffer -> Point
bufCursorPos = cursorPosition . bufferCursor

bufRelCursorPos :: Buffer -> (Int, Int)
bufRelCursorPos buf = (x - x0, y - y0)
    where (x,y) = cursorPosition $ bufferCursor buf
          (x0, y0) = rectOrigin $ viewRect buf

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start) . drop start

visibleC :: Buffer -> BufferView
visibleC buf = map (slice x (x+w)) $ slice y (y+h) $ fromCursor $ bufferCursor buf
    where (Rect (x,y) w h) = viewRect buf
 
buffer :: [String] -> String -> Point -> Int -> Int -> Buffer
buffer str title pt w h = Buffer { bufferCursor = textCursor str pt
                                 , bufferTitle  = title
                                 , viewRect     = Rect pt w h
                                 }
 
modifyCursor :: (Monad m) => (TextCursor -> Maybe TextCursor) -> BufferST m Bool
modifyCursor f = get >>= maybe (return False) (flip (>>) (return True) . setCursor) . f . bufferCursor
    where setCursor crs = modify (\x -> x { bufferCursor = crs }) >> updateViewRect

updateViewRect :: (Monad m) => BufferST m ()
updateViewRect = modify (\x -> x { viewRect = updateRect (bufCursorPos x) (viewRect x) })

resizeBuffer :: (Monad m) => Int -> Int -> BufferST m ()
resizeBuffer w h = do modify (\x -> x { viewRect = resizeRect (viewRect x) w h })
                      updateViewRect

inInterval :: Int -> (Int, Int) -> Bool
inInterval z (x,y) = z >= x && z < y
              
pointInRect :: Point -> Rect -> Bool
pointInRect (x,y) (Rect (rx, ry) w h) = inInterval x (rx, rx + w)  && inInterval y (ry, ry + h)
 
getRect :: Point -> Rect -> Rect
getRect (x,y) (Rect (rx,ry) w h) | x >= w + rx = Rect (x - w + 1, ry) w h
                                 | x < rx      = Rect (x, ry) w h
                                 | y >= h + ry = Rect (rx,  y - h + 1) w h
                                 | y < ry      = Rect (rx, y) w h
                                 | otherwise   = Rect (rx,ry) w h
                                         
updateRect :: Point -> Rect -> Rect
updateRect pt rect = if (not $ pointInRect pt rect) then getRect pt rect else rect
                                                 
resizeRect :: Rect -> Int -> Int -> Rect
resizeRect (Rect p _ _) w h = Rect p w h

