module Main where

import Buffer
import Editor
import Cursor
import Graphics.Vty
import Control.Monad.State


main = do
  vty <- mkVty
  reserve_display (terminal vty)
  show_cursor (terminal vty)
  bounds <- display_bounds (terminal vty)
  let buf = buffer ["Welcome!","", "This is an amazing editor"] "Welcome" (0,0) (fromIntegral $ region_width bounds) (fromIntegral $ region_height bounds)
  show_cursor (terminal vty)
  runStateT (loop vty) buf
            
loop vty = do buf <- get
              lift $ update vty $ pic_for_image (display $ viewVisible $ bufferView buf) 
              -- setCursor must come after update, otherwise cursor will be hidden!
              setCursor vty
              k <- lift $ next_event vty
              case k of
                  EvResize w h  -> resizeBuffer w h
                  EvKey KEsc [] -> lift $ shutdown vty >> return True
                  k@(EvKey c m) -> performAction k
                  _             -> return False
              loop vty

display buf = foldr1 (<->) $ map (\x -> string current_attr (f x)) buf
    where f [] = " "
          f x = x

setCursor vty  = do b <- get
                    let (x,y) = cursorPosition $ bufferCursor b
                    lift $ do show_cursor (terminal vty)
                              set_cursor_pos (terminal vty) (fromIntegral x) (fromIntegral y)
                                
