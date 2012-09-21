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
              let (x,y) = cursorPosition $ bufferCursor buf
              lift $ set_cursor_pos (terminal vty) (fromIntegral x) (fromIntegral y)
              lift $ show_cursor (terminal vty)
              k <- lift $ next_event vty
              case k of
                  EvResize w h -> resizeBuffer w h >> loop vty
                  EvKey KEsc [] -> lift $ shutdown vty 
                  k@(EvKey c m) -> do performAction k
                                      b <- get
                                      let (x,y) = cursorPosition $ bufferCursor b
                                      lift $ set_cursor_pos (terminal vty) (fromIntegral x + 1) (fromIntegral y)
                                      lift $ show_cursor (terminal vty)
                                      loop vty
                  _ -> loop vty

display buf = foldr1 (<->) $ map (\x -> string current_attr (f x)) buf
    where f [] = " "
          f x = x

