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
  show_cursor (terminal vty)
  buf <- loadFile "/home/tom/dev/ziped/src/Buffer.hs"
  runStateT (loop vty) buf
            
loop vty = do bounds <- lift $ display_bounds (terminal vty)
              resizeBuffer (fromIntegral $ region_width bounds) ((fromIntegral $ region_height bounds) - 1)
              buf <- get
              lift $ update vty $ pic_for_image $ display  buf $ fromIntegral $ region_height bounds
              -- setCursor must come after update, otherwise cursor will be hidden!
              setCursorPos vty
              k <- lift $ next_event vty
              handleKey k vty
              loop vty

handleKey (EvResize w h)  vty = resizeBuffer w h >> return True
handleKey (EvKey KEsc []) vty = lift $ shutdown vty >> return True
handleKey (k@(EvKey c m)) vty = performAction k
handleKey _               vty = return False

display buf windowLines = text <-> emptyLines <-> status
    where text = foldr1 (<->) $ map (\x -> string current_attr (f x)) visLines
          visLines = visibleC buf
          emptyLines = foldr (<->) empty_image $ map (string current_attr . f) $ replicate nEmptyLines " "
          nEmptyLines = if (length visLines < windowLines) then (windowLines - (length visLines) - 1) else 0
          cpos = cursorPosition $ bufferCursor buf
          status = string (with_back_color current_attr blue) ("   cursor: " ++ show cpos ++ "  " ++ bufferTitle buf)
          f [] = " "
          f x = x

setCursorPos vty  = do b <- get
                       lift $ do show_cursor (terminal vty)
                                 let (dx, dy) = bufRelCursorPos b
                                 set_cursor_pos (terminal vty) (fromIntegral dx) (fromIntegral dy)
                                
