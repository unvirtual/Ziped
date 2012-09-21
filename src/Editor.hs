module Editor where

import Cursor
import Buffer
import Graphics.Vty

import qualified Data.Map as M

data Action = MoveLeft
            | MoveRight
            | MoveUp
            | MoveDown
            | Insert Char
            | InsertLine
            | InsertLineTop
            | InsertLineSplit
            | Delete
            | DeleteBack
            | DeleteLine
            | DeleteLineBack

keyAction :: Key -> Action
keyAction = undefined

actionMap :: Action ->  (TextCursor -> Maybe TextCursor)
actionMap MoveLeft       = fmapM (move Bwd)
actionMap MoveRight      = fmapM (move Fwd)
actionMap MoveUp         = move Bwd
actionMap MoveDown       = move Fwd
actionMap (Insert c)     = Just . fmap (insert c Fwd)
actionMap InsertLine     = Just . insert [] Fwd
actionMap InsertLineTop  = Just . insert [] Bwd
actionMap Delete         = fmapM (delete Fwd)
actionMap DeleteBack     = fmapM (delete Bwd)
actionMap DeleteLine     = delete Fwd
actionMap DeleteLineBack = delete Bwd

specialKeys = M.fromList [ (EvKey KLeft [],   MoveLeft)
                         , (EvKey KRight [],  MoveRight)
                         , (EvKey KDown [],   MoveDown)
                         , (EvKey KUp [],     MoveUp)
                         , (EvKey KDel [],    Delete)
                         , (EvKey KBS [],     DeleteBack)
                         , (EvKey KEnter [],  InsertLine)
                         , (EvKey KEnter [MMeta],  InsertLineTop)
                         ]

performAction   :: (Monad m) => Event -> BufferST m Bool
performAction k = maybe (return False) (\x -> modifyCursor (actionMap x) >> return True) (translateKey k)

translateKey :: Event -> Maybe Action
translateKey (EvKey (KASCII c) []) = Just $ Insert c
translateKey k                     = M.lookup k specialKeys

