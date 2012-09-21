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

actionMap :: Action ->  (TextCursor -> TextCursor)
actionMap MoveLeft       = nextChar Bwd
actionMap MoveRight      = nextChar Fwd
actionMap MoveUp         = nextLine Bwd
actionMap MoveDown       = nextLine Fwd
actionMap (Insert c)     = insertChar c Fwd
actionMap InsertLine     = insertLine Fwd
actionMap InsertLineTop  = insertLine Bwd
actionMap Delete         = deleteChar Fwd
actionMap DeleteBack     = deleteChar Bwd
actionMap DeleteLine     = deleteLine Fwd
actionMap DeleteLineBack = deleteLine Bwd

specialKeys = M.fromList [ (KLeft,   MoveLeft)
                         , (KRight,  MoveRight)
                         , (KDown,   MoveDown)
                         , (KUp,     MoveUp)
                         , (KDel,    Delete)
                         , (KBS,     DeleteBack)
                         , (KEnter,  InsertLine)
                         ]

performAction :: (Monad m) => Key -> BufferST m ()
performAction k = maybe (return ()) (modifyCursor . actionMap) (translateKey k)

translateKey :: Key -> Maybe Action
translateKey (KASCII c) = Just $ Insert c
translateKey k          = M.lookup k specialKeys

