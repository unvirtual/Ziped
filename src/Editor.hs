{-# LANGUAGE BangPatterns #-}
module Editor where

import Cursor
import Buffer
import Graphics.Vty
import Data.Maybe
import Control.Monad

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
            | MoveEnd
            | MoveBegin
            | MoveEndLine
            | MoveBeginLine
            | KillToEnd

repeatMaybe :: (a -> Maybe a) -> a -> Maybe a
repeatMaybe f x = maybe (return x) (repeatMaybe f) (f x)

-- TODO: here we need information on display size for PageDown and the like!!
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
actionMap MoveEnd        = Just . moveEnd Fwd
actionMap MoveBegin      = Just . moveEnd Bwd
actionMap MoveEndLine    = Just . fmap (moveEnd Fwd)
actionMap MoveBeginLine  = Just . fmap (moveEnd Bwd)
actionMap KillToEnd      = repeatMaybe $ fmapM (delete Fwd)

specialKeys = M.fromList [ (EvKey KLeft [],   MoveLeft)
                         , (EvKey KRight [],  MoveRight)
                         , (EvKey KDown [],   MoveDown)
                         , (EvKey KUp [],     MoveUp)
                         , (EvKey KDel [],    Delete)
                         , (EvKey KBS [],     DeleteBack)
                         , (EvKey KEnter [],  InsertLine)
                         , (EvKey KEnter [MMeta],  InsertLineTop)
                         , (EvKey (KASCII 'e') [MCtrl],  MoveEndLine)
                         , (EvKey (KASCII 'a') [MCtrl],  MoveBeginLine)
                         , (EvKey (KASCII 'k') [MCtrl],  KillToEnd)
                         , (EvKey (KASCII 'e') [MMeta, MCtrl],  MoveEnd)
                         , (EvKey (KASCII 'a') [MMeta, MCtrl],  MoveBegin)
                         ]


performAction   :: (Monad m) => Event -> BufferST m Bool
performAction k = maybe (return False) (\x -> modifyCursor (actionMap x) >> return True) (translateKey k)

translateKey :: Event -> Maybe Action
translateKey (EvKey (KASCII c) []) = Just $ Insert c
translateKey k                     = M.lookup k specialKeys

