module Hadoo.StateLanes where

data State = Todo | Started | Done deriving (Show, Read, Eq, Ord, Enum, Bounded)