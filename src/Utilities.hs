module Utilities where

pairs [] = []
pairs xs = zip xs (tail xs)