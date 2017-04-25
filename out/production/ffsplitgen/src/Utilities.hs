module Utilities where

import Control.Monad
import Text.Read

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs (tail xs)

listReadM :: (Read a) => [String] -> Maybe [a]
listReadM = mapM readMaybe

-- Take a function from list of a to b and a list of a
-- and return either mzero or the lifted result of applying
-- the function
zeroMap ::(MonadPlus m) => ([a] -> b) -> [a] -> m b
zeroMap f [] = mzero
zeroMap f b = return . f $ b
