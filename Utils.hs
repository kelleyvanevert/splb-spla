
module Utils where

import Debug.Trace



mapI :: (Int -> a -> b) -> [a] -> [b]
mapI f list =
  map (uncurry f) $ zip [1 .. (length list)] list

mapZI :: (Int -> a -> b) -> [a] -> [b]
mapZI f list =
  map (uncurry f) $ zip [0 .. (length list - 1)] list

foldlZI :: (Int -> a -> b -> a) -> a -> [b] -> a
foldlZI f init list =
  foldl (\m (i, b) -> f i m b) init $ zip [0 .. (length list - 1)] list


traceM :: Monad m => String -> m ()
traceM msg = trace msg (return ())

traceShowM :: (Show a, Monad m) => a -> m ()
traceShowM = traceM . show

traceShowId :: Show a => a -> a
traceShowId x = trace (show x) x
