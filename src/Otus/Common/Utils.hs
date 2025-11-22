module Otus.Common.Utils (
  enumurate,
  mapTuple,
  mapFst,
  mapSnd,
) where

enumurate :: [a] -> [(Int, a)]
enumurate = zip [0 ..]

mapTuple :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapTuple f g (a, b) = (f a, g b)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f = mapTuple f id

mapSnd :: (b -> d) -> (a, b) -> (a, d)
mapSnd = mapTuple id
