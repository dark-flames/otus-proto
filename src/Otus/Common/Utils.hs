module Otus.Common.Utils (
  enumurate,
  mapTuple,
  mapFst,
  mapSnd,
  uncurry3,
) where

enumurate :: [a] -> [(Int, a)]
enumurate = zip [0 ..]

mapTuple :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapTuple f g (a, b) = (f a, g b)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f = mapTuple f id

mapSnd :: (b -> d) -> (a, b) -> (a, d)
mapSnd = mapTuple id

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
