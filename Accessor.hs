{-# OPTIONS_GHC -Wall -O2
 #-}

module Accessor(Accessor,accessor,convertor
               -- getter/setter
               ,(^.),(^:)
               -- composition
               ,(<^),(^>)
               -- Accessors
               ,self,null,reader
               ,write
               -- Tuple
               ,afirst,asecond
               -- List
               ,anth
               -- Data.Map
               ,aMapValue
               ,aMapValueDefault) where

import Control.Arrow(first, second)
import List(nth)
import qualified Data.Map as Map
import Data.Map(Map)

data Accessor whole part =
    Accessor
    {
      accessorGet :: whole -> part
    , accessorSet :: part -> whole -> whole
    }

accessor :: (whole -> part) ->
            (part -> whole -> whole) ->
            Accessor whole part
accessor = Accessor

-- If you can create a whole from a part, then its really a convertor:
convertor :: (whole -> part) -> (part -> whole) ->
             Accessor whole part
convertor extract build = accessor extract (const . build)

self :: Accessor a a
self = accessor id const

reader :: r -> Accessor a r
reader x = accessor (const x) (const id)

write :: Accessor whole part -> part -> whole -> whole
write = accessorSet

(^.) :: whole -> Accessor whole part -> part
(^.) = flip accessorGet

(^:) :: Accessor whole part -> (part -> part) -> whole -> whole
(acc ^: modifyPart) whole = accessorSet acc
                            (modifyPart (whole ^. acc)) whole

(^>) :: Accessor a b -> Accessor b c -> Accessor a c
x ^> y = accessor (accessorGet y . accessorGet x)
                  ((x ^:) . accessorSet y)

(<^) :: Accessor b c -> Accessor a b -> Accessor a c
(<^) = flip (^>)

afirst :: Accessor (a, b) a
afirst = accessor fst (first . const)
asecond :: Accessor (a, b) b
asecond = accessor snd (second . const)

anth :: Int -> Accessor [a] a
anth n = accessor (!!n) (nth n . const)

aMapValue :: Ord k => k -> Accessor (Map k a) a
aMapValue key = accessor (Map.! key) setValue
    where
      setValue value = Map.adjust (const value) key

aMapValueDefault :: Ord k => a -> k -> Accessor (Map k a) a
aMapValueDefault def key = accessor (Map.findWithDefault def key) (Map.insert key)
