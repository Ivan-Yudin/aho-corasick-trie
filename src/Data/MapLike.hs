{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

{-
Copyright (C) 2019 Ivan Yudin <yudinivan@yahoo.de> 

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

{- |
   Module     : Data.MapLike
   Copyright  : Copyright (C) 2019 Ivan Yudin
   License    : BSD3

   Maintainer : Ivan Yudin <yudinivan@yahoo.de>
   Stability  : experimental 

Generic operations over map-like structures

-}

module Data.MapLike
  (MapLike , merge, empty, null,singleton, fromList,lookup,mapWithKey)  where

import Prelude hiding (lookup, null) 

import qualified Data.Map.Lazy as M
import qualified Data.Map.Merge.Lazy as M
import           Data.Map.Lazy       (Map)
import qualified Data.List     as L

class MapLike full key val | full -> key, full -> val where
  
  empty :: full
  null  :: full -> Bool 
  singleton :: key -> val -> full 
  fromList :: [(key,val)] -> full 
  lookup   :: key -> full -> Maybe val
  mapWithKey :: (key -> val -> val) -> full -> full 
  merge :: (key -> Maybe val -> Maybe val -> Maybe val ) -> 
              full -> full -> full 

instance (Ord key, Eq val) => MapLike (Map key val) key val where
   empty = M.empty
   null  = M.null 
   singleton key val = M.fromList [(key,val)]
   fromList = M.fromList
   lookup  = M.lookup 
   mapWithKey = M.mapWithKey

   merge f = M.merge  
              (M.mapMaybeMissing $ (\key val1 -> f key (Just val1) Nothing))
              (M.mapMaybeMissing $ (\key val2 -> f key Nothing (Just val2)))
              (M.zipWithMaybeMatched (\key val1 val2 -> f key (Just val1) (Just val2)))
                           

instance (Ord key, Eq val) => MapLike ([(key,val)]) key val where

   empty = [] 
   null  = L.null 
   singleton key val = [(key,val)] 
   fromList = id 
   lookup = L.lookup
   mapWithKey h = map ( \(key,val) -> (key, h key val) ) 
   merge f list1 list2 = M.toList $ merge f (M.fromList list1 ) (M.fromList list2) 
   

