{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
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
  (empy, null, fromList,lookup,mapWithKey)  where

import qualified Data.Map.Lazy as M
import           Data.Map.Lazy       (Map)
import qualified Data.List     as L

class MapLike full key val | full -> key, full -> val where
  
  empty :: full
  null  :: full -> Bool 
  fromList :: [(key,value)] -> full 
  lookup   :: key -> full -> Maybe val
  mapWithKey :: (key -> val -> val) -> full -> full 

instance MapLike (Map key val) key val where
   empty = M.empty
   null  = M.null 
   formList = M.fromList
   lookuop  = M. lookup 
   mapWithKey = M.mapWithKey

instance MapLike ([(key,val)]) key val where

   empty = [] 
   null  = L.null 
   fromList = id 
   lookup = L.lookup
   mapWithKey h = map ( \(key,val) -> (key, h key val) ) 

