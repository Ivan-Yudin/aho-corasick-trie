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

import qualified Data.IntMap   as IM
import           Data.IntMap         (IntMap) 
import qualified Data.Map.Lazy as M
import qualified Data.Map.Merge.Lazy as M
import           Data.Map.Lazy       (Map)
import           Data.Maybe_         (Maybe'(..))
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


---------------------------------------------------------------
-- Instances of @MapLike@
-------------------------------------------------------------- 


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
                           
instance (Eq val) => MapLike (IntMap val) Int val  where 
   empty = IM.empty
   null  = IM.null 
   singleton key val = IM.fromList [(key,val)]
   fromList = IM.fromList
   lookup  = IM.lookup 
   mapWithKey = IM.mapWithKey

   merge f = IM.mergeWithKey 
              (\key a b -> f key (Just a) (Just b) )
              ( restrictToDiag $ 
                IM.mergeWithKey (\key a _ -> f key (Just a) Nothing)
                                id id ) 
              ( restrictToDiag $
                IM.mergeWithKey (\key b _ -> f key Nothing  (Just b) )
                                id id ) 
                where 
                  restrictToDiag h x = h x x 
   

instance (Ord key, Eq val) => MapLike ([(key,val)]) key val where

   empty = [] 
   null  = L.null 
   singleton key val = [(key,val)] 
   fromList = id 
   lookup = L.lookup
   mapWithKey h = map ( \(key,val) -> (key, h key val) ) 
   merge f list1 list2 = M.toList $ merge f (M.fromList list1 ) (M.fromList list2) 
   
instance (Eq a ) => MapLike (Maybe' a b) a b where
  empty = Nothing'

  null :: Maybe' a b -> Bool
  null Nothing' = True
  null _        = False

  singleton a b = Just' a b

  fromList [] =         Nothing'
  fromList xx = uncurry Just'   $ last xx

  lookup x (Just' y b) | x == y = Just b
  lookup _ _                    = Nothing

  mapWithKey h (Just' a b) = Just' a (h a b)
  mapWithKey _  _          = Nothing'

  merge hh (Just' a1 b1) (Just' a2 b2)
    | a1 == a2 = case hh a1 (Just b1) (Just b2) of
          Nothing -> Nothing'
          Just b  -> Just' a1 b
    | a1 /= a2 = merge hh (Just' a1 b1) Nothing' -- we have to make a choice here
                                                 -- which key to keep...

  merge hh (Just' a b) Nothing'
    = case hh a (Just b) Nothing of
          Nothing -> Nothing'
          Just b' -> Just' a b'

  merge hh Nothing' (Just' a b)
    = case hh a Nothing (Just b) of
          Nothing -> Nothing'
          Just b' -> Just' a b'

  merge hh Nothing' Nothing' = Nothing'



