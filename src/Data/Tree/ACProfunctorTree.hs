{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Data.Tree.ACProfunctorTree where 

import Prelude hiding (uncons)

import Control.Applicative ((<|>)) 


import Data.Function (on)
import Data.Maybe (fromMaybe)

import Data.ListLike (nub,sort,uncons,groupBy)
import  Data.Map.Lazy (Map) 
import qualified  Data.Map.Lazy as M (lookup,fromList,empty,mapWithKey,null)
import qualified  Data.List as L (lookup,null)

(<>) :: (Monoid m ) => m -> m -> m
(<>) = mappend

data Maybe' a b = Just' a b | Nothing' deriving (Eq,Show,Read)

class Sieve p f a where 
  sieve ::  p a b -> a -> f b

instance (Eq a) => Sieve Maybe' Maybe a where 
   sieve (Just' a b) x | x == a = Just b 
   sieve  _          _          = Nothing 

instance (Ord a) => Sieve Map Maybe a where 
  sieve = flip M.lookup 


data Tree p out a = Node out                -- the output we produce if we reach the node
                     (Maybe (Tree p out a))    -- failure handling
                     (p a (Tree p out a)) -- continuation from the node driven by input


deriving instance (Show a, Show out) => Show (Tree Maybe' out a) 
deriving instance (Eq   a, Eq   out) => Eq   (Tree Maybe' out a) 

branch  (Node _   _ f) = f
failure (Node _   f _) = f




-- | Given  tree and a string collects outputs produced by each matchLing
-- substring. In the typical use @out~[a]@ and it equals to the matchLing
-- substring. 
match :: (Monoid out, Sieve p Maybe a ) =>
         (Tree p out a) -> [a]  -> out

match _ [] = mempty

match root@(Node out _ branch) xx@(x:xs)   
  = case sieve branch x of 
     Just  t ->  out <> match' root t xs 
     Nothing ->         match  root   xs

match' :: (Monoid out, Sieve p Maybe a ) =>
          (Tree p out a) -> (Tree p out a) -> [a] -> out

match' root (Node out failure branch) [] = out 

match' root (Node out failure branch) xx@(x:xs)
  = case sieve branch x of 
     Just  t                      -> out <> match' root t xs 
     Nothing  | Just t <- failure -> out <> match' root t xx 
              | otherwise         -> out <> match  root   xx  

-- | The following function produces  tree that in conjunction with
-- match permits to find some matchLes in given sequences.  
-- Namely it finds the longest matchLing prefix @p@ of a given string @s@, 
-- outputs all matchLing prefixes of @p@, including @p@, and then resumes
-- with 

prebuild :: (Ord a, Monoid out, Eq out, p ~ Maybe' ) => ([a] -> out) -> [a] -> (Tree p out a) 
prebuild f string = prebuild'  string (f string) 
  where 
    prebuild' :: (Ord a, Monoid out, Eq out, p~ Maybe') =>
              [a] -> out -> (Tree p out a) 
    prebuild' []     out =   Node out    Nothing  Nothing'
    prebuild' (x:xs) out =   Node mempty Nothing (Just'   x $ prebuild' xs out)

type BinOpOn a = a -> a -> a

tie :: (Eq out, Monoid out, Ord a, p~Maybe') => BinOpOn (Tree p out a)


tie     (Node outA _        (Just' a tA)) tBB
    -- tBB --@(Node outB failureB  _ )
                                            = Node  outA
                                                    Nothing
                                                   (Just' a $ pretie' tA  tBB)
                                                       -- ^insert vertex at the top at any case
   where 

    pretie'   (Node outA' _         (Just'  a' tA' )) 
            t@(Node outB' failureB' (Just'  b' tB' ))
              | a' == b'  = Node (         outA' ) failureB' $ Just'  a' $ tie'    tA' tB' 
              |otherwise  = Node (         outA' ) Nothing   $ Just'  a' $ pretie' tA' t 
  
    pretie' s _ = s 

    tie'   (Node outA' _         Nothing'       )
         t@(Node outB' failureB' (Just'  b' tB' ))
                       = Node (outB' <> outA') (Just t) Nothing'  --  failureB' (Just'  b' tB' )

    tie'   (Node outA' _         (Just'  a' tA' )) 
         t@(Node outB' failureB' (Just'  b' tB' ))
           | a' == b'  = Node (outB' <> outA' ) failureB' $ 
                         Just'  a'$ tie' tA' tB' 
           |otherwise  = Node (outB' <> outA' ) (Just t) $
                         Just'  a' $ case h a failureB' of 
                                       Nothing    -> pretie' tA' tBB
                                       (Just tC)  -> tie'    tA' tC
  
   -- h :: (Ord a) => a -> Maybe (Tree out a) -> Maybe (Tree out a )

    h _ Nothing = Nothing 
    h x (Just (Node _ f Nothing'))  = Nothing 
    h x (Just (Node _ f (Just'  y t ))) 
        | x == y    = h x f 
        | otherwise = Just t 

-----------------------------------------------------------------------
--            SOME TESTING STAFF  -------------------------------------
--            TO BE USED IN GHCI  -------------------------------------
-----------------------------------------------------------------------

 
t = prebuild (:[]) "ababcababcabcd" 
w = let s = tie t s in s
label (Node _  _        (Just' a t )) = a
next  (Node _  _        (Just' a t )) = t 
fails (Node _ (Just t )  _          ) = t 
h t k = foldr ($) t $ replicate k next


