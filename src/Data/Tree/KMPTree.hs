{-# LANGUAGE FlexibleContexts #-}

module Data.Tree.KMPTree where 

import Prelude hiding (uncons)

import Control.Applicative ((<|>)) 

import Data.Function (on)
import Data.Maybe (fromMaybe)

import Data.ListLike (nub,sort,uncons,groupBy)
import  Data.Map.Lazy (Map) 
import qualified  Data.Map.Lazy as M (lookup,fromList,empty,mapWithKey,null)
import qualified  Data.List as L (lookup,null)

import Debug.Hood.Observe

(<>) :: (Monoid m ) => m -> m -> m
(<>) = mappend


data KMPTree out a = KMPNode out                -- the output we produce if we reach the node
                     (Maybe (KMPTree out a))    -- failure handling
                     (Maybe (a, KMPTree out a)) -- continuation from the node driven by input
                    deriving (Show, Eq)

outKMP     (KMPNode out _ _) = out
branchKMP  (KMPNode _   _ f) = f
failureKMP (KMPNode _   f _) = f


instance (Observable out, Observable a) => Observable ( KMPTree out a) where
  observer (KMPNode out failure branch ) = send "KMPNode" (return (KMPNode) << out << failure << branch) 


-- | Given KMP tree and a string collects outputs produced by each matchLing
-- substring. In the typical use @out~[a]@ and it equals to the matchLing
-- substring. 
matchK :: (Monoid out, Ord a) => (KMPTree out a) -> [a]  -> out
matchK tree string = matchK' tree tree string 

matchK' :: (Monoid out, Ord a) =>  (KMPTree out a) -> (KMPTree out a) -> [a] -> out
matchK' root (KMPNode out failure branch) []        =  out 
matchK' root (KMPNode out failure branch) xx@(x:xs)
  | (Just (y,t)) <- branch, y == x =  out <> matchK' root t    xs 
  | (Just t)     <- failure        =  out <> matchK' root t    xx 
  |  otherwise                     =  out <> matchK' root root xs  

-- | The following function produces KMP tree that in conjunction with
-- matchK permits to find some matchLes in given sequences.  
-- Namely it finds the longest matchLing prefix @p@ of a given string @s@, 
-- outputs all matchLing prefixes of @p@, including @p@, and then resumes
-- with 

prebuildK :: (Ord a, Monoid out, Eq out) => ([a] -> out) -> [a] -> (KMPTree out a) 
prebuildK f string = prebuildK'  string (f string) 
  where 
    prebuildK' :: (Ord a, Monoid out, Eq out) => [a] -> out -> (KMPTree out a) 
    prebuildK' []    out = KMPNode out    Nothing  Nothing 
    prebuildK' (x:xs) out = KMPNode mempty Nothing (Just $ (x,prebuildK' xs out) )

type BinOpOn a = a -> a -> a

tie :: (Eq out, Monoid out, Ord a) => BinOpOn (KMPTree out a)


tie (KMPNode out _ (Just (a,tA))) tB = KMPNode  out
                                              Nothing
                                             (Just (a, tie' tA tB ))
                                                 -- ^insert vertex at the top at any case
   where 
  --  tie' :: BinOpOn (KMPTree out a) 

    tie'   (KMPNode outA _         Nothing       )
         t@(KMPNode outB failureB (Just (b',tB')))
                       = KMPNode (outB <> outA) failureB (Just (b',tB'))

    tie'   (KMPNode outA _        (Just (a',tA'))) 
         t@(KMPNode outB failureB (Just (b',tB')))
           | a' == b'  = KMPNode (outB <> outA ) failureB $ 
                         Just ( a', tie' tA' tB') 
           |otherwise  = KMPNode (outB <> outA ) (Just t) $
                         Just ( a' , tie' tA' $ fromMaybe tB $ h a failureB) 
  
   -- h :: (Ord a) => a -> Maybe (KMPTree out a) -> Maybe (KMPTree out a )

    h _ Nothing = Nothing 
    h x (Just (KMPNode _ f Nothing))  = Nothing 
    h x (Just (KMPNode _ f (Just (y,t)))) 
        | x == y    = h x f 
        | otherwise = Just t 
 
