{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{- 
Module : 
Copyright : Ivan Yudin 
License : BSD-3 
-}

module Data.Tree.AhoCorasickTrie where

import            Prelude                   hiding (null,uncons,lookup)
import            Control.Applicative              ((<|>),liftA2,(<*>),pure)
import            Data.Default.Class               (Default,def)
import            Data.Function                    (on)
import            Data.Maybe                       (fromMaybe)
import            Data.Maybe_                      (Maybe'(..))               
import            Data.ListLike                    (nub,sort,uncons,groupBy)
import qualified  Data.MapLike        as ML
import            Data.MapLike                     (MapLike)
import            Data.Map.Lazy                    (Map)
import            Data.IntMap                      (IntMap)
import qualified  Data.Map.Lazy       as M         (keys,intersectionWith,lookup,fromList,toList,empty,mapWithKey,null)
import qualified  Data.List           as L         (lookup,null)

-- | The next data type is used to model epsilon-trees. 
--
-- These trees are graphs with two type of arrows: solid arrows and dotted arrows.
-- The subgraph formed by solid arrows is a tree. 
-- The dotted arrows represent backreferences and they go up in the sense
-- that the target of a dotted arrow has smaller depth than its source. 
--
-- The solid arrows are labeled by the elements of the alphabet @a@.
--
--
data Tree f out a =
     Node out                   -- the output to produce if the node is reached
         (Maybe (Tree f out a)) -- backtracking if there is no continuation
         (f     (Tree f out a)) -- continuation from the node driven by the input


deriving instance (Eq   a, Eq   out) => Eq   (Tree  IntMap     out a)
deriving instance (Show a, Show out) => Show (Tree  IntMap     out a)

deriving instance (Eq   a, Eq   out) => Eq   (Tree (Map    a)  out a)
deriving instance (Eq   a, Eq   out) => Eq   (Tree (Maybe' a)  out a)
deriving instance (Show a, Show out) => Show (Tree (Map    a)  out a)
deriving instance (Show a, Show out) => Show (Tree (Maybe' a)  out a)




-- | Given  tree and a string collects outputs produced by each matching
-- substring. In the typical use @out~[a]@ and it equals to the matching
-- substring.
match :: (Monoid out, t ~ Tree f out a, MapLike (f t) a t ) =>
         t  -> [a]  -> out

-- Corner case when the input is empty
match _ [] = mempty

-- Initialization of the matching algorithm. 
-- If we cannot move out of the root, we just discard the 
-- input symbol. 
-- If we can move out of the root we pass the execution to @match'@. 
-- Normally @out@ at the root node is @mempty@. 
match root@(Node out _ branches) (x:xs)
  = case ML.lookup x branches of
     Just  branch  ->  out <> match' root branch xs
     Nothing       ->         match  root        xs

match' :: (Monoid out, t ~ Tree f out a, MapLike (f t) a t ) =>
          t -> t  -> [a] -> out

match' root (Node out failure branch) []
  = case failure of 
      Nothing  -> out
      (Just t) -> out <> match' root t [] 

match' root (Node out failure branch) xx@(x:xs)
  = case ML.lookup x branch  of
     Just  t                       -> out <> collectOut failure <> match' root t xs
     Nothing  | Just t  <- failure -> out <>                       match' root t xx
              | Nothing <- failure -> out <>                       match  root   xx

collectOut :: (Monoid out, t ~ Tree f out a, MapLike (f t) a t ) =>
              Maybe t -> out
collectOut Nothing = mempty
collectOut (Just (Node out failure _)  )  = out <> collectOut failure
 

-- | The following function produces  tree that in conjunction with
-- match permits to find some matches in given sequences.
-- Namely it finds the longest matching prefix @p@ of a given string @s@,
-- outputs all matching prefixes of @p@, including @p@, and then resumes
-- with

prebuild1 :: (Ord a, Monoid out, Eq out,  t ~ Tree f out a
             , MapLike (f t) a t ) =>
            ([a] -> out) -> [a] -> (Tree f out a)

prebuild1 f string = prebuild'  string (f string)
  where
    prebuild' :: (Ord a, Monoid out, Eq out, t~ Tree f out a
                 , MapLike (f t) a t) =>
                  [a] -> out -> t

    prebuild' []     out =   Node out    Nothing   ML.empty
    prebuild' (x:xs) out =   Node mempty Nothing $ ML.singleton x
                                                 $ prebuild' xs out


prebuild :: (Ord a, Monoid out, Eq out, t ~ Tree f out a
            , MapLike (f t) a t) =>
            ([a] -> out) -> [[a]] -> t
prebuild f strings =
   let strings' = nub $ sort strings
   in  prebuild' $ zip strings' (map f strings')
  where
--    prebuild' :: (Ord a, Monoid out, Eq out, t ~ Tree p out a) =>
--                 [( [a] , out )]  -> t

    prebuild' [] = Node mempty Nothing ML.empty
    prebuild' pairs@( (x,out):pairs')
      | L.null x  =Node out    Nothing (createMap pairs')
      | otherwise =Node mempty Nothing (createMap pairs )
    createMap pairs = ML.fromList $
              createKeyValuePair  <$>
              groupBy ( (==) `on` head.fst) pairs

--    createKeyValuePair ::(Ord a, Monoid out, Eq out) =>
 --                        [ ([a], out ) ] -> (a , Tree p out a  )

    createKeyValuePair pairs@( ( a:_ , _ ) : _)
      = ( a, prebuild' $ tailOnFst <$> pairs)

    tailOnFst :: ([a],out) -> ([a], out)
    tailOnFst (x,out) = (tail x , out)




type BinOpOn a = a -> a -> a



tie :: (Eq out, Monoid out, Ord a, t ~ Tree f out a
       , MapLike (f t) a t, Functor f ) => BinOpOn t

tie  (Node outA _ continuation) rootB
    = Node outA Nothing $ fmap (prezip rootB) continuation

   where
    prezip t@(Node _     _ continuationB')
             (Node outA' _ continuationA')
      = Node outA' Nothing $ flip ML.mapWithKey continuationA'
        ( \a' -> case ML.lookup a' continuationB' of
                   Just tB' -> zipping tB'
                   Nothing  -> prezip  t )


    zipping t@(Node _     _ continuationB')
              (Node outA' _ continuationA')
      = Node outA' (Just t)  continuationC
        where
          continuationC = ML.merge hh continuationA' continuationB'

          hh _ (Just tA') (Just tB') = Just    $ zipping tB'   tA'
          hh _ (Just tA')  Nothing   = Just    $ prezip  rootB tA'
          hh _ _           _         = Nothing  



-----------------------------------------------------------------------
--            SOME TESTING STAFF  -------------------------------------
--            TO BE USED IN GHCI  -------------------------------------
-----------------------------------------------------------------------


t = prebuild1 (:[]) "ababcababcabcd" :: Tree (Map Char) [String] Char
tt = prebuild (:[]) ["abc","acd","cc"] :: Tree (Map Char) [String] Char
w = let s = tie t s in s
ww = tie tt ww
label (Node _  _        (Just' a t )) = a
labels (Node _  _        continuation) = M.keys continuation
next  (Node _  _        (Just' a t )) = t
nexts  (Node _  _         continuation) = map snd $ M.toList continuation
fails (Node _ (Just t )  _          ) = t
fails (Node _ Nothing _ )            = Node mempty Nothing ML.empty
h t k = foldr ($) t $ replicate k next
h' t k = foldr (=<<) [t] $ replicate k nexts


