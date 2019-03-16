{-# LANGUAGE FlexibleContexts #-}

module Data.Tree.ACTreeL where 

import Prelude hiding (uncons)

import Control.Applicative ((<|>)) 

import Data.Function
import Data.ListLike (nub,sort,uncons,groupBy)
import  Data.Map.Lazy (Map) 
import qualified  Data.Map.Lazy as M (lookup,fromList,empty,mapWithKey,null)
import qualified  Data.List as L (lookup,null)

import Debug.Hood.Observe

(<>) :: (Monoid m ) => m -> m -> m
(<>) = mappend


data LACTree out a = LACNode out         -- the output we produce if we reach the node
                     (Maybe (LACTree out a)) -- failure handling
                     [ (a, LACTree out a)] -- continuations from the node driven by input
                    deriving (Show, Eq)

outLAC     (LACNode out _ _) = out
forestLAC  (LACNode _   _ f) = f
failureLAC (LACNode _   f _) = f

--instance (Observable a) => Observable [a] where
--  observer (a:as) = send ":"  (return (:) << a << as)
--  observer []     = send "[]" (return [])

instance (Observable out, Observable a) => Observable ( LACTree out a) where
  observer (LACNode out failure forest ) = send "LACNode" (return (LACNode) << out << failure << forest) 


-- | Given A-C tree and a string collects outputs produced by each matchLing
-- substring. In the typical use @out~[a]@ and it equals to the matchLing
-- substring. 
matchL :: (Monoid out, Ord a) => (LACTree out a) -> [a]  -> out
matchL tree string = matchL' tree tree string 

matchL' :: (Monoid out, Ord a) =>  (LACTree out a) -> (LACTree out a) -> [a] -> out
matchL' root (LACNode out failure forest) []        =  out 
matchL' root (LACNode out failure forest) xx@(x:xs) = 
  case L.lookup x forest of 
    (Just  t)            -> out <> matchL' root t    xs
    (Nothing)            ->
       case failure of 
        (Just  t)        -> out <> matchL' root t    xx
        (Nothing)        -> out <> matchL' root root xs

-- | The following function produces A-C tree that in conjunction with
-- matchL permits to find some matchLes in given sequences.  
-- Namely it finds the longest matchLing prefix @p@ of a given string @s@, 
-- outputs all matchLing prefixes of @p@, including @p@, and then resumes
-- with 

prebuildL :: (Ord a, Monoid out, Eq out) => ([a] -> out) -> [[a]] -> (LACTree out a) 
prebuildL f strings = prebuildL' $ zip strings' (map f strings') 
  where 
    strings' = nub $ sort strings 
    prebuildL' :: (Ord a, Monoid out, Eq out) =>[( [a] , out )]  -> (LACTree out a) 
    prebuildL' [] =LACNode mempty Nothing []
    prebuildL' pairs@( (x,out):pairs') 
      | null x    =LACNode out    Nothing (createMap pairs')
      | otherwise =LACNode mempty Nothing (createMap pairs )
    createMap pairs = 
              createKeyValuePair  <$>  
              groupBy ( (==) `on` head.fst) pairs
    
    createKeyValuePair ::(Ord a, Monoid out, Eq out) => [ ([a], out ) ] -> (a , LACTree out a  )
    createKeyValuePair pairs@( ( a:_ , _ ) : _) = ( a, prebuildL' $ tailOnFst <$> pairs) 
    tailOnFst (x,out) = (tail x , out)


tieL :: (Eq out, Monoid out, Ord a, Observable out, Observable a) => (LACTree out a) -> (LACTree out a ) ->  (LACTree out a) 

tieL (LACNode out _     forest)
     (LACNode _   _ rootForest) = LACNode  out
                                        Nothing
                                       (fmap (\(key,val) -> ( key,  go Nothing val) ) $ observe "forest"  forest)
   where 
     go failure (LACNode out _ forest') =
       let 
          failureForest = case failure of 
            Nothing -> []
            (Just t) -> let LACNode _ _ f = t in f  
          failureOut    = case failure of
            Nothing -> mempty
            (Just t) -> let LACNode o _ _ = t in o 
       in case L.null forest' of 
          True  -> LACNode (failureOut <> out) failure   []
          False -> LACNode (failureOut <> out) failure $
                           map (\(key, val) -> (key, go   
                            ((L.lookup key failureForest) <|> 
                              -- ^if possible continue with current branch failures
                             (L.lookup key    rootForest)
                              -- ^alternatively, if possible, start to fail with top branch 
                             ) val ) 
                            ) $ observe "forest'" forest' 


