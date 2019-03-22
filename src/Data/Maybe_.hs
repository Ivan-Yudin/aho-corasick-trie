{-# LANGUAGE InstanceSigs          #-}

module Data.Maybe_ where 
  
import Control.Applicative ((<|>),liftA2,(<*>),pure)
import Data.Default.Class(Default,def)

data Maybe' a b = Just' a b | Nothing' deriving (Eq,Show,Read)

-------------------------------------------------------------
-- Classical instances for Maybe'
------------------------------------------------------------

instance (Eq a) => Semigroup (Maybe' a b) where
  Nothing' <> y  = y
  x  <> _        = x

instance (Eq a ) => Monoid (Maybe' a b) where
  mempty = Nothing'


instance Functor (Maybe' a) where
  fmap :: (b -> c) -> (Maybe' a b -> Maybe' a c)
  fmap f (Just' a b) = Just'    a (f b)
  fmap f  Nothing'   = Nothing'

-------------------------------------------------------------------
--  Applicative instance for @Maybe' a@
-------------------------------------------------------------------

instance (Default a, Eq a) => Applicative (Maybe' a) where
  pure :: b -> Maybe' a b
  pure b = Just' def b

  liftA2 :: (b -> c -> d) -> (Maybe' a b) -> (Maybe' a c) -> Maybe' a d
  liftA2 h (Just' a b) (Just' a' c) | a == a' = Just' a (h b c)
  liftA2 _ _ _ = Nothing'


