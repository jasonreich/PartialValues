% Partial Values
% Dealing with Haskell values that we expect to contain exceptions.
% Jason S. Reich
% 6th June 2012

> {-# LANGUAGE DeriveFunctor, Rank2Types, ScopedTypeVariables #-}

This module provides an interface for coping with values that you
know may contain exceptions, perhaps because you put them there (like
in LSC). The techniques used largely taken from 'Chasing Bottoms'
(Danielsson and Jansson, 2004) but with different formulations for my
purposes.

> module Test.PartialValues(
>   -- * Partial Values
>   Partial, isException, (?), peek, 
>   -- * Augmented Show typeclass
>   ShowA, AugmentedShow(..), showsA, showA, showsPrecA, appPrec,
>   defaultShowsPrecA',
>   -- * Explicitly Partial Functors
>   ExplicitF(..), ExplicitF2(..), BT(..), consBT, MaybePair(..),
>   toList_BT, toMaybe_MP) where

A few neccessary imports and hides.

> import Control.Applicative
> import Control.DeepSeq
> import Control.Exception
> import Data.Data
> import GHC.Show (appPrec)
> import System.IO.Unsafe
> import Prelude hiding (catch)

Partial Values
============

Partial values are just normal Haskell values that we know something
about. We represent this as a boring functor that is phantom-indexed
by an exception type.

> -- | 'Partial' values --- Values into which we may have deliberately
> -- injected exceptions.
> newtype Partial e a = Partial { unsafePeek :: a }
>   deriving Functor

Name a mathematical abstraction, it probably holds over the 'Partial'
functor.

> instance Monad (Partial e) where
>   return = Partial
>   Partial x >>= f = f x
>
> instance Applicative (Partial e) where
>   pure = return
>   f <*> x = unwrapMonad $ WrapMonad f <*> WrapMonad x

(?) creates partial values.

> -- | Throws an exception and wraps it as a 'Partial' value.
> (?) :: Exception e => e -> Partial e a
> (?) = Partial . throw

Peek all the way inside a 'Partial' value, catching the exception.

> peek :: (NFData a, Exception e) => Partial e a -> Either e a
> peek value = unsafePerformIO $
>   (Right <$> evaluate (force (unsafePeek value)))
>   `catch` (return . Left)
>   where force x = x `deepseq` x

'isException' tests for really partial values, i.e. ones that match
an exception predicate at their head.

> -- | Is the head of a Partial value an exception?
> isException :: Exception e => Partial e a -> Bool
> isException v = unsafePerformIO $
>   (evaluate (unsafePeek v) >> return False) `catch` aux v
>   where aux :: Partial e a -> e -> IO Bool
>         aux _ _ = return True

'show' will display '_' for values that are really partial.
*Requires an AugmentedShow instance*.

> instance (Exception e, AugmentedShow a) => Show (Partial e a) where
>   show = showA aux . unsafePeek
>     where
>       mkPartial :: forall a. a -> Partial e a
>       mkPartial = Partial
>       aux :: forall a. (a -> ShowS) -> a -> ShowS
>       aux rec x s | isException (mkPartial x) = "_" ++ s
>                   | otherwise = rec x s

AugmentedShow typeclass
=======================

To achieve this, we using a special variant of 'Show' that allows
a modification to be made at head before the instance definition takes
over.

> -- | Show function that is augmentable
> class AugmentedShow a where
>   showsPrecA' :: ShowA -> Int -> a -> ShowS
>   showA'      :: ShowA -> a -> String
>  
>   showsPrecA' aug _ x s = showA' aug x ++ s
>   showA' aug x = showsPrecA' aug 0 x ""

These 'augmentations' are described using a Rank-2 polymorphic
function.

> -- | Modification of shows functions
> type ShowA = forall a. (a -> ShowS) -> (a -> ShowS)

We wrap up the prime variants to automatically apply the modification
at head.

> showsA :: AugmentedShow a => ShowA -> a -> ShowS
> showsA aug = aug $ showsPrecA' aug 0
>
> showsPrecA :: AugmentedShow a => ShowA -> Int -> a -> ShowS
> showsPrecA aug p = aug $ showsPrecA' aug p
>
> showA :: AugmentedShow a => ShowA -> a -> String
> showA aug x = aug (\x s -> showA' aug x ++ s) x []

For example, here are some boring instances where they never recurse.

> instance AugmentedShow () where
>   showA' _ () = "()"
  
> instance AugmentedShow Bool where
>   showA' _ = show

> instance AugmentedShow Char where
>   showA' _ = show
  
> instance AugmentedShow Int where
>   showA' _ = show
  
> instance AugmentedShow Integer where
>   showA' _ = show

And here are some more interesting ones.

> instance (AugmentedShow a, AugmentedShow b) => 
>          AugmentedShow (a,b) where
>   showsPrecA' aug _ (a,b) s = show_tuple [showsA aug a, showsA aug b] s
>  
> instance (AugmentedShow a, AugmentedShow b, AugmentedShow c) 
>          => AugmentedShow (a,b,c) where
>   showsPrecA' aug _ (a,b,c) s = show_tuple [showsA aug a, showsA aug b, showsA aug c] s
>
> instance (AugmentedShow a, AugmentedShow b, AugmentedShow c, AugmentedShow d, AugmentedShow e) 
>          => AugmentedShow (a,b,c,d,e) where
>   showsPrecA' aug _ (a,b,c,d,e) s = show_tuple [showsA aug a, showsA aug b, showsA aug c, showsA aug d, showsA aug e] s
>
> show_tuple :: [ShowS] -> ShowS
> show_tuple ss = ('(':) . foldr1 (\s r -> s . (',':) . r) ss . (')':)
>
> instance (AugmentedShow a) => AugmentedShow [a] where
>   showsPrecA' _   d []  = (++) "[]"
>   showsPrecA' aug d (x:xs) = showParen (d > 5) showStr
>     where
>       showStr = showsPrecA aug (5+1) x
>               . showString ":" 
>               . showsPrecA aug (5) xs
> 
> instance (AugmentedShow a) => AugmentedShow (Maybe a) where
>   showsPrecA' _   d Nothing  = (++) "Nothing"
>   showsPrecA' aug d (Just n) = showParen (d > appPrec) showStr
>     where
>       showStr = showString "Just " . showsPrecA aug (appPrec+1) n

I'm sure these could be automatically derived using Derive or DrIFT. I've
gone for a SYB approach.

~ Data.Generics.Text

> constrArity :: Data d => d -> Int
> constrArity = length . gmapQ (const ())

> defaultShowsPrecA :: Data a => ShowA -> Int -> a -> ShowS
> defaultShowsPrecA inj p = inj (defaultShowsPrecA' inj p)

> -- | A default definition of 'showsPrecA'' for Data instances.
> defaultShowsPrecA' :: Data a => ShowA -> Int -> a -> ShowS
> defaultShowsPrecA' inj p t = showParen ((constrArity $ t) > 0 && p > appPrec) (aux t)
>   where aux t = (showString . showConstr . toConstr $ t)
>               . (foldr (.) id . gmapQ ((showChar ' ' .) . defaultShowsPrecA inj (appPrec + 1)) $ t)

Explicitly Partial Functors
=========================

Sometimes I want to make the undefined at the head of a structure
explicit. I don't have a safe way of doing this yet, but until then
we have Explictly Partial Functors; make Partial structure explicit.

The basic idea is there exists some function such that;

> class ExplicitF f where
>   absorb :: Exception e => Partial e (f a) -> f (Partial e a)

> class ExplicitF2 f where
>   absorb2 :: Exception e => Partial e (f a b) -> f (Partial e a) (Partial e b)

Binary trees
------------

With binary trees, we can transform Really Partial tree structure into
empty nodes.

> data BT a = Empty | Leaf a | Branch (BT a) (BT a) deriving Functor

> consBT x xs = Leaf x `Branch` xs
> toList_BT = flip aux []
>   where aux Empty        = id
>         aux (Leaf x)     = (x :)
>         aux (Branch l r) = aux l . aux r

> instance Monad BT where
>   return = Leaf
>   Empty      >>= _ = Empty
>   Leaf x     >>= f = f x
>   Branch l r >>= f = Branch (l >>= f) (r >>= f)

> instance ExplicitF BT where
>   absorb xs | isException xs  = Empty
>   absorb (Partial Empty)        = Empty
>   absorb (Partial (Leaf a))     = Leaf (Partial a)
>   absorb (Partial (Branch l r)) = (absorb . Partial $ l) `Branch` (absorb . Partial $ r)

Maybe Pairs
-----------

With Maybe enclosing Tuples, we turn Really Partial maybe and tuple
structure into Nothings.

> data MaybePair a b = NothingPair | JustPair (a, b)
> toMaybe_MP NothingPair  = Nothing
> toMaybe_MP (JustPair x) = Just x

> instance ExplicitF2 MaybePair where
>   absorb2 xs | isException xs       = NothingPair
>   absorb2 (Partial (NothingPair))     = NothingPair
>   absorb2 (Partial (JustPair (a, b))) = JustPair (Partial a, Partial b)
