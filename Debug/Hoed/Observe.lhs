\begin{code}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
\end{code}

The file is part of the Haskell Object Observation Debugger,
(HOOD) March 2010 release.

HOOD is a small post-mortem debugger for the lazy functional
language Haskell. It is based on the concept of observation of
intermediate data structures, rather than the more traditional
stepping and variable examination paradigm used by imperative
language debuggers.

Copyright (c) Andy Gill, 1992-2000
Copyright (c) The University of Kansas 2010
Copyright (c) Maarten Faddegon, 2013-2014

All rights reserved. HOOD is distributed as free software under
the license in the file "License", which available from the HOOD
web page, http://www.haskell.org/hood

This module produces CDS's, based on the observation made on Haskell
objects, including base types, constructors and functions.

WARNING: unrestricted use of unsafePerformIO below.

This was ported for the version found on www.haskell.org/hood.


%************************************************************************
%*									*
\subsection{Exports}
%*									*
%************************************************************************

\begin{code}
module Debug.Hoed.Observe
  (
   -- * The main Hood API
  
    gdmobserve
  , gdmobserve1
  , gdmobservers
  , Observer(..)   -- contains a 'forall' typed observe (if supported).
  -- , Observing      -- a -> a
  , Observable(..) -- Class
  , Observable1(..)  -- Higher order version of Observable
  , Parent
  , nilObserver
  , runO	   -- IO a -> IO ()
  , printO	   -- a -> IO ()
  , putStrO	   -- String -> IO ()

   -- * For advanced users, that want to render their own datatypes.
  , (<<)           -- (Observable a) => ObserverM (a -> b) -> a -> ObserverM b
  , (.<<)          -- (Observable1 f) => ObserverM (f a -> b) -> f a -> ObserverM b
  , thunk          -- (Observable a) => a -> ObserverM a	
  , nothunk
  , send
  , observeBase
  , observeOpaque

  -- * For users that want to write there own render drivers.
  
  , debugO	   -- IO a -> IO [CDS]
  , CDS(..)

  , Generic, Generic1
  ) where
\end{code}


%************************************************************************
%*									*
\subsection{Imports and infixing}
%*									*
%************************************************************************

\begin{code}
import System.IO
import Data.Maybe
import Control.Applicative
import Control.Monad
import Data.Array as Array
import Data.List
import Data.Char
--import System

import GHC.Generics

-- The only non standard one we assume
--import IOExts
import Data.IORef
import System.IO.Unsafe
\end{code}

\begin{code}
import Control.Concurrent
\end{code}

\begin{code}
import Control.Exception ( Exception, throw )
import qualified Control.Exception as Exception
{-
 ( catch
		, Exception(..)
		, throw
		) as Exception
-}
import Data.Dynamic ( Dynamic )

import Prelude.Extras (Lift1(..))
\end{code}

\begin{code}
infixl 9 <<
infixl 9 .<<
\end{code}


%************************************************************************
%*									*
\subsection{External start functions}
%*									*
%************************************************************************

Run the observe ridden code.

\begin{code}
-- | run some code and return the CDS structure (for when you want to write your own debugger).
debugO :: IO a -> IO [CDS]
debugO program = 
     do { initUniq
	; startEventStream
        ; let errorMsg e = "[Escaping Exception in Code : " ++ show e ++ "]"
	; ourCatchAllIO (do { program ; return () }) 
			(hPutStrLn stderr . errorMsg)
        ; events <- endEventStream
	; return (eventsToCDS events)
	}

-- | print a value, with debugging 
printO :: (Show a) => a -> IO ()
printO expr = runO (print expr)

-- | print a string, with debugging 
putStrO :: String -> IO ()
putStrO expr = runO (putStr expr)

-- | The main entry point; run some IO code, and debug inside it.
-- 
-- An example of using this debugger is 
--
-- @runO (print [ observe "+1" (+1) x | x <- observe "xs" [1..3]])@
-- 
-- @[2,3,4]
-- -- +1
--  { \ 1  -> 2
--  }
-- -- +1
--  { \ 2  -> 3
--  }
-- -- +1
--  { \ 3  -> 4
--  }
-- -- xs
--  1 : 2 : 3 : []@
-- 
-- Which says, the return is @[2,3,4]@, there were @3@ calls to +1
-- (showing arguments and results), and @xs@, which was the list
-- @1 : 2 : 3 : []@.
-- 

runO :: IO a -> IO ()
runO program =
    do { cdss <- debugO program
       ; let cdss1 = rmEntrySet cdss
       ; let cdss2 = simplifyCDSSet cdss1
       ; let output1 = cdssToOutput cdss2 
       ; let output2 = commonOutput output1
       ; let ptyout  = pretty 160 (foldr (<>) nil (map renderTop output2))
       ; hPutStrLn stderr ""
       ; hPutStrLn stderr ptyout
       }
\end{code}


%************************************************************************
%*									*
\subsection{Simulations}
%*									*
%************************************************************************

Here we provide stubs for the functionally that is not supported
by some compilers, and provide some combinators of various flavors.

\begin{code}
ourCatchAllIO :: IO a -> (Exception.SomeException -> IO a) -> IO a
ourCatchAllIO = Exception.catch

handleExc :: Parent -> Exception.SomeException -> IO a
handleExc context exc = return (send "throw" (return throw << exc) context)
\end{code}


%************************************************************************
%*									*
\subsection{GDM Generics}
%*									*
%************************************************************************

he generic implementation of the observer function.

\begin{code}
class Observable a where
	observer  :: a -> Parent -> a
        default observer :: (Generic a, GObservable (Rep a)) => a -> Parent -> a
        observer x c = to (gdmobserver (from x) c)
	{-
         - This used used to group several observer instances together.
	 -}
	observers :: Parent -> String -> (Observer -> a) -> a
	observers = defaultObservers

class Observable1 f where
        observer1  :: Observable a => f a -> Parent -> f a
--        default observer1 :: (Observable (f a), Observable a) => f a -> Parent -> f a
--        observer1 = observer

        default observer1 :: (Generic1 f, Observable a, GObservable1 (Rep1 f)) => f a -> Parent -> f a
        observer1 x c = to1 (gdmobserver1 (from1 x) c)

        observers1 :: Observable a => Parent -> String -> (Observer -> f a) -> f a
        observers1 = defaultObservers1
{-
instance (Observable1 f, Observable a) => Observable (f a) where
  observer = observer1
  observers = observers1
-}

class GObservable f where
        gdmobserver :: f a -> Parent -> f a
        gdmObserveChildren :: f a -> ObserverM (f a)
        gdmShallowShow :: f a -> String

class GObservable1 f where
  gdmobserver1 :: Observable a => f a -> Parent -> f a
  gdmObserveChildren1 :: Observable a => f a -> ObserverM (f a)
  gdmShallowShow1 :: Observable a => f a -> String

\end{code}

Creating a shallow representation for types of the Data class.

\begin{code}

-- shallowShow :: Constructor c => t c (f :: * -> *) a -> [Char]
-- shallowShow = conName

\end{code}

Observing the children of Data types of kind *.

\begin{code}

-- Meta: data types

instance GObservable (V1) where
instance GObservable1 V1

instance (GObservable a) => GObservable (M1 D d a) where
        gdmobserver m@(M1 x) cxt = M1 (gdmobserver x cxt)
        gdmObserveChildren = gthunk
instance (GObservable1 a) => GObservable1 (M1 D d a) where
        gdmobserver1 m@(M1 x) cxt = M1 (gdmobserver1 x cxt)
        gdmObserveChildren1 = gthunk1

-- Meta: Constructors
instance (GObservable a, Constructor c) => GObservable (M1 C c a) where
        gdmobserver m@(M1 x) cxt = M1 (send (gdmShallowShow m) (gdmObserveChildren x) cxt)
        gdmObserveChildren = gthunk
        gdmShallowShow = conName
instance (GObservable1 a, Constructor c) => GObservable1 (M1 C c a) where
        gdmobserver1 m@(M1 x) cxt = M1 (send (gdmShallowShow1 m) (gdmObserveChildren1 x) cxt)
        gdmObserveChildren1 = gthunk1
        gdmShallowShow1 = conName

-- Meta: Selectors
--      | selName m == "" = M1 y
--      | otherwise       = M1 (send (selName m) (return y) cxt)
instance (GObservable a, Selector s) => GObservable (M1 S s a) where
        gdmobserver m@(M1 x) cxt
          | selName m == "" = M1 (gdmobserver x cxt)
          | otherwise       = M1 (send (selName m ++ " =") (gdmObserveChildren x) cxt)
        gdmObserveChildren = gthunk
instance (GObservable1 a, Selector s) => GObservable1 (M1 S s a) where
        gdmobserver1 m@(M1 x) cxt
          | selName m == "" = M1 (gdmobserver1 x cxt)
          | otherwise       = M1 (send (selName m ++ " =") (gdmObserveChildren1 x) cxt)
        gdmObserveChildren1 = gthunk1

-- Unit: used for constructors without arguments
instance GObservable U1 where
        gdmobserver x _ = x
        gdmObserveChildren = return
instance GObservable1 U1 where
        gdmobserver1 x _ = x
        gdmObserveChildren1 = return

-- Products: encode multiple arguments to constructors
instance (GObservable a, GObservable b) => GObservable (a :*: b) where
        gdmobserver (a :*: b) cxt = error "gdmobserver product"
        gdmObserveChildren (a :*: b) = do a'  <- gdmObserveChildren a
                                          b'  <- gdmObserveChildren b
                                          return (a' :*: b')
instance (GObservable1 a, GObservable1 b) => GObservable1 (a :*: b) where
        gdmobserver1 (a :*: b) cxt = error "gdmobserver product"
        gdmObserveChildren1 (a :*: b) = do a'  <- gdmObserveChildren1 a
                                           b'  <- gdmObserveChildren1 b
                                           return (a' :*: b')


-- Sums: encode choice between constructors
instance (GObservable a, GObservable b) => GObservable (a :+: b) where
        gdmobserver (L1 x) cxt = L1 (gdmobserver x cxt)
        gdmobserver (R1 x) cxt = R1 (gdmobserver x cxt)
        gdmObserveChildren (R1 x) = do {x' <- gdmObserveChildren x; return (R1 x')}
        gdmObserveChildren (L1 x) = do {x' <- gdmObserveChildren x; return (L1 x')}
instance (GObservable1 a, GObservable1 b) => GObservable1 (a :+: b) where
        gdmobserver1 (L1 x) cxt = L1 (gdmobserver1 x cxt)
        gdmobserver1 (R1 x) cxt = R1 (gdmobserver1 x cxt)
        gdmObserveChildren1 (R1 x) = do {x' <- gdmObserveChildren1 x; return (R1 x')}
        gdmObserveChildren1 (L1 x) = do {x' <- gdmObserveChildren1 x; return (L1 x')}

-- Constants: additional parameters and recursion of kind *
instance (Observable a) => GObservable (K1 i a) where
        gdmobserver (K1 x) cxt = K1 (observer_ observer x cxt)
        gdmObserveChildren = gthunk
instance (Observable a) => GObservable1 (K1 i a) where
        gdmobserver1 (K1 x) cxt = K1 (observer_ observer x cxt)
        gdmObserveChildren1 = gthunk1

-- Recursion
instance (Observable1 f) => GObservable1 (Rec1 f) where
        gdmobserver1 (Rec1 x) ctxt = Rec1 (observer1 x ctxt)
        gdmObserveChildren1 = gthunk1
-- Ocurrence
instance GObservable1 Par1 where
        gdmobserver1 (Par1 x) ctxt = Par1 (observer x ctxt)
        gdmObserveChildren1 = gthunk1

-- Composition
instance (Functor f, Observable1 f, GObservable1 g) => GObservable1 (f :.: g) where
        gdmobserver1 (Comp1 fg) ctxt =
          Comp1 (fmap glower1 $ observer1 (fmap GLift1 fg) ctxt)

newtype GLift1 f a = GLift1 { glower1 :: f a }

instance GObservable1 f => Observable1 (GLift1 f) where
  observer1 (GLift1 fx) = GLift1 . gdmobserver1 fx

instance (Observable a, GObservable1 f) => Observable (GLift1 f a) where
  observer = observer1

\end{code}

Observing functions is done via the ad-hoc mechanism, because
we provide an instance definition the default is ignored for
this type.

\begin{code}
instance {-# OVERLAPPABLE #-} (Observable a) => Observable1 ((->) a) where
  observer1 fn cxt arg = gdmFunObserver cxt fn arg
  observers1 = defaultFnObservers

instance {-# OVERLAPPABLE #-} (Observable a, Observable b) => Observable (a -> b) where
  observer = observer1
  observers = observers1

instance {-# OVERLAPPABLE #-} (Observable a, Observable1 m, Observable b) => Observable(a -> m b) where
  observer fn ctxt arg
         = sendObserveFnPacket (do { arg' <- thunk observer arg
                                  ; thunk observer1 (fn arg')
                                  }
                              ) ctxt
  observers = defaultFnObservers1

--  observers = observers1

\end{code}

Observing the children of Data types of kind *->*.

\begin{code}
gdmFunObserver :: (Observable a,Observable b) => Parent -> (a->b) -> (a->b)
gdmFunObserver cxt fn arg
        = sendObserveFnPacket (do { arg' <- thunk observer arg
                                  ; thunk observer (fn arg')
                                  }
                              ) cxt

gdmFunObserver1 :: (Observable a, Observable b, Observable1 f
                   ) => Parent -> (f a -> f b) -> f a -> f b
gdmFunObserver1 cxt fn arg
        = sendObserveFnPacket (do { arg' <- thunk observer1 arg
                                  ; thunk observer1 (fn arg')
                                  }
                              ) cxt
\end{code}


%************************************************************************
%*									*
\subsection{Generics}
%*									*
%************************************************************************

Generate a new observe from generated observers and the gobserve mechanism.
Where gobserve is the 'classic' observe but parametrized.


Observing functions is done via the ad-hoc mechanism, because
we provide an instance definition the default is ignored for
this type.

\begin{code}

-- instance (Observable a,Observable b) => Observable (a -> b) where
--   observer = funObserver
\end{code}

%************************************************************************
%*									*
\subsection{Instances}
%*									*
%************************************************************************

 The Haskell Base types

\begin{code}
instance Observable Int 	where { observer = observeBase }
instance Observable Bool 	where { observer = observeBase }
instance Observable Integer 	where { observer = observeBase }
instance Observable Float 	where { observer = observeBase }
instance Observable Double	where { observer = observeBase }
instance Observable Char 	where { observer = observeBase }

instance Observable ()		where { observer = observeOpaque "()" }

-- utilities for base types.
-- The strictness (by using seq) is the same 
-- as the pattern matching done on other constructors.
-- we evalute to WHNF, and not further.

observeBase :: (Show a) => a -> Parent -> a
observeBase lit cxt = seq lit $ send (show lit) (return lit) cxt

observeOpaque :: String -> a -> Parent -> a
observeOpaque str val cxt = seq val $ send str (return val) cxt
\end{code}

The Constructors.

\begin{code}
instance (Observable a) => Observable1 ((,) a) where
  observer1 (a,b) = send "," (return (,) << a << b)

instance (Observable a, Observable b) => Observable (a,b) where
  observer = observer1
  observers = observers1

instance (Observable a,Observable b) => Observable1 ((,,) a b) where
  observer1 (a,b,c) = send "," (return (,,) << a << b << c)

instance (Observable a, Observable b, Observable c) => Observable (a,b,c) where
  observer = observer1
  observers = observers1

instance (Observable a,Observable b,Observable c) 
	  => Observable1 ((,,,) a b c) where
  observer1 (a,b,c,d) = send "," (return (,,,) << a << b << c << d)

instance (Observable a, Observable b, Observable c, Observable d) => Observable (a,b,c,d) where
  observer = observer1
  observers = observers1

instance (Observable a,Observable b,Observable c,Observable d) 
	 => Observable1 ((,,,,) a b c d) where
  observer1 (a,b,c,d,e) = send "," (return (,,,,) << a << b << c << d << e)

instance (Observable a, Observable b, Observable c, Observable d, Observable e) => Observable (a,b,c,d,e) where
  observer = observer1
  observers = observers1

instance Observable1 [] where
  observer1 (a:as) = send ":"  (return (:) << a .<< as)
  observer1 []     = send "[]" (return [])

instance Observable a => Observable [a] where
  observer = observer1
  observers = observers1

instance Observable1 Maybe where
  observer1 (Just a) = send "Just"    (return Just << a)
  observer1 Nothing  = send "Nothing" (return Nothing)

instance Observable a => Observable (Maybe a) where
  observer = observer1
  observers = observers1

instance (Observable a) => Observable1 (Either a) where
  observer1 (Left a)  = send "Left"  (return Left  << a)
  observer1 (Right a) = send "Right" (return Right << a)

instance (Observable a, Observable b) => Observable (Either a b) where
  observer = observer1
  observers = observers1

instance Observable1 f => Observable1 (Lift1 f) where
  observer1 (Lift1 f) = Lift1 . observer1 f
  observers1 p tag f = Lift1 $ observers1 p tag (lower1 . f)

instance (Observable a, Observable1 f) => Observable (Lift1 f a) where
  observer = observer1
  observers = observers1

\end{code}

Arrays.

\begin{code}
instance (Ix a,Observable a) => Observable1 (Array.Array a) where
  observer1 arr = send "array" (return (\bounds assocs -> Array.array bounds (map lower1 assocs))
                                        .<< Array.bounds arr
                                        .<< fmap Lift1(Array.assocs arr)
			      )
instance (Ix i, Observable i, Observable a) => Observable (Array.Array i a) where
  observer = observer1
  observers = observers1
\end{code}

IO monad.

\begin{code}
instance Observable1 IO where
  observer1 fn cxt =
	do res <- fn
	   send "<IO>" (return return << res) cxt
instance Observable a => Observable (IO a) where
  observers = observers1
  observer = observer1
\end{code}

Higuer-Order Observables
begin{code}

newtype WrapObserveM m a = WrapObserveM {unwrapObserveM :: m a} deriving (Observable, Observable1)
class ObserveM ma where observeM :: String -> ma -> ma
instance (Observable1 m, Observable a) => ObserveM (m a) where
  observeM tag = unwrapObserveM . gdmobserve1 tag . WrapObserveM
instance (Observable1 m, Observable a) => ObserveM (b -> m a) where
  observeM tag = (observeM tag .)
instance (Observable1 m, Observable a) => ObserveM (c -> b -> m a) where
  observeM tag = ((observeM tag .).)
instance (Observable1 m, Observable a) => ObserveM (d -> c -> b -> m a) where
  observeM tag = (((observeM tag .).).)
instance (Observable1 m, Observable a) => ObserveM (e -> d -> c -> b -> m a) where
  observeM tag = ((((observeM tag .).).).)

end{code}

The Exception *datatype* (not exceptions themselves!).
For now, we only display IOExceptions and calls to Error.

\begin{code}
instance Observable Exception.SomeException where
--  observer (IOException a)      = observeOpaque "IOException" (IOException a)
--  observer (ErrorCall a)        = send "ErrorCall"   (return ErrorCall << a)
  observer other                = send "<Exception>" (return other)

instance Observable Dynamic where { observer = observeOpaque "<Dynamic>" }
\end{code}


%************************************************************************
%*									*
\subsection{Classes and Data Definitions}
%*									*
%************************************************************************

MF TODO: remove

class Observable a where
	{-
	 - This reveals the name of a specific constructor.
	 - and gets ready to explain the sub-components.
         -
         - We put the context second so we can do eta-reduction
	 - with some of our definitions.
	 -}
	observer  :: a -> Parent -> a 

type Observing a = a -> a

MF TODO: end

\begin{code}
data Observer =
  O (forall a. Observable a => String -> a -> a)
    (forall a. Observable a => String -> (Observer -> a) -> a)

nilObserver :: Observer
nilObserver = O o oo where
  o  _ x = x
  oo _ f = f nilObserver

defaultObservers :: (Observable a) => Parent -> String -> (Observer -> a) -> a
defaultObservers parent label fn = unsafeWithUniq $ \node -> do
        sendEvent node parent (Observe label)
        let p = Parent node 0
        return$ observer_ observer (fn (O (gobserve p) (observers p))) p

defaultObservers1 :: (Observable1 f, Observable a) => Parent -> String -> (Observer -> f a) -> f a
defaultObservers1 parent label fn = unsafeWithUniq $ \node -> do
        sendEvent node parent (Observe label)
        let p = Parent node 0
        return$ observer_ observer1 (fn (O (gobserve p) (observers p))) p

defaultFnObservers :: (Observable1 ((->) a), Observable b)
		      => Parent -> String -> (Observer -> a -> b) -> a -> b
defaultFnObservers parent label fn arg = unsafeWithUniq $ \node -> do
        sendEvent node parent (Observe label)
        let p = Parent node 0
        return$ observer_ observer1 (fn (O (gobserve p) (observers p))) p arg

defaultFnObservers1 :: (Observable1 ((->) a), Observable1 g, Observable a, Observable b)
		      => Parent -> String -> (Observer -> a -> g b) -> a -> g b
defaultFnObservers1 parent label fn arg = unsafeWithUniq $ \node -> do
        sendEvent node parent (Observe label)
        let p = Parent node 0
        return$ observer_ observer (fn (O (gobserve p) (observers p))) p arg

\end{code}


%************************************************************************
%*									*
\subsection{The ObserveM Monad}
%*									*
%************************************************************************

The Observer monad, a simple state monad, 
for placing numbers on sub-observations.

\begin{code}
newtype ObserverM a = ObserverM { runMO :: Int -> Int -> (a,Int) }

instance Functor ObserverM where
  fmap = liftM

instance Applicative ObserverM where
  pure  = return
  (<*>) = ap

instance Monad ObserverM where
	return a = ObserverM (\ c i -> (a,i))
	fn >>= k = ObserverM (\ c i ->
		case runMO fn c i of
		  (r,i2) -> i2 `seq` runMO (k r) c i2
		)

thunk :: (a -> Parent -> a) -> a -> ObserverM a
thunk f a = ObserverM $ \ parent port ->
                let port1 = port+1 in
                port1 `seq`
		( observer_ f a (Parent
				{ observeParent = parent
				, observePort   = port
				}) 
		, port1 )

gthunk :: (GObservable f) => f a -> ObserverM (f a)
gthunk a = ObserverM $ \ parent port ->
                let port1 = port+1 in
                port1 `seq`
		( gdmobserver_ a (Parent
				{ observeParent = parent
				, observePort   = port
				})
		, port1 )

gthunk1 :: (GObservable1 f, Observable a) => f a -> ObserverM (f a)
gthunk1 a = ObserverM $ \ parent port ->
                let port1 = port+1 in
                port1 `seq`
		( gdmobserver1_ a (Parent
				{ observeParent = parent
				, observePort   = port
				})
		, port1 )

nothunk :: a -> ObserverM a
nothunk a = ObserverM $ \ parent port ->
                let port1 = port+1 in
                port1 `seq`
		( observer__ a (Parent
				{ observeParent = parent
				, observePort   = port
				})
		, port1 )


(<<) :: (Observable a) => ObserverM (a -> b) -> a -> ObserverM b
-- fn << a = do { fn' <- fn ; a' <- thunk a ; return (fn' a') }
fn << a = gdMapM (thunk observer) fn a

(.<<) :: (Observable1 f, Observable a) => ObserverM (f a -> b) -> f a -> ObserverM b
-- fn << a = do { fn' <- fn ; a' <- thunk a ; return (fn' a') }
fn .<< a = gdMapM (thunk observer1) fn a

gdMapM :: (Monad m)
        => (a -> m a)  -- f
        -> m (a -> b)  -- data constructor
        -> a           -- argument
        -> m b         -- data
gdMapM f c a = do { c' <- c ; a' <- f a ; return (c' a') }

\end{code}


%************************************************************************
%*									*
\subsection{observe and friends}
%*									*
%************************************************************************

Our principle function and class

\begin{code}
-- | 'observe' observes data structures in flight.
--  
-- An example of use is 
--  @
--    map (+1) . observe \"intermeduate\" . map (+2)
--  @
--
-- In this example, we observe the value that flows from the producer
-- @map (+2)@ to the consumer @map (+1)@.
-- 
-- 'observe' can also observe functions as well a structural values.
-- 
gobserve_ f parent label orig = unsafeWithUniq $ \ node ->
     do { sendEvent node parent (Observe label)
	; return (observer_ f orig (Parent node 0))
        }

{-# NOINLINE gobserve #-}
{-# NOINLINE gobserve1 #-}
gobserve :: Observable a => Parent -> String -> a -> a
gobserve = gobserve_ observer
gobserve1 ::  (Observable a, Observable1 m) => Parent -> String -> m a -> m a
gobserve1 = gobserve_ observer1

{-# NOINLINE gdmobserve #-}
{-# NOINLINE gdmobserve1 #-}
gdmobserve :: Observable a => String -> a -> a
gdmobserve  = gobserve root
gdmobserve1 :: (Observable1 f, Observable a) => String -> f a -> f a
gdmobserve1 = gobserve1 root

{-# NOINLINE gdmobservers #-}
{-# NOINLINE gdmobservers1 #-}

gdmobservers :: Observable a => String -> (Observer -> a) -> a
gdmobservers = observers root

gdmobservers1 :: (Observable1 f, Observable a) => String -> (Observer -> f a) -> f a
gdmobservers1 = observers1 root

{- This gets called before observer, allowing us to mark
 - we are entering a, before we do case analysis on
 - our object.
 -}

{-# NOINLINE observer_ #-}
observer_ :: (a -> Parent -> a) -> a -> Parent -> a 
observer_ f a context = sendEnterPacket f a context

gdmobserver_ :: (GObservable f) => f a -> Parent -> f a
gdmobserver_ a context = gsendEnterPacket a context

gdmobserver1_ :: (GObservable1 f, Observable a) => f a -> Parent -> f a
gdmobserver1_ a context = gsendEnterPacket1 a context

{-# NOINLINE observer__ #-}
observer__ :: a -> Parent -> a
observer__ a context = sendNoEnterPacket a context

\end{code}

\begin{code}
data Parent = Parent
	{ observeParent :: !Int	-- my parent
	, observePort   :: !Int	-- my branch number
	} deriving Show
root = Parent 0 0

\end{code}


The functions that output the data. All are dirty.

\begin{code}
unsafeWithUniq :: (Int -> IO a) -> a
unsafeWithUniq fn 
  = unsafePerformIO $ do { node <- getUniq
		         ; fn node
		         }
\end{code}

\begin{code}

send :: String -> ObserverM a -> Parent -> a
send consLabel fn context = unsafeWithUniq $ \ node ->
     do { let (r,portCount) = runMO fn node 0
	; sendEvent node context (Cons portCount consLabel)
	; return r
	}

sendEnterPacket :: (a -> Parent -> a) -> a -> Parent -> a
sendEnterPacket f r context = unsafeWithUniq $ \ node ->
     do	{ sendEvent node context Enter
	; ourCatchAllIO (evaluate (f r context))
	                (handleExc context)
	}

gsendEnterPacket :: (GObservable f) => f a -> Parent -> f a
gsendEnterPacket r context = unsafeWithUniq $ \ node ->
     do	{ sendEvent node context Enter
	; ourCatchAllIO (evaluate (gdmobserver r context))
	                (handleExc context)
	}

gsendEnterPacket1 :: (GObservable1 f, Observable a) => f a -> Parent -> f a
gsendEnterPacket1 r context = unsafeWithUniq $ \ node ->
     do	{ sendEvent node context Enter
	; ourCatchAllIO (evaluate (gdmobserver1 r context))
	                (handleExc context)
	}

sendNoEnterPacket :: a -> Parent -> a
sendNoEnterPacket r context = unsafeWithUniq $ \ node ->
     do	{ sendEvent node context NoEnter
	; ourCatchAllIO (evaluate r)
	                (handleExc context)
	}

evaluate :: a -> IO a
evaluate a = a `seq` return a

sendObserveFnPacket :: ObserverM a -> Parent -> a
sendObserveFnPacket fn context = unsafeWithUniq $ \ node ->
     do	{ let (r,_) = runMO fn node 0
	; sendEvent node context Fun
	; return r
	}
\end{code}


%************************************************************************
%*									*
\subsection{Event stream}
%*									*
%************************************************************************

Trival output functions

\begin{code}
data Event = Event
		{ portId     :: !Int
		, parent     :: !Parent
		, change     :: !Change
		}
	deriving Show

data Change
	= Observe 	!String
	| Cons    !Int 	!String
	| Enter
        | NoEnter
	| Fun
	deriving Show

startEventStream :: IO ()
startEventStream = writeIORef events []

endEventStream :: IO [Event]
endEventStream =
	do { es <- readIORef events
	   ; writeIORef events badEvents 
	   ; return es
	   }

sendEvent :: Int -> Parent -> Change -> IO ()
sendEvent nodeId parent change =
	do { nodeId `seq` parent `seq` return ()
	   ; change `seq` return ()
	   ; takeMVar sendSem
	   ; es <- readIORef events
	   ; let event = Event nodeId parent change
	   ; writeIORef events (event `seq` (event : es))
	   ; putMVar sendSem ()
	   }

-- local
events :: IORef [Event]
events = unsafePerformIO $ newIORef badEvents

badEvents :: [Event]
badEvents = error "Bad Event Stream"

-- use as a trivial semiphore
{-# NOINLINE sendSem #-}
sendSem :: MVar ()
sendSem = unsafePerformIO $ newMVar ()
-- end local
\end{code}


%************************************************************************
%*									*
\subsection{unique name supply code}
%*									*
%************************************************************************

Use the single threaded version

\begin{code}
initUniq :: IO ()
initUniq = writeIORef uniq 1

getUniq :: IO Int
getUniq
    = do { takeMVar uniqSem
	 ; n <- readIORef uniq
	 ; writeIORef uniq $! (n + 1)
	 ; putMVar uniqSem ()
	 ; return n
	 }

peepUniq :: IO Int
peepUniq = readIORef uniq

-- locals
{-# NOINLINE uniq #-}
uniq :: IORef Int
uniq = unsafePerformIO $ newIORef 1

{-# NOINLINE uniqSem #-}
uniqSem :: MVar ()
uniqSem = unsafePerformIO $ newMVar ()
\end{code}



%************************************************************************
%*									*
\subsection{Global, initualizers, etc}
%*									*
%************************************************************************

\begin{code}
openObserveGlobal :: IO ()
openObserveGlobal =
     do { initUniq
	; startEventStream
	}

closeObserveGlobal :: IO [Event]
closeObserveGlobal =
     do { evs <- endEventStream
        ; putStrLn ""
	; return evs
	}
\end{code}


%************************************************************************
%*									*
\subsection{The CDS and converting functions}
%*									*
%************************************************************************

\begin{code}
data CDS = CDSNamed String         CDSSet
	 | CDSCons Int String     [CDSSet]
	 | CDSFun  Int             CDSSet CDSSet
	 | CDSEntered Int
	 | CDSTerminated Int
	deriving (Show,Eq,Ord)

type CDSSet = [CDS]


eventsToCDS :: [Event] -> CDSSet
eventsToCDS pairs = getChild 0 0
   where
     res i = (!) out_arr i

     maxBnd = if null pairs then 0 else maximum $ map portId pairs
     bnds = (0, maxBnd)

     mid_arr :: Array Int [(Int,CDS)]
     mid_arr = accumArray (flip (:)) [] bnds
		[ (pnode,(pport,res node))
	        | (Event node (Parent pnode pport) _) <- pairs
		]

     out_arr = array bnds	-- never uses 0 index
	        [ (node,getNode'' node change)
	 	| (Event node _ change) <- pairs
		]

     getNode'' ::  Int -> Change -> CDS
     getNode'' node change =
       case change of
	(Observe str) -> CDSNamed str (getChild node 0)
	(Enter)       -> CDSEntered node
	(NoEnter)     -> CDSTerminated node
	(Fun)         -> CDSFun node (getChild node 0) (getChild node 1)
	(Cons portc cons)
		      -> CDSCons node cons 
				[ getChild node n | n <- [0..(portc-1)]]

     getChild :: Int -> Int -> CDSSet
     getChild pnode pport =
	[ content
        | (pport',content) <- (!) mid_arr pnode
	, pport == pport'
	]

render  :: Int -> Bool -> CDS -> DOC
render prec par (CDSCons _ ":" [cds1,cds2]) =
	if (par && not needParen)  
	then doc -- dont use paren (..) because we dont want a grp here!
	else paren needParen doc
   where
	doc = grp (brk <> renderSet' 5 False cds1 <> text " : ") <>
	      renderSet' 4 True cds2
	needParen = prec > 4
render prec par (CDSCons _ "," cdss) | length cdss > 0 =
	nest 2 (text "(" <> foldl1 (\ a b -> a <> text ", " <> b)
			    (map renderSet cdss) <>
		text ")")
render prec par (CDSCons _ name cdss) =
	paren (length cdss > 0 && prec /= 0)
	      (nest 2
	         (text name <> foldr (<>) nil
			 	[ sep <> renderSet' 10 False cds
			 	| cds <- cdss 
			 	]
		 )
	      )

{- renderSet handles the various styles of CDSSet.
 -}

renderSet :: CDSSet -> DOC
renderSet = renderSet' 0 False

renderSet' :: Int -> Bool -> CDSSet -> DOC
renderSet' _ _      [] = text "_"
renderSet' prec par [cons@(CDSCons {})]    = render prec par cons
renderSet' prec par cdss		   = 
	nest 0 (text "{ " <> foldl1 (\ a b -> a <> line <>
				    text ", " <> b)
				    (map renderFn pairs) <>
	        line <> text "}")

   where
	pairs = nub (sort (findFn cdss))
	-- local nub for sorted lists
	nub []                  = []
	nub (a:a':as) | a == a' = nub (a' : as)
        nub (a:as)              = a : nub as

renderFn :: ([CDSSet],CDSSet) -> DOC
renderFn (args,res) 
	= grp  (nest 3 
		(text "\\ " <>
		 foldr (\ a b -> nest 0 (renderSet' 10 False a) <> sep <> b)
		       nil
		       args <> sep <>
		 text "-> " <> renderSet' 0 False res
		)
               )

findFn :: CDSSet -> [([CDSSet],CDSSet)]
findFn = foldr findFn' []

findFn' (CDSFun _ arg res) rest =
    case findFn res of
       [(args',res')] -> (arg : args', res') : rest
       _              -> ([arg], res) : rest
findFn' other rest = ([],[other]) : rest

renderTops []   = nil
renderTops tops = line <> foldr (<>) nil (map renderTop tops)

renderTop :: Output -> DOC
renderTop (OutLabel str set extras) =
	nest 2 (text ("-- " ++ str) <> line <>
		renderSet set
		<> renderTops extras) <> line

rmEntry :: CDS -> CDS
rmEntry (CDSNamed str set)   = CDSNamed str (rmEntrySet set)
rmEntry (CDSCons i str sets) = CDSCons i str (map rmEntrySet sets)
rmEntry (CDSFun i a b)       = CDSFun i (rmEntrySet a) (rmEntrySet b)
rmEntry (CDSTerminated i)    = CDSTerminated i
rmEntry (CDSEntered i)       = error "found bad CDSEntered"

rmEntrySet = map rmEntry . filter noEntered
  where
	noEntered (CDSEntered _) = False
	noEntered _              = True

simplifyCDS :: CDS -> CDS
simplifyCDS (CDSNamed str set) = CDSNamed str (simplifyCDSSet set)
simplifyCDS (CDSCons _ "throw" 
		  [[CDSCons _ "ErrorCall" set]]
	    ) = simplifyCDS (CDSCons 0 "error" set)
simplifyCDS cons@(CDSCons i str sets) = 
	case spotString [cons] of
	  Just str | not (null str) -> CDSCons 0 (show str) []
	  _ -> CDSCons 0 str (map simplifyCDSSet sets)

simplifyCDS (CDSFun i a b) = CDSFun 0 (simplifyCDSSet a) (simplifyCDSSet b)
	-- replace with 
	-- 	CDSCons i "->" [simplifyCDSSet a,simplifyCDSSet b]
	-- for turning off the function stuff.

simplifyCDS (CDSTerminated i) = (CDSCons 0 "<?>" [])

simplifyCDSSet = map simplifyCDS 

spotString :: CDSSet -> Maybe String
spotString [CDSCons _ ":"
		[[CDSCons _ str []]
		,rest
		]
	   ] 
	= do { ch <- case reads str of
	               [(ch,"")] -> return ch
                       _ -> Nothing
	     ; more <- spotString rest
	     ; return (ch : more)
	     }
spotString [CDSCons _ "[]" []] = return []
spotString other = Nothing

paren :: Bool -> DOC -> DOC
paren False doc = grp (nest 0 doc)
paren True  doc = grp (nest 0 (text "(" <> nest 0 doc <> brk <> text ")"))

sp :: DOC
sp = text " "

data Output = OutLabel String CDSSet [Output]
            | OutData  CDS
	      deriving (Eq,Ord)


commonOutput :: [Output] -> [Output]
commonOutput = sortBy byLabel
  where
     byLabel (OutLabel lab _ _) (OutLabel lab' _ _) = compare lab lab'
     byLabel OutLabel{} OutData{} = GT
     byLabel OutData{} OutLabel{} = LT
     byLabel data1 data2 = compare data1 data2

cdssToOutput :: CDSSet -> [Output]
cdssToOutput =  map cdsToOutput

cdsToOutput (CDSNamed name cdsset)
	    = OutLabel name res1 res2
  where
      res1 = [ cdss | (OutData cdss) <- res ]
      res2 = [ out  | out@(OutLabel {}) <- res ]
      res  = cdssToOutput cdsset
cdsToOutput cons@(CDSCons {}) = OutData cons
cdsToOutput    fn@(CDSFun {}) = OutData fn
\end{code}



%************************************************************************
%*									*
\subsection{A Pretty Printer}
%*									*
%************************************************************************

This pretty printer is based on Wadler's pretty printer.

\begin{code}
data DOC		= NIL			-- nil	  
			| DOC :<> DOC		-- beside 
			| NEST Int DOC
			| TEXT String
			| LINE			-- always "\n"
			| SEP			-- " " or "\n"
			| BREAK			-- ""  or "\n"
			| DOC :<|> DOC		-- choose one
			deriving (Eq,Show)
data Doc		= Nil
			| Text Int String Doc
			| Line Int Int Doc
			deriving (Show,Eq)


mkText			:: String -> Doc -> Doc
mkText s d		= Text (toplen d + length s) s d

mkLine			:: Int -> Doc -> Doc
mkLine i d		= Line (toplen d + i) i d

toplen			:: Doc -> Int
toplen Nil		= 0
toplen (Text w s x)	= w
toplen (Line w s x)	= 0

nil			= NIL
x <> y			= x :<> y
nest i x		= NEST i x
text s 			= TEXT s
line			= LINE
sep			= SEP
brk			= BREAK

fold x			= grp (brk <> x)

grp 			:: DOC -> DOC
grp x			= 
	case flatten x of
	  Just x' -> x' :<|> x
	  Nothing -> x

flatten 		:: DOC -> Maybe DOC
flatten	NIL		= return NIL
flatten (x :<> y)	= 
	do x' <- flatten x
	   y' <- flatten y
	   return (x' :<> y')
flatten (NEST i x)	= 
	do x' <- flatten x
	   return (NEST i x')
flatten (TEXT s)	= return (TEXT s)
flatten LINE		= Nothing		-- abort
flatten SEP		= return (TEXT " ")	-- SEP is space
flatten BREAK		= return NIL		-- BREAK is nil
flatten (x :<|> y)	= flatten x

layout 			:: Doc -> String
layout Nil		= ""
layout (Text _ s x)	= s ++ layout x
layout (Line _ i x)	= '\n' : replicate i ' ' ++ layout x

best :: Int -> Int -> DOC -> Doc
best w k doc = be w k [(0,doc)]

be 			:: Int -> Int -> [(Int,DOC)] -> Doc
be w k []		= Nil
be w k ((i,NIL):z)	= be w k z
be w k ((i,x :<> y):z)	= be w k ((i,x):(i,y):z)
be w k ((i,NEST j x):z) = be w k ((k+j,x):z)
be w k ((i,TEXT s):z)	= s `mkText` be w (k+length s) z
be w k ((i,LINE):z)	= i `mkLine` be w i z
be w k ((i,SEP):z)	= i `mkLine` be w i z
be w k ((i,BREAK):z)	= i `mkLine` be w i z
be w k ((i,x :<|> y):z) = better w k 
				(be w k ((i,x):z))
				(be w k ((i,y):z))

better			:: Int -> Int -> Doc -> Doc -> Doc
better w k x y		= if (w-k) >= toplen x then x else y

pretty			:: Int -> DOC -> String
pretty w x		= layout (best w 0 x)
\end{code}
