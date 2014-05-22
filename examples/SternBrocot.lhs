We need the Template Haskell extension to splice in the generated
Observable instances. We need the Rank2Types extionsion to be able
to specify parametrized types such as 'Tree a' with forall a.

> {-# LANGUAGE TemplateHaskell, Rank2Types #-}
> import Debug.Hoed.Observe

We use the Stern–Brocot tree as example. The Stern-Brocot tree is a
binary tree containing all rational numbers. The tree is infinit and
is therefore a nice example to demonstrate how laziness is handled.

To store the tree we use the following datatype, note that because
our definition is endless we do not actually use Leaf.

> data Tree a = Node a (Tree a) (Tree a)

The values in the tree will be fractional numbers:

> data Frac = Frac Int Int deriving Show

We use cache to store what the last seen up and to the left, and up and
to the right values are.

> data Cache a = Cache { v :: a
>                      , l :: a
>                      , r :: a
>                      }

The mediant is used to find which new number to insert between 2 exisiting
numbers.

> mediant :: Frac -> Frac -> Frac
> mediant (Frac p1 q1) (Frac p2 q2) = Frac (p1+p2) (q1+q2)

Definition of the sternbrocot tree:

> sternbrocot :: Tree Frac
> sternbrocot = sternbrocot' mediant
>
> sternbrocot' :: (Frac -> Frac -> Frac) -> Tree Frac
> sternbrocot' m = w_sternbrocot m Cache{v=(Frac 1 1), l=(Frac 0 1), r=(Frac 1 0)}
>
> w_sternbrocot :: (Frac -> Frac -> Frac) -> (Cache Frac) -> Tree Frac
> w_sternbrocot m Cache{v=v, l=l, r=r}
>  = Node v
>         (w_sternbrocot m Cache{v=m v l, l=l, r=v})
>         (w_sternbrocot m Cache{v=m v r, l=v, r=r})

The Stern-Brocot tree is sorted: all values in the left subtree are
smaller than the value of the current node and all values in the right subtree
are greater than the value in the current node.
This can be used to approximate a Float value by doing a binary search where
each next rational number is a better aproximation of the Float.

> toFrac :: Float -> Tree Frac -> Frac
> toFrac val (Node frac@(Frac p q) left right)
>  = case compare ((fromIntegral p) / (fromIntegral q)) val of
>       LT -> toFrac val right 
>       GT -> toFrac val left
>       EQ -> frac

We use template-haskell to observe Tree and the values stored in Tree.

> $(observedTypes "sternbrocot1" [ [t| forall a . Observable a => Tree a |]
>                                , [t| Frac |]
>                                ]
>  )
>
> frac1 = toFrac 0.6 ($(observe "sternbrocot1") sternbrocot)

Or to only observe which part of the tree is walked while ignoring
the values stored in the tree.

> $(observedTypes "sternbrocot2" [ [t| forall a . Tree a |]])
>
> frac2 = toFrac 0.6 ($(observe "sternbrocot2") sternbrocot)

Example main function:

> main = runO $ do print frac1
>                  print frac2
