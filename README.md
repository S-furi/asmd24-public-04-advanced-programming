# asmd24-public-04-advanced-programming

## ADT-Verifier

`SetADT` axioms checking are performed in the corresponding [scalacheck suite](https://github.com/S-furi/asmd24-public-04-advanced-programming/blob/main/src/test/scala/lab04/SetADTCheck.scala).
An alternative implmentation of `SetADT` backed by a tree-like data structure has been implemented
by means of a Binary Search Tree. Being a tree that needs a meaning of `Ordering`, a new `SetADT` has been
implemented that just introduces a bound in the types that that set can have. In this way we can easily
use a BST as a underlying data structure.

If we would like to be agnositic about the ordering or the kind the set elements can have,
another possibile solution is to use trees that do not leverage ordering or values comparison,
like tries. On the other hand, we could still use BST but inserting values in the tree by their hashcode
(taking care of handling potential collisions), building a HashTree.

## Monad-Verifier

You can find the Monad-Verifier task [here](https://github.com/S-furi/monads-axioms-checker)
