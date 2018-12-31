This is a digital copy of the Haskell source included in my 2008
dissertation _Towards an efficient decision procedure for the
existential theory of the reals_.

I provide one branch (6.10.1) with the unmodified original code.
However, this will no longer compile with recent versions of GHC,
for the following reasons:

1.  Hierarchical modules are now required for the standard libraries unless you use a
compatibility option, and I missed one `import` back when they were
only recommended.

2.  The version of GHC I ran it under, 6.10.1, had a defective
implementation of the `Integer` GCD algorithm, which ultimately exacted
a significant performance cost.  As such, I wrote a substitute function
using a "backdoor" into the GHC codebase to make it as efficient
as it should be.  This backdoor no longer works, and is [no longer
necessary](https://ghc.haskell.org/trac/ghc/changeset/8827985d7ce902bfc916e4168049c9a46a1d7fe8/base).

3.  The GHC community has opted to
[remove](http://www.haskell.org/ghc/docs/7.4.1/html/users_guide/release-7-4-1.html#id3013571)
`Eq` and `Show` as superclasses of `Num`.  My polynomial extensions
library especially needs `Eq` to test for coefficients being zero.
Thus, many type signatures and instance declarations required
additional constraints.

I have also removed a superfluous dependency from the parser.
It still requires `parsec`, which is included in all modern GHC installs.

The master branch has been updated so it will compile and run
correctly under recent versions of GHC (tested under 7.4.1, 7.6.3,
7.8.3, 7.10.1, 8.0.2, 8.2.1, 8.4.3, and 8.6.3).  No attempt has been made to maintain the
code other than to make it work.


##  Abstract of the thesis

Many results in analysis can be stated in the limited language of
"real-closed fields".  In 1948, Tarski showed that one can decide in
a finite, albeit very long, time whether any sentence in this language
is true.  In 1975, Collins developed a decision procedure which, unlike
Tarski's, was feasible for solving many small problems, but, due to
a doubly exponential growth rate, could not handle very large ones.

By restricting to merely the existential fragment of this theory,
which still includes several interesting questions, several researchers
developed algorithms with only singly exponential growth, thus providing
hope for reaching problems beyond the limits of Collins' algorithm.
A 1991 paper by Hong, however, argued that the constants involved made
them useless in practice, as the runtime would be many millions of years.
His results suggested that for practical purposes this line of research
is a dead end.

But new approaches to the existential theory have been discovered since,
two major ones being that of Canny and that of Basu, Pollack, and Roy.
In this thesis, I show that while the latter suffers infeasibility
similar to the earlier algorithms, the former can be made to perform
well enough to solve small problems.

I do this by actually building a full implementation that is able to solve
the test case used by Hong, as well as several others, in on the order of
one second.  The implementation is based on Canny's method but uses novel
data structures and more recent algorithms for the various subproblems
that arise, with different versions and variations explained and compared.

Finally, I consider likely future developments, and conclude that,
despite the past negative indicators and their still limited reach,
the prospect for existential-theory decision procedures is bright.

