[Real World Haskell](/)by Bryan O'Sullivan, Don Stewart, and John Goerzen
-------------------------------------------------------------------------

Chapter 26. Advanced library design: building a Bloom filter

[Prev](profiling-and-optimization.html)

[Next](sockets-and-syslog.html)

Chapter 26. Advanced library design: building a Bloom filter
------------------------------------------------------------

**Table of Contents**

[Introducing the Bloom
filter](advanced-library-design-building-a-bloom-filter.html#id679702)

[Use cases and package
layout](advanced-library-design-building-a-bloom-filter.html#id680015)

[Basic
design](advanced-library-design-building-a-bloom-filter.html#id680073)

[Unboxing, lifting, and
bottom](advanced-library-design-building-a-bloom-filter.html#bloomfilter.uarray)

[The ST
monad](advanced-library-design-building-a-bloom-filter.html#id680273)

[Designing an API for qualified
import](advanced-library-design-building-a-bloom-filter.html#id680447)

[Creating a mutable Bloom
filter](advanced-library-design-building-a-bloom-filter.html#id680590)

[The immutable
API](advanced-library-design-building-a-bloom-filter.html#id680809)

[Creating a friendly
interface](advanced-library-design-building-a-bloom-filter.html#id680975)

[Re-exporting names for
convenience](advanced-library-design-building-a-bloom-filter.html#id681040)

[Hashing
values](advanced-library-design-building-a-bloom-filter.html#id681143)

[Turning two hashes into
many](advanced-library-design-building-a-bloom-filter.html#id681716)

[Implementing the easy creation
function](advanced-library-design-building-a-bloom-filter.html#id681748)

[Creating a Cabal
package](advanced-library-design-building-a-bloom-filter.html#id681953)

[Dealing with different build
setups](advanced-library-design-building-a-bloom-filter.html#id682065)

[Compilation options, and interfacing to
C](advanced-library-design-building-a-bloom-filter.html#id682403)

[Testing with
QuickCheck](advanced-library-design-building-a-bloom-filter.html#id682639)

[Polymorphic
testing](advanced-library-design-building-a-bloom-filter.html#id682839)

[Writing Arbitrary instances for
ByteStrings](advanced-library-design-building-a-bloom-filter.html#id683014)

[Are suggested sizes
correct?](advanced-library-design-building-a-bloom-filter.html#id683175)

[Performance analysis and
tuning](advanced-library-design-building-a-bloom-filter.html#id683448)

[Profile-driven performance
tuning](advanced-library-design-building-a-bloom-filter.html#id683609)

[Exercises](advanced-library-design-building-a-bloom-filter.html#id684546)

Introducing the Bloom filter
----------------------------

A Bloom filter is a set-like data structure that is highly efficient in
its use of space. It only supports two operations: insertion and
membership querying. Unlike a normal set data structure, a Bloom filter
can give incorrect answers. If we query it to see whether an element
that we have inserted is present, it will answer affirmatively. If we
query for an element that we have *not* inserted, it *might* incorrectly
claim that the element is present.

For many applications, a low rate of false positives is tolerable. For
instance, the job of a network traffic shaper is to throttle bulk
transfers (e.g. BitTorrent) so that interactive sessions (such as
**ssh** sessions or games) see good response times. A traffic shaper
might use a Bloom filter to determine whether a packet belonging to a
particular session is bulk or interactive. If it misidentifies one in
ten thousand bulk packets as interactive and fails to throttle it,
nobody will notice.

The attraction of a Bloom filter is its space efficiency. If we want to
build a spell checker, and have a dictionary of half a million words, a
set data structure might consume 20 megabytes of space. A Bloom filter,
in contrast, would consume about half a megabyte, at the cost of missing
perhaps 1% of misspelled words.

Behind the scenes, a Bloom filter is remarkably simple. It consists of a
bit array and a handful of hash functions. We'll use *k* for the number
of hash functions. If we want to insert a value into the Bloom filter,
we compute *k* hashes of the value, and turn on those bits in the bit
array. If we want to see whether a value is present, we compute *k*
hashes, and check all of those bits in the array to see if they are
turned on.

To see how this works, let's say we want to insert the strings
`"foo"`{.code} and `"bar"`{.code} into a Bloom filter that is 8 bits
wide, and we have two hash functions.

1.  Compute the two hashes of `"foo"`{.code}, and get the values
    `1`{.code} and `6`{.code}.

2.  Set bits `1`{.code} and `6`{.code} in the bit array.

3.  Compute the two hashes of `"bar"`{.code}, and get the values
    `6`{.code} and `3`{.code}.

4.  Set bits `6`{.code} and `3`{.code} in the bit array.

This example should make it clear why we cannot remove an element from a
Bloom filter: both `"foo"`{.code} and `"bar"`{.code} resulted in bit 6
being set.

Suppose we now want to query the Bloom filter, to see whether the values
`"quux"`{.code} and `"baz"`{.code} are present.

1.  Compute the two hashes of `"quux"`{.code}, and get the values
    `4`{.code} and `0`{.code}.

2.  Check bit `4`{.code} in the bit array. It is not set, so
    `"quux"`{.code} cannot be present. We do not need to check bit
    `0`{.code}.

3.  Compute the two hashes of `"baz"`{.code}, and get the values
    `1`{.code} and `3`{.code}.

4.  Check bit `1`{.code} in the bit array. It is set, as is bit
    `3`{.code}, so we say that `"baz"`{.code} is present even though it
    is not. We have reported a false positive.

For a survey of some of the uses of Bloom filters in networking, see
[[Broder02](bibliography.html#bib.broder02 "[Broder02]")].

Use cases and package layout
----------------------------

Not all users of Bloom filters have the same needs. In some cases, it
suffices to create a Bloom filter in one pass, and only query it
afterwards. For other applications, we may need to continue to update
the Bloom filter after we create it. To accommodate these needs, we will
design our library with mutable and immutable APIs.

We will segregate the mutable and immutable APIs that we publish by
placing them in different modules: `BloomFilter`{.code} for the
immutable code, and `BloomFilter.Mutable`{.code} for the mutable code.

In addition, we will create several “helper” modules that won't provide
parts of the public API, but will keep the internal code cleaner.

Finally, we will ask the user of our API to provide a function that can
generate a number of hashes of an element. This function will have the
type a -\> [Word32]. We will use all of the hashes that this function
returns, so the list must not be infinite!

Basic design
------------

The data structure that we use for our Haskell Bloom filter is a direct
translation of the simple description we gave earlier: a bit array and a
function that computes hashes.

~~~~ {#Internal.hs:Bloom .programlisting}
-- file: BloomFilter/Internal.hs
module BloomFilter.Internal
    (
      Bloom(..)
    , MutBloom(..)
    ) where

import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Word (Word32)

data Bloom a = B {
      blmHash  :: (a -> [Word32])
    , blmArray :: UArray Word32 Bool
    }
~~~~

When we create our Cabal package, we will not be exporting this
`BloomFilter.Internal`{.code} module. It exists purely to let us control
the visibility of names. We will import `BloomFilter.Internal`{.code}
into both the mutable and immutable modules, but we will re-export from
each module only the type that is relevant to that module's API.

### Unboxing, lifting, and bottom

Unlike other Haskell arrays, a UArray contains *unboxed* values.

For a normal Haskell type, a value can be either fully evaluated, an
unevaluated thunk, or the special value ⊥, pronounced (and sometimes
written) “bottom”. The value ⊥ is a placeholder for a computation that
does not succeed. Such a computation could take any of several forms. It
could be an infinite loop; an application of `error`{.function}; or the
special value `undefined`{.function}.

A type that can contain ⊥ is referred to as *lifted*. All normal Haskell
types are lifted. In practice, this means that we can always write
`error "eek!"`{.code} or `undefined`{.code} in place of a normal
expression.

This ability to store thunks or ⊥ comes with a performance cost: it adds
an extra layer of indirection. To see why we need this indirection,
consider the Word32 type. A value of this type is a full 32 bits wide,
so on a 32-bit system, there is no way to directly encode the value ⊥
within 32 bits. The runtime system has to maintain, and check, some
extra data to track whether the value is ⊥ or not.

An unboxed value does away with this indirection. In doing so, it gains
performance, but sacrifices the ability to represent a thunk or ⊥. Since
it can be denser than a normal Haskell array, an array of unboxed values
is an excellent choice for numeric data and bits.

![[Note]](/support/figs/note.png)

Boxing and lifting

The counterpart of an unboxed type is a *boxed* type, which uses
indirection. All lifted types are boxed, but a few low-level boxed types
are not lifted. For instance, GHC's runtime system has a low-level array
type for which it uses boxing (i.e. it maintains a pointer to the
array). If it has a reference to such an array, it knows that the array
must exist, so it does not need to account for the possibility of ⊥.
This array type is thus boxed, but not lifted. Boxed but unlifted types
only show up at the lowest level of runtime hacking. We will never
encounter them in normal use.

GHC implements a UArray of Bool values by packing eight array elements
into each byte, so this type is perfect for our needs.

The ST monad
------------

Back in [the section called “Modifying array
elements”](barcode-recognition.html#barcode.array.mutable "Modifying array elements"),
we mentioned that modifying an immutable array is prohibitively
expensive, as it requires copying the entire array. Using a UArray does
not change this, so what can we do to reduce the cost to bearable
levels?

In an imperative language, we would simply modify the elements of the
array in place; this will be our approach in Haskell, too.

Haskell provides a special monad, named ST^[[58](#ftn.id680314)]^, which
lets us work safely with mutable state. Compared to the State monad, it
has some powerful added capabilities.

-   We can *thaw* an immutable array to give a mutable array; modify the
    mutable array in place; and *freeze* a new immutable array when we
    are done.

-   We have the ability to use *mutable references*. This lets us
    implement data structures that we can modify after construction, as
    in an imperative language. This ability is vital for some imperative
    data structures and algorithms, for which similarly efficient purely
    functional alternatives have not yet been discovered.

The IO monad also provides these capabilities. The major difference
between the two is that the ST monad is intentionally designed so that
we can *escape* from it back into pure Haskell code. We enter the ST
monad via the execution function `runST`{.function}, in the same way as
for most other Haskell monads (except IO, of course), and we escape by
returning from `runST`{.function}.

When we apply a monad's execution function, we expect it to behave
repeatably: given the same body and arguments, we must get the same
results every time. This also applies to `runST`{.function}. To achieve
this repeatability, the ST monad is more restrictive than the IO monad.
We cannot read or write files, create global variables, or fork threads.
Indeed, although we can create and work with mutable references and
arrays, the type system prevents them from escaping to the caller of
`runST`{.function}. A mutable array must be frozen into an immutable
array before we can return it, and a mutable reference cannot escape at
all.

Designing an API for qualified import
-------------------------------------

The public interfaces that we provide for working with Bloom filters are
worth a little discussion.

~~~~ {#Mutable.hs:Mutable .programlisting}
-- file: BloomFilter/Mutable.hs
module BloomFilter.Mutable
    (
      MutBloom
    , elem
    , notElem
    , insert
    , length
    , new
    ) where

import Control.Monad (liftM)
import Control.Monad.ST (ST)
import Data.Array.MArray (getBounds, newArray, readArray, writeArray)
import Data.Word (Word32)
import Prelude hiding (elem, length, notElem)

import BloomFilter.Internal (MutBloom(..))
~~~~

We export several names that clash with names exported by the Prelude.
This is deliberate: we expect users of our modules to import them with
qualified names. This reduces the burden on the memory of our users, as
they should already be familiar with the Prelude's `elem`{.function},
`notElem`{.function}, and `length`{.function} functions.

When we use a module written in this style, we might often import it
with a single-letter prefix, for instance as
`import qualified BloomFilter.Mutable as M`{.code}. This would allow us
to write `M.length`{.function}, which stays compact and readable.

Alternatively, we could import the module unqualified, and import the
Prelude while hiding the clashing names with
`import Prelude hiding (length)`{.code}. This is much less useful, as it
gives a reader skimming the code no local cue that they are *not*
actually seeing the Prelude's `length`{.function}.

Of course, we seem to be violating this precept in our own module's
header: we import the Prelude, and hide some of the names it exports.
There is a practical reason for this. We define a function named
`length`{.function}. If we export this from our module without first
hiding the Prelude's `length`{.function}, the compiler will complain
that it cannot tell whether to export our version of `length`{.function}
or the Prelude's.

While we could export the fully qualified name
`BloomFilter.Mutable.length`{.function} to eliminate the ambiguity, that
seems uglier in this case. This decision has no consequences for someone
using our module, just for ourselves as the authors of what ought to be
a “black box”, so there is little chance of confusion here.

Creating a mutable Bloom filter
-------------------------------

We put type declaration for our mutable Bloom filter in the
`BloomFilter.Internal`{.code} module, along with the immutable Bloom
type.

~~~~ {#Internal.hs:MutBloom .programlisting}
-- file: BloomFilter/Internal.hs
data MutBloom s a = MB {
      mutHash :: (a -> [Word32])
    , mutArray :: STUArray s Word32 Bool
    }
~~~~

The STUArray type gives us a mutable unboxed array that we can work with
in the ST monad. To create an STUArray, we use the `newArray`{.function}
function. The `new`{.function} function belongs in the
`BloomFilter.Mutable`{.code} function.

~~~~ {#Mutable.hs:new .programlisting}
-- file: BloomFilter/Mutable.hs
new :: (a -> [Word32]) -> Word32 -> ST s (MutBloom s a)
new hash numBits = MB hash `liftM` newArray (0,numBits-1) False
~~~~

Most of the methods of STUArray are actually implementations of the
MArray typeclass, which is defined in the `Data.Array.MArray`{.code}
module.

Our `length`{.function} function is slightly complicated by two factors.
We are relying on our bit array's record of its own bounds, and an
MArray instance's `getBounds`{.function} function has a monadic type. We
also have to add one to the answer, as the upper bound of the array is
one less than its actual length.

~~~~ {#Mutable.hs:length .programlisting}
-- file: BloomFilter/Mutable.hs
length :: MutBloom s a -> ST s Word32
length filt = (succ . snd) `liftM` getBounds (mutArray filt)
~~~~

To add an element to the Bloom filter, we set all of the bits indicated
by the hash function. We use the `mod`{.function} function to ensure
that all of the hashes stay within the bounds of our array, and isolate
our code that computes offsets into the bit array in one function.

~~~~ {#Mutable.hs:insert .programlisting}
-- file: BloomFilter/Mutable.hs
insert :: MutBloom s a -> a -> ST s ()
insert filt elt = indices filt elt >>=
                  mapM_ (\bit -> writeArray (mutArray filt) bit True)

indices :: MutBloom s a -> a -> ST s [Word32]
indices filt elt = do
  modulus <- length filt
  return $ map (`mod` modulus) (mutHash filt elt)
~~~~

Testing for membership is no more difficult. If every bit indicated by
the hash function is set, we consider an element to be present in the
Bloom filter.

~~~~ {#Mutable.hs:elem .programlisting}
-- file: BloomFilter/Mutable.hs
elem, notElem :: a -> MutBloom s a -> ST s Bool

elem elt filt = indices filt elt >>=
                allM (readArray (mutArray filt))

notElem elt filt = not `liftM` elem elt filt
~~~~

We need to write a small supporting function: a monadic version of
`all`{.function}, which we will call `allM`{.function}.

~~~~ {#Mutable.hs:allM .programlisting}
-- file: BloomFilter/Mutable.hs
allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p (x:xs) = do
  ok <- p x
  if ok
    then allM p xs
    else return False
allM _ [] = return True
~~~~

The immutable API
-----------------

Our interface to the immutable Bloom filter has the same structure as
the mutable API.

~~~~ {#BloomFilter.hs:module .programlisting}
-- file: ch26/BloomFilter.hs
module BloomFilter
    (
      Bloom
    , length
    , elem
    , notElem
    , fromList
    ) where

import BloomFilter.Internal
import BloomFilter.Mutable (insert, new)
import Data.Array.ST (runSTUArray)
import Data.Array.IArray ((!), bounds)
import Data.Word (Word32)
import Prelude hiding (elem, length, notElem)

length :: Bloom a -> Int
length = fromIntegral . len

len :: Bloom a -> Word32
len = succ . snd . bounds . blmArray

elem :: a -> Bloom a -> Bool
elt `elem` filt   = all test (blmHash filt elt)
  where test hash = blmArray filt ! (hash `mod` len filt)

notElem :: a -> Bloom a -> Bool
elt `notElem` filt = not (elt `elem` filt)
~~~~

We provide an easy-to-use means to create an immutable Bloom filter, via
a `fromList`{.function} function. This hides the ST monad from our
users, so that they only see the immutable type.

~~~~ {#BloomFilter.hs:fromList .programlisting}
-- file: ch26/BloomFilter.hs
fromList :: (a -> [Word32])    -- family of hash functions to use
         -> Word32             -- number of bits in filter
         -> [a]                -- values to populate with
         -> Bloom a
fromList hash numBits values =
    B hash . runSTUArray $
      do mb <- new hash numBits
         mapM_ (insert mb) values
         return (mutArray mb)
~~~~

The key to this function is `runSTUArray`{.function}. We mentioned
earlier that in order to return an immutable array from the ST monad, we
must freeze a mutable array. The `runSTUArray`{.function} function
combines execution with freezing. Given an action that returns an
STUArray, it executes the action using `runST`{.function}; freezes the
STUArray that it returns; and returns that as a UArray.

The `MArray`{.code} typeclass provides a `freeze`{.function} function
that we could use instead, but `runSTUArray`{.function} is both more
convenient and more efficient. The efficiency lies in the fact that
`freeze`{.function} must copy the underlying data from the STUArray to
the new UArray, to ensure that subsequent modifications of the STUArray
cannot affect the contents of the UArray. Thanks to the type system,
`runSTUArray`{.function} can guarantee that an STUArray is no longer
accessible when it uses it to create a UArray. It can thus share the
underlying contents between the two arrays, avoiding the copy.

Creating a friendly interface
-----------------------------

Although our immutable Bloom filter API is straightforward to use once
we have created a Bloom value, the `fromList`{.function} function leaves
some important decisions unresolved. We still have to choose a function
that can generate many hash values, and determine what the capacity of a
Bloom filter should be.

~~~~ {#Easy.hs:easyList.type .programlisting}
-- file: BloomFilter/Easy.hs
easyList :: (Hashable a)
         => Double        -- false positive rate (between 0 and 1)
         -> [a]           -- values to populate the filter with
         -> Either String (B.Bloom a)
~~~~

Here is a possible “friendlier” way to create a Bloom filter. It leaves
responsibility for hashing values in the hands of a typeclass, Hashable.
It lets us configure the Bloom filter based on a parameter that is
easier to understand, namely the rate of false positives that we are
willing to tolerate. And it chooses the size of the filter for us, based
on the desired false positive rate and the number of elements in the
input list.

This function will of course not always be usable: for example, it will
fail if the length of the input list is too long. However, its
simplicity rounds out the other interfaces we provide. It lets us
provide our users with a range of control over creation, from entirely
imperative to completely declarative.

### Re-exporting names for convenience

In the export list for our module, we re-export some names from the base
`BloomFilter`{.code} module. This allows casual users to import only the
`BloomFilter.Easy`{.code} module, and have access to all of the types
and functions they are likely to need.

If we import both `BloomFilter.Easy`{.code} and `BloomFilter`{.code},
you might wonder what will happen if we try to use a name exported by
both. We already know that if we import `BloomFilter`{.code} unqualified
and try to use `length`{.function}, GHC will issue an error about
ambiguity, because the Prelude also makes the name `length`{.function}
available.

The Haskell standard requires an implementation to be able to tell when
several names refer to the same “thing”. For instance, the Bloom type is
exported by `BloomFilter`{.code} and `BloomFilter.Easy`{.code}. If we
import both modules and try to use Bloom, GHC will be able to see that
the Bloom re-exported from `BloomFilter.Easy`{.code} is the same as the
one exported from `BloomFilter`{.code}, and it will not report an
ambiguity.

### Hashing values

A Bloom filter depends on fast, high-quality hashes for good performance
and a low false positive rate. It is surprisingly difficult to write a
general purpose hash function that has both of these properties.

Luckily for us, a fellow named Bob Jenkins developed some hash functions
that have exactly these properties, and he placed the code in the public
domain at
[http://burtleburtle.net/bob/hash/doobs.html](http://burtleburtle.net/bob/hash/doobs.html)^[[59](#ftn.id681170)]^
. He wrote his hash functions in C, so we can easily use the FFI to
create bindings to them. The specific source file that we need from that
site is named
[`lookup3.c`{.filename}](http://burtleburtle.net/bob/c/lookup3.c). We
create a `cbits`{.filename} directory and download it to there.

![[Note]](/support/figs/note.png)

A little editing

On line 36 of the copy of `lookup3.c`{.filename} that you just
downloaded, there is a macro named `SELF_TEST`{.code} defined. To use
this source file as a library, you *must* delete this line or comment it
out. If you forget to do so, the `main`{.function} function defined near
the bottom of the file will supersede the `main`{.function} of any
Haskell program you link this library against.

There remains one hitch: we will frequently need seven or even ten hash
functions. We really don't want to scrape together that many different
functions, and fortunately we do not need to: in most cases, we can get
away with just two. We will see how shortly. The Jenkins hash library
includes two functions, `hashword2`{.function} and
`hashlittle2`{.function}, that compute two hash values. Here is a C
header file that describes the APIs of these two functions. We save this
to `cbits/lookup3.h`{.filename}.

~~~~ {#lookup3.h:header .programlisting}
/* save this file as lookup3.h */

#ifndef _lookup3_h
#define _lookup3_h

#include <stdint.h>
#include <sys/types.h>

/* only accepts uint32_t aligned arrays of uint32_t */
void hashword2(const uint32_t *key,  /* array of uint32_t */
           size_t length,        /* number of uint32_t values */
           uint32_t *pc,         /* in: seed1, out: hash1 */
           uint32_t *pb);        /* in: seed2, out: hash2 */

/* handles arbitrarily aligned arrays of bytes */
void hashlittle2(const void *key,   /* array of bytes */
         size_t length,     /* number of bytes */
         uint32_t *pc,      /* in: seed1, out: hash1 */
         uint32_t *pb);     /* in: seed2, out: hash2 */

#endif /* _lookup3_h */
~~~~

A “salt” is a value that perturbs the hash value that the function
computes. If we hash the same value with two different salts, we will
get two different hashes. Since these functions compute two hashes, they
accept two salts.

Here are our Haskell bindings to these functions.

~~~~ {#Hash.hs:jenkins .programlisting}
-- file: BloomFilter/Hash.hs
{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
module BloomFilter.Hash
    (
      Hashable(..)
    , hash
    , doubleHash
    ) where

import Data.Bits ((.&.), shiftR)
import Foreign.Marshal.Array (withArrayLen)
import Control.Monad (foldM)
import Data.Word (Word32, Word64)
import Foreign.C.Types (CSize)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable, peek, sizeOf)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall unsafe "lookup3.h hashword2" hashWord2
    :: Ptr Word32 -> CSize -> Ptr Word32 -> Ptr Word32 -> IO ()

foreign import ccall unsafe "lookup3.h hashlittle2" hashLittle2
    :: Ptr a -> CSize -> Ptr Word32 -> Ptr Word32 -> IO ()
~~~~

We have specified that the definitions of the functions can be found in
the `lookup3.h`{.filename} header file that we just created.

For convenience and efficiency, we will combine the 32-bit salts
consumed, and the hash values computed, by the Jenkins hash functions
into a single 64-bit value.

~~~~ {#Hash.hs:hashIO .programlisting}
-- file: BloomFilter/Hash.hs
hashIO :: Ptr a    -- value to hash
       -> CSize    -- number of bytes
       -> Word64   -- salt
       -> IO Word64
hashIO ptr bytes salt =
    with (fromIntegral salt) $ \sp -> do
      let p1 = castPtr sp
          p2 = castPtr sp `plusPtr` 4
      go p1 p2
      peek sp
  where go p1 p2
          | bytes .&. 3 == 0 = hashWord2 (castPtr ptr) words p1 p2
          | otherwise        = hashLittle2 ptr bytes p1 p2
        words = bytes `div` 4
~~~~

Without explicit types around to describe what is happening, the above
code is not completely obvious. The `with`{.function} function allocates
room for the salt on the C stack, and stores the current salt value in
there, so `sp`{.varname} is a Ptr Word64. The pointers `p1`{.varname}
and `p2`{.varname} are Ptr Word32; `p1`{.varname} points at the low word
of `sp`{.varname}, and `p2`{.varname} at the high word. This is how we
chop the single Word64 salt into two Ptr Word32 parameters.

Because all of our data pointers are coming from the Haskell heap, we
know that they will be aligned on an address that is safe to pass to
either `hashWord2`{.function} (which only accepts 32-bit-aligned
addresses) or `hashLittle2`{.function}. Since `hashWord32`{.function} is
the faster of the two hashing functions, we call it if our data is a
multiple of 4 bytes in size, otherwise `hashLittle2`{.function}.

Since the C hash function will write the computed hashes into
`p1`{.varname} and `p2`{.varname}, we only need to `peek`{.function} the
pointer `sp`{.varname} to retrieve the computed hash.

We don't want clients of this module to be stuck fiddling with low-level
details, so we use a typeclass to provide a clean, high-level interface.

~~~~ {#Hash.hs:Hashable .programlisting}
-- file: BloomFilter/Hash.hs
class Hashable a where
    hashSalt :: Word64        -- ^ salt
             -> a             -- ^ value to hash
             -> Word64

hash :: Hashable a => a -> Word64
hash = hashSalt 0x106fc397cf62f64d3
~~~~

We also provide a number of useful implementations of this typeclass. To
hash basic types, we must write a little boilerplate code.

~~~~ {#Hash.hs:hashStorable .programlisting}
-- file: BloomFilter/Hash.hs
hashStorable :: Storable a => Word64 -> a -> Word64
hashStorable salt k = unsafePerformIO . with k $ \ptr ->
                      hashIO ptr (fromIntegral (sizeOf k)) salt

instance Hashable Char   where hashSalt = hashStorable
instance Hashable Int    where hashSalt = hashStorable
instance Hashable Double where hashSalt = hashStorable
~~~~

We might prefer to use the Storable typeclass to write just one
declaration, as follows:

~~~~ {#Hash.hs:Storable .programlisting}
-- file: BloomFilter/Hash.hs
instance Storable a => Hashable a where
    hashSalt = hashStorable
~~~~

Unfortunately, Haskell does not permit us to write instances of this
form, as allowing them would make the type system *undecidable*: they
can cause the compiler's type checker to loop infinitely. This
restriction on undecidable types forces us to write out individual
declarations. It does not, however, pose a problem for a definition such
as this one.

~~~~ {#Hash.hs:hashList .programlisting}
-- file: BloomFilter/Hash.hs
hashList :: (Storable a) => Word64 -> [a] -> IO Word64
hashList salt xs =
    withArrayLen xs $ \len ptr ->
      hashIO ptr (fromIntegral (len * sizeOf x)) salt
  where x = head xs

instance (Storable a) => Hashable [a] where
    hashSalt salt xs = unsafePerformIO $ hashList salt xs
~~~~

The compiler will accept this instance, so we gain the ability to hash
values of many list types^[[60](#ftn.id681600)]^. Most importantly,
since Char is an instance of Storable, we can now hash String values.

For tuple types, we take advantage of function composition. We take a
salt in at one end of the composition pipeline, and use the result of
hashing each tuple element as the salt for the next element.

~~~~ {#Hash.hs:hash2 .programlisting}
-- file: BloomFilter/Hash.hs
hash2 :: (Hashable a) => a -> Word64 -> Word64
hash2 k salt = hashSalt salt k

instance (Hashable a, Hashable b) => Hashable (a,b) where
    hashSalt salt (a,b) = hash2 b . hash2 a $ salt

instance (Hashable a, Hashable b, Hashable c) => Hashable (a,b,c) where
    hashSalt salt (a,b,c) = hash2 c . hash2 b . hash2 a $ salt
~~~~

To hash ByteString types, we write special instances that plug straight
into the internals of the ByteString types. This gives us excellent
hashing performance.

~~~~ {#Hash.hs:hashSB .programlisting}
-- file: BloomFilter/Hash.hs
hashByteString :: Word64 -> Strict.ByteString -> IO Word64
hashByteString salt bs = Strict.useAsCStringLen bs $ \(ptr, len) ->
                         hashIO ptr (fromIntegral len) salt

instance Hashable Strict.ByteString where
    hashSalt salt bs = unsafePerformIO $ hashByteString salt bs

rechunk :: Lazy.ByteString -> [Strict.ByteString]
rechunk s
    | Lazy.null s = []
    | otherwise   = let (pre,suf) = Lazy.splitAt chunkSize s
                    in  repack pre : rechunk suf
    where repack    = Strict.concat . Lazy.toChunks
          chunkSize = 64 * 1024

instance Hashable Lazy.ByteString where
    hashSalt salt bs = unsafePerformIO $
                       foldM hashByteString salt (rechunk bs)
~~~~

Since a lazy ByteString is represented as a series of chunks, we must be
careful with the boundaries between those chunks. The string
`"foobar"`{.code} can be represented in five different ways, for example
`["fo","obar"]`{.code} or `["foob","ar"]`{.code}. This is invisible to
most users of the type, but not to us since we use the underlying chunks
directly. Our `rechunk`{.function} function ensures that the chunks we
pass to the C hashing code are a uniform 64KB in size, so that we will
give consistent hash values no matter where the original chunk
boundaries lie.

### Turning two hashes into many

As we mentioned earlier, we need many more than two hashes to make
effective use of a Bloom filter. We can use a technique called *double
hashing* to combine the two values computed by the Jenkins hash
functions, yielding many more hashes. The resulting hashes are of good
enough quality for our needs, and far cheaper than computing many
distinct hashes.

~~~~ {#Hash.hs:doubleHash .programlisting}
-- file: BloomFilter/Hash.hs
doubleHash :: Hashable a => Int -> a -> [Word32]
doubleHash numHashes value = [h1 + h2 * i | i <- [0..num]]
    where h   = hashSalt 0x9150a946c4a8966e value
          h1  = fromIntegral (h `shiftR` 32) .&. maxBound
          h2  = fromIntegral h
          num = fromIntegral numHashes
~~~~

### Implementing the easy creation function

In the `BloomFilter.Easy`{.code} module, we use our new
`doubleHash`{.function} function to define the `easyList`{.function}
function whose type we defined earlier.

~~~~ {#Easy.hs:easyList .programlisting}
-- file: BloomFilter/Easy.hs
module BloomFilter.Easy
    (
      suggestSizing
    , sizings
    , easyList

    -- re-export useful names from BloomFilter
    , B.Bloom
    , B.length
    , B.elem
    , B.notElem
    ) where

import BloomFilter.Hash (Hashable, doubleHash)
import Data.List (genericLength)
import Data.Maybe (catMaybes)
import Data.Word (Word32)
import qualified BloomFilter as B

easyList errRate values =
    case suggestSizing (genericLength values) errRate of
      Left err            -> Left err
      Right (bits,hashes) -> Right filt
        where filt = B.fromList (doubleHash hashes) bits values
~~~~

This depends on a `suggestSizing`{.function} function that estimates the
best combination of filter size and number of hashes to compute, based
on our desired false positive rate and the maximum number of elements
that we expect the filter to contain.

~~~~ {#Easy.hs:suggestSizing .programlisting}
-- file: BloomFilter/Easy.hs
suggestSizing
    :: Integer       -- expected maximum capacity
    -> Double        -- desired false positive rate
    -> Either String (Word32,Int) -- (filter size, number of hashes)
suggestSizing capacity errRate
    | capacity <= 0                = Left "capacity too small"
    | errRate <= 0 || errRate >= 1 = Left "invalid error rate"
    | null saneSizes               = Left "capacity too large"
    | otherwise                    = Right (minimum saneSizes)
  where saneSizes = catMaybes . map sanitize $ sizings capacity errRate
        sanitize (bits,hashes)
          | bits > maxWord32 - 1 = Nothing
          | otherwise            = Just (ceiling bits, truncate hashes)
          where maxWord32 = fromIntegral (maxBound :: Word32)

sizings :: Integer -> Double -> [(Double, Double)]
sizings capacity errRate =
    [(((-k) * cap / log (1 - (errRate ** (1 / k)))), k) | k <- [1..50]]
  where cap = fromIntegral capacity
~~~~

We perform some rather paranoid checking. For instance, the
`sizings`{.function} function suggests pairs of array size and hash
count, but it does not validate its suggestions. Since we use 32-bit
hashes, we must filter out suggested array sizes that are too large.

In our `suggestSizing`{.function} function, we attempt to minimise only
the size of the bit array, without regard for the number of hashes. To
see why, let us interactively explore the relationship between array
size and number of hashes.

Suppose we want to insert 10 million elements into a Bloom filter, with
a false positive rate of 0.1%.

~~~~ {#sizings.ghci:kbytes .screen}
ghci> let kbytes (bits,hashes) = (ceiling bits `div` 8192, hashes)
ghci> :m +BloomFilter.Easy Data.List
Could not find module `BloomFilter.Easy':
  Use -v to see a list of the files searched for.
ghci> mapM_ (print . kbytes) . take 10 . sort $ sizings 10000000 0.001

<interactive>:1:35: Not in scope: `sort'

<interactive>:1:42: Not in scope: `sizings'
~~~~

We achieve the most compact table (just over 17KB) by computing 10
hashes. If we really were hashing the data repeatedly, we could reduce
the number of hashes to 7 at a cost of 5% in space. Since we are using
Jenkins's hash functions which compute two hashes in a single pass, and
double hashing the results to produce additional hashes, the cost to us
of computing extra those hashes is tiny, so we will choose the smallest
table size.

If we increase our tolerance for false positives tenfold, to 1%, the
amount of space and the number of hashes we need drop, though not by
easily predictable amounts.

~~~~ {#sizings.ghci:kbytes2 .screen}
ghci> mapM_ (print . kbytes) . take 10 . sort $ sizings 10000000 0.01

<interactive>:1:35: Not in scope: `sort'

<interactive>:1:42: Not in scope: `sizings'
~~~~

Creating a Cabal package
------------------------

We have created a moderately complicated library, with four public
modules and one internal module. To turn this into a package that we can
easily redistribute, we create a `rwh-bloomfilter.cabal`{.filename}
file.

Cabal allows us to describe several libraries in a single package. A
`.cabal`{.filename} file begins with information that is common to all
of the libraries, which is followed by a distinct section for each
library.

~~~~ {#rwh-bloomfilter.cabal:header .programlisting}
Name:               rwh-bloomfilter
Version:            0.1
License:            BSD3
License-File:       License.txt
Category:           Data
Stability:          experimental
Build-Type:         Simple
~~~~

As we are bundling some C code with our library, we tell Cabal about our
C source files.

~~~~ {#rwh-bloomfilter.cabal:extraSourceFiles .programlisting}
Extra-Source-Files: cbits/lookup3.c cbits/lookup3.h
~~~~

The `extra-source-files`{.code} directive has no effect on a build: it
directs Cabal to bundle some extra files if we run **runhaskell Setup
sdist** to create a source tarball for redistribution.

![[Tip]](/support/figs/tip.png)

Property names are case insensitive

When reading a property (the text before a “`:`{.literal}” character),
Cabal ignores case, so it treats `extra-source-files`{.code} and
`Extra-Source-Files`{.code} as the same.

### Dealing with different build setups

Prior to 2007, the standard Haskell libraries were organised in a
handful of large packages, of which the biggest was named
`base`{.filename}. This organisation tied many unrelated libraries
together, so the Haskell community split the `base`{.filename} package
up into a number of more modular libraries. For instance, the array
types migrated from `base`{.filename} into a package named
`array`{.filename}.

A Cabal package needs to specify the other packages that it needs to
have present in order to build. This makes it possible for Cabal's
command line interface automatically download and build a package's
dependencies, if necessary. We would like our code to work with as many
versions of GHC as possible, regardless of whether they have the modern
layout of `base`{.filename} and numerous other packages. We thus need to
be able to specify that we depend on the `array`{.filename} package if
it is present, and `base`{.filename} alone otherwise.

Cabal provides a generic *configurations* feature, which we can use to
selectively enable parts of a `.cabal`{.filename} file. A build
configuration is controlled by a Boolean-valued *flag*. If it is
`True`{.code}, the text following an `if flag`{.code} directive is used,
otherwise the text following the associated `else`{.code} is used.

~~~~ {#rwh-bloomfilter.cabal:splitBase .programlisting}
Cabal-Version:      >= 1.2

Flag split-base
  Description: Has the base package been split up?
  Default: True

Flag bytestring-in-base
  Description: Is ByteString in the base or bytestring package?
  Default: False
~~~~

-   The configurations feature was introduced in version 1.2 of Cabal,
    so we specify that our package cannot be built with an older
    version.

-   The meaning of the `split-base`{.code} flag should be
    self-explanatory.

-   The `bytestring-in-base`{.code} flag deals with a more torturous
    history. When the `bytestring`{.filename} package was first created,
    it was bundled with GHC 6.4, and kept separate from the
    `base`{.filename} package. In GHC 6.6, it was incorporated into the
    `base`{.filename} package, but it became independent again when the
    `base`{.filename} package was split before the release of GHC 6.8.1.

These flags are usually invisible to people building a package, because
Cabal handles them automatically. Before we explain what happens, it
will help to see the beginning of the `Library`{.code} section of our
`.cabal`{.code} file.

~~~~ {#rwh-bloomfilter.cabal:library .programlisting}
Library
  if flag(bytestring-in-base)
    -- bytestring was in base-2.0 and 2.1.1
    Build-Depends: base >= 2.0 && < 2.2
  else
    -- in base 1.0 and 3.0, bytestring is a separate package
    Build-Depends: base < 2.0 || >= 3, bytestring >= 0.9

  if flag(split-base)
    Build-Depends: base >= 3.0, array
  else
    Build-Depends: base < 3.0
~~~~

Cabal creates a package description with the default values of the flags
(a missing default is assumed to be `True`{.code}). If that
configuration can be built (e.g. because all of the needed package
versions are available), it will be used. Otherwise, Cabal tries
different combinations of flags until it either finds a configuration
that it can build or exhausts the alternatives.

For example, if we were to begin with both `split-base`{.code} and
`bytestring-in-base`{.code} set to `True`{.code}, Cabal would select the
following package dependencies.

~~~~ {#inconsistent.cabal:bogus .programlisting}
Build-Depends: base >= 2.0 && < 2.2
Build-Depends: base >= 3.0, array
~~~~

The `base`{.filename} package cannot simultaneously be newer than
`3.0`{.code} and older than `2.2`{.code}, so Cabal would reject this
configuration as inconsistent. For a modern version of GHC, after a few
attempts it would discover this configuration that will indeed build.

~~~~ {#inconsistent.cabal:modern .programlisting}
-- in base 1.0 and 3.0, bytestring is a separate package
Build-Depends: base < 2.0 || >= 3, bytestring >= 0.9
Build-Depends: base >= 3.0, array
~~~~

When we run **runhaskell Setup configure**, we can manually specify the
values of flags via the `--flag`{.option} option, though we will rarely
need to do so in practice.

### Compilation options, and interfacing to C

Continuing with our `.cabal`{.filename} file, we fill out the remaining
details of the Haskell side of our library. If we enable profiling when
we build, we want all of our top-level functions to show up in any
profiling output.

~~~~ {#rwh-bloomfilter.cabal:modules .programlisting}
  GHC-Prof-Options: -auto-all
~~~~

The `Other-Modules`{.code} property lists Haskell modules that are
private to the library. Such modules will be invisible to code that uses
this package.

When we build this package with GHC, Cabal will pass the options from
the `GHC-Options`{.code} property to the compiler.

The `-O2`{.option} option makes GHC optimise our code aggressively. Code
compiled without optimisation is very slow, so we should always use
`-O2`{.option} for production code.

To help ourselves to write cleaner code, we usually add the
`-Wall`{.option} option, which enables all of GHC's warnings. This will
cause GHC to issue complaints if it encounters potential problems, such
as overlapping patterns; function parameters that are not used; and a
myriad of other potential stumbling blocks. While it is often safe to
ignore these warnings, we generally prefer to fix up our code to
eliminate them. The small added effort usually yields code that is
easier to read and maintain.

When we compile with `-fvia-C`{.option}, GHC will generate C code and
use the system's C compiler to compile it, instead of going straight to
assembly language as it usually does. This slows compilation down, but
sometimes the C compiler can further improve GHC's optimised code, so it
can be worthwhile.

We include `-fvia-C`{.option} here mainly to show how to make
compilation with it work.

~~~~ {#rwh-bloomfilter.cabal:cbits .programlisting}
  C-Sources:        cbits/lookup3.c
  CC-Options:       -O3
  Include-Dirs:     cbits
  Includes:         lookup3.h
  Install-Includes: lookup3.h
~~~~

For the `C-Sources`{.code} property, we only need to list files that
must be compiled into our library. The `CC-Options`{.code} property
contains options for the C compiler (`-O3`{.option} specifies a high
level of optimisation). Because our FFI bindings for the Jenkins hash
functions refer to the `lookup3.h`{.filename} header file, we need to
tell Cabal where to find the header file. We must also tell it to
*install* the header file (`Install-Includes`{.code}), as otherwise
client code will fail to find the header file when we try to build it.

![[Tip]](/support/figs/tip.png)

The value of -fvia-C with the FFI

Compiling with `-fvia-C`{.option} has a useful safety benefit when we
write FFI bindings. If we mention a header file in an FFI declaration
(e.g. `foreign import         "string.h memcpy")`{.code}, the C compiler
will typecheck the generated Haskell code and ensure that its invocation
of the C function is consistent with the C function's prototype in the
header file.

If we do not use `-fvia-C`{.option}, we lose that additional layer of
safety. This makes it easy to let simple C type errors slip into our
Haskell code. As an example, on most 64-bit machines, a CInt is 32 bits
wide, and a CSize is 64 bits wide. If we accidentally use one type to
describe a parameter for an FFI binding when we should use the other, we
are likely to cause data corruption or a crash.

Testing with QuickCheck
-----------------------

Before we pay any attention to performance, we want to establish that
our Bloom filter behaves correctly. We can easily use QuickCheck to test
some basic properties.

~~~~ {#BloomCheck.hs:BloomCheck .programlisting}
-- file: examples/BloomCheck.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import BloomFilter.Hash (Hashable)
import Data.Word (Word8, Word32)
import System.Random (Random(..), RandomGen)
import Test.QuickCheck
import qualified BloomFilter.Easy as B
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
~~~~

We will not use the normal `quickCheck`{.function} function to test our
properties, as the 100 test inputs that it generates do not provide much
coverage.

~~~~ {#BloomCheck.hs:handyCheck .programlisting}
-- file: examples/BloomCheck.hs
handyCheck :: Testable a => Int -> a -> IO ()
handyCheck limit = check defaultConfig {
                     configMaxTest = limit
                   , configEvery   = \_ _ -> ""
                   }
~~~~

Our first task is to ensure that if we add a value to a Bloom filter, a
subsequent membership test will always report it as present, no matter
what the chosen false positive rate or input value is.

We will use the `easyList`{.function} function to create a Bloom filter.
The Random instance for Double generates numbers in the range zero to
one, so QuickCheck can *nearly* supply us with arbitrary false positive
rates.

However, we need to ensure that both zero and one are excluded from the
false positives we test with. QuickCheck gives us two ways to do this.

-   By *construction*: we specify the range of valid values to generate.
    QuickCheck provides a `forAll`{.function} combinator for this
    purpose.

-   By *elimination*: when QuickCheck generates an arbitrary value for
    us, we filter out those that do not fit our criteria, using the
    `(==>)`{.function} operator. If we reject a value in this way, a
    test will appear to succeed.

If we can choose either method, it is always preferable to take the
constructive approach. To see why, suppose that QuickCheck generates
1,000 arbitrary values for us, and we filter out 800 as unsuitable for
some reason. We will *appear* to run 1,000 tests, but only 200 will
actually do anything useful.

Following this idea, when we generate desired false positive rates, we
could eliminate zeroes and ones from whatever QuickCheck gives us, but
instead we construct values in an interval that will always be valid.

~~~~ {#BloomCheck.hs:prop_one_present .programlisting}
-- file: examples/BloomCheck.hs
falsePositive :: Gen Double
falsePositive = choose (epsilon, 1 - epsilon)
    where epsilon = 1e-6

(=~>) :: Either a b -> (b -> Bool) -> Bool
k =~> f = either (const True) f k

prop_one_present _ elt =
    forAll falsePositive $ \errRate ->
      B.easyList errRate [elt] =~> \filt ->
        elt `B.elem` filt
~~~~

Our small combinator, `(=~>)`{.function}, lets us filter out failures of
`easyList`{.function}: if it fails, the test automatically passes.

### Polymorphic testing

QuickCheck requires properties to be *monomorphic*. Since we have many
different hashable types that we would like to test, we would very much
like to avoid having to write the same test in many different ways.

Notice that although our `prop_one_present`{.function} function is
polymorphic, it ignores its first argument. We use this to simulate
monomorphic properties, as follows.

~~~~ {#bloomCheck.ghci:monomorphic .screen}
ghci> :load BloomCheck

BloomCheck.hs:9:17:
    Could not find module `BloomFilter.Easy':
      Use -v to see a list of the files searched for.
Failed, modules loaded: none.
ghci> :t prop_one_present

<interactive>:1:0: Not in scope: `prop_one_present'
ghci> :t prop_one_present (undefined :: Int)   

<interactive>:1:0: Not in scope: `prop_one_present'
~~~~

We can supply any value as the first argument to
`prop_one_present`{.function}. All that matters is its *type*, as the
same type will be used for the first element of the second argument.

~~~~ {#bloomCheck.ghci:one .screen}
ghci> handyCheck 5000 $ prop_one_present (undefined :: Int)

<interactive>:1:0: Not in scope: `handyCheck'

<interactive>:1:18: Not in scope: `prop_one_present'
ghci> handyCheck 5000 $ prop_one_present (undefined :: Double)

<interactive>:1:0: Not in scope: `handyCheck'

<interactive>:1:18: Not in scope: `prop_one_present'
~~~~

If we populate a Bloom filter with many elements, they should all be
present afterwards.

~~~~ {#BloomCheck.hs:prop_all_present .programlisting}
-- file: examples/BloomCheck.hs
prop_all_present _ xs =
    forAll falsePositive $ \errRate ->
      B.easyList errRate xs =~> \filt ->
        all (`B.elem` filt) xs
~~~~

This test also succeeds.

~~~~ {#bloomCheck.ghci:all .screen}
ghci> handyCheck 2000 $ prop_all_present (undefined :: Int)

<interactive>:1:0: Not in scope: `handyCheck'

<interactive>:1:18: Not in scope: `prop_all_present'
~~~~

### Writing Arbitrary instances for ByteStrings

The QuickCheck library does not provide Arbitrary instances for
ByteString types, so we must write our own. Rather than create a
ByteString directly, we will use a `pack`{.function} function to create
one from a [Word8].

~~~~ {#BloomCheck.hs:ByteString .programlisting}
-- file: examples/BloomCheck.hs
instance Arbitrary Lazy.ByteString where
    arbitrary = Lazy.pack `fmap` arbitrary
    coarbitrary = coarbitrary . Lazy.unpack

instance Arbitrary Strict.ByteString where
    arbitrary = Strict.pack `fmap` arbitrary
    coarbitrary = coarbitrary . Strict.unpack
~~~~

Also missing from QuickCheck are Arbitrary instances for the fixed-width
types defined in `Data.Word`{.code} and `Data.Int`{.code}. We need to at
least create an Arbitrary instance for Word8.

~~~~ {#BloomCheck.hs:Word8 .programlisting}
-- file: examples/BloomCheck.hs
instance Random Word8 where
  randomR = integralRandomR
  random = randomR (minBound, maxBound)

instance Arbitrary Word8 where
    arbitrary = choose (minBound, maxBound)
    coarbitrary = integralCoarbitrary
~~~~

We support these instances with a few common functions so that we can
reuse them when writing instances for other integral types.

~~~~ {#BloomCheck.hs:Word32 .programlisting}
-- file: examples/BloomCheck.hs
integralCoarbitrary n =
    variant $ if m >= 0 then 2*m else 2*(-m) + 1
  where m = fromIntegral n

integralRandomR (a,b) g = case randomR (c,d) g of
                            (x,h) -> (fromIntegral x, h)
    where (c,d) = (fromIntegral a :: Integer,
                   fromIntegral b :: Integer)

instance Random Word32 where
  randomR = integralRandomR
  random = randomR (minBound, maxBound)

instance Arbitrary Word32 where
    arbitrary = choose (minBound, maxBound)
    coarbitrary = integralCoarbitrary
~~~~

With these Arbitrary instances created, we can try our existing
properties on the ByteString types.

~~~~ {#bloomCheck.ghci:bs .screen}
ghci> handyCheck 1000 $ prop_one_present (undefined :: Lazy.ByteString)

<interactive>:1:0: Not in scope: `handyCheck'

<interactive>:1:18: Not in scope: `prop_one_present'

<interactive>:1:49:
    Failed to load interface for `Lazy':
      Use -v to see a list of the files searched for.
ghci> handyCheck 1000 $ prop_all_present (undefined :: Strict.ByteString)

<interactive>:1:0: Not in scope: `handyCheck'

<interactive>:1:18: Not in scope: `prop_all_present'

<interactive>:1:49:
    Failed to load interface for `Strict':
      Use -v to see a list of the files searched for.
~~~~

### Are suggested sizes correct?

The cost of testing properties of easyList increases rapidly as we
increase the number of tests to run. We would still like to have some
assurance that `easyList`{.function} will behave well on huge inputs.
Since it is not practical to test this directly, we can use a proxy:
will `suggestSizing`{.function} give a sensible array size and number of
hashes even with extreme inputs?

This is a slightly tricky property to check. We need to vary both the
desired false positive rate and the expected capacity. When we looked at
some results from the `sizings`{.function} function, we saw that the
relationship between these values is not easy to predict.

We can try to ignore the complexity.

~~~~ {#BloomCheck.hs:prop_suggest_try1 .programlisting}
-- file: examples/BloomCheck.hs
prop_suggest_try1 =
  forAll falsePositive $ \errRate ->
    forAll (choose (1,maxBound :: Word32)) $ \cap ->
      case B.suggestSizing (fromIntegral cap) errRate of
        Left err -> False
        Right (bits,hashes) -> bits > 0 && bits < maxBound && hashes > 0
~~~~

Not surprisingly, this gives us a test that is not actually useful.

~~~~ {#bloomCheck.ghci:try1 .screen}
ghci> handyCheck 1000 $ prop_suggest_try1

<interactive>:1:0: Not in scope: `handyCheck'

<interactive>:1:18: Not in scope: `prop_suggest_try1'
ghci> handyCheck 1000 $ prop_suggest_try1

<interactive>:1:0: Not in scope: `handyCheck'

<interactive>:1:18: Not in scope: `prop_suggest_try1'
~~~~

When we plug the counterexamples that QuickCheck prints into
`suggestSizings`{.function}, we can see that these inputs are rejected
because they would result in a bit array that would be too large.

~~~~ {#bloomCheck.ghci:why .screen}
ghci> B.suggestSizing 1678125842 8.501133057303545e-3

<interactive>:1:0:
    Failed to load interface for `B':
      Use -v to see a list of the files searched for.
~~~~

Since we can't easily predict which combinations will cause this
problem, we must resort to eliminating sizes and false positive rates
before they bite us.

~~~~ {#BloomCheck.hs:prop_suggest_try2 .programlisting}
-- file: examples/BloomCheck.hs
prop_suggest_try2 =
    forAll falsePositive $ \errRate ->
      forAll (choose (1,fromIntegral maxWord32)) $ \cap ->
        let bestSize = fst . minimum $ B.sizings cap errRate
        in bestSize < fromIntegral maxWord32 ==>
           either (const False) sane $ B.suggestSizing cap errRate
  where sane (bits,hashes) = bits > 0 && bits < maxBound && hashes > 0
        maxWord32 = maxBound :: Word32
~~~~

If we try this with a small number of tests, it seems to work well.

~~~~ {#bloomCheck.ghci:try2a .screen}
ghci> handyCheck 1000 $ prop_suggest_try2

<interactive>:1:0: Not in scope: `handyCheck'

<interactive>:1:18: Not in scope: `prop_suggest_try2'
~~~~

On a larger body of tests, we filter out too many combinations.

~~~~ {#bloomCheck.ghci:try2b .screen}
ghci> handyCheck 10000 $ prop_suggest_try2

<interactive>:1:0: Not in scope: `handyCheck'

<interactive>:1:19: Not in scope: `prop_suggest_try2'
~~~~

To deal with this, we try to reduce the likelihood of generating inputs
that we will subsequently reject.

~~~~ {#BloomCheck.hs:prop_suggestions_sane .programlisting}
-- file: examples/BloomCheck.hs
prop_suggestions_sane =
    forAll falsePositive $ \errRate ->
      forAll (choose (1,fromIntegral maxWord32 `div` 8)) $ \cap ->
        let size = fst . minimum $ B.sizings cap errRate
        in size < fromIntegral maxWord32 ==>
           either (const False) sane $ B.suggestSizing cap errRate
  where sane (bits,hashes) = bits > 0 && bits < maxBound && hashes > 0
        maxWord32 = maxBound :: Word32
~~~~

Finally, we have a robust looking property.

~~~~ {#bloomCheck.ghci:try3 .screen}
ghci> handyCheck 40000 $ prop_suggestions_sane

<interactive>:1:0: Not in scope: `handyCheck'

<interactive>:1:19: Not in scope: `prop_suggestions_sane'
~~~~

Performance analysis and tuning
-------------------------------

We now have a correctness base line: our QuickCheck tests pass. When we
start tweaking performance, we can rerun the tests at any time to ensure
that we haven't inadvertently broken anything.

Our first step is to write a small test application that we can use for
timing.

~~~~ {#WordTest.hs:module .programlisting}
-- file: examples/WordTest.hs
module Main where

import Control.Parallel.Strategies (NFData(..))
import Control.Monad (forM_, mapM_)
import qualified BloomFilter.Easy as B
import qualified Data.ByteString.Char8 as BS
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.Exit (exitFailure)

timed :: (NFData a) => String -> IO a -> IO a
timed desc act = do
    start <- getCurrentTime
    ret <- act
    end <- rnf ret `seq` getCurrentTime
    putStrLn $ show (diffUTCTime end start) ++ " to " ++ desc
    return ret

instance NFData BS.ByteString where
    rnf _ = ()

instance NFData (B.Bloom a) where
    rnf filt = B.length filt `seq` ()
~~~~

We borrow the `rnf`{.function} function that we introduced in [the
section called “Separating algorithm from
evaluation”](concurrent-and-multicore-programming.html#concurrent.strategies "Separating algorithm from evaluation")
to develop a simple timing harness. Out `timed`{.function} action
ensures that a value is evaluated to normal form in order to accurately
capture the cost of evaluating it.

The application creates a Bloom filter from the contents of a file,
treating each line as an element to add to the filter.

~~~~ {#WordTest.hs:main .programlisting}
-- file: examples/WordTest.hs
main = do
  args <- getArgs
  let files | null args = ["/usr/share/dict/words"]
            | otherwise = args
  forM_ files $ \file -> do

    words <- timed "read words" $
      BS.lines `fmap` BS.readFile file

    let len = length words
        errRate = 0.01

    putStrLn $ show len ++ " words"
    putStrLn $ "suggested sizings: " ++
               show (B.suggestSizing (fromIntegral len) errRate)

    filt <- timed "construct filter" $
      case B.easyList errRate words of
        Left errmsg -> do
          putStrLn $ "Error: " ++ errmsg
          exitFailure
        Right filt -> return filt

    timed "query every element" $
      mapM_ print $ filter (not . (`B.elem` filt)) words
~~~~

We use `timed`{.function} to account for the costs of three distinct
phases: reading and splitting the data into lines; populating the Bloom
filter; and querying every element in it.

If we compile this and run it a few times, we can see that the execution
time is just long enough to be interesting, while the timing variation
from run to run is small. We have created a plausible-looking
microbenchmark.

~~~~ {#id683565 .screen}
$ ghc -O2  --make WordTest
[1 of 1] Compiling Main             ( WordTest.hs, WordTest.o )
Linking WordTest ...
$ ./WordTest
0.196347s to read words
479829 words
1.063537s to construct filter
4602978 bits
0.766899s to query every element
$ ./WordTest
0.179284s to read words
479829 words
1.069363s to construct filter
4602978 bits
0.780079s to query every element
~~~~

### Profile-driven performance tuning

To understand where our program might benefit from some tuning, we
rebuild it and run it with profiling enabled.

Since we already built `WordTest`{.filename} and have not subsequently
changed it, if we rerun **ghc** to enable profiling support, it will
quite reasonably decide to do nothing. We must force it to rebuild,
which we accomplish by updating the filesystem's idea of when we last
edited the source file.

~~~~ {#id683644 .screen}
$ touch WordTest.hs
$ ghc -O2 -prof -auto-all --make WordTest
[1 of 1] Compiling Main             ( WordTest.hs, WordTest.o )
Linking WordTest ...

$ ./WordTest +RTS -p
0.322675s to read words
479829 words
suggested sizings: Right (4602978,7)
2.475339s to construct filter
1.964404s to query every element

$ head -20 WordTest.prof
total time  =          4.10 secs   (205 ticks @ 20 ms)
total alloc = 2,752,287,168 bytes  (excludes profiling overheads)

COST CENTRE                    MODULE               %time %alloc

doubleHash                     BloomFilter.Hash      48.8   66.4
indices                        BloomFilter.Mutable   13.7   15.8
elem                           BloomFilter            9.8    1.3
hashByteString                 BloomFilter.Hash       6.8    3.8
easyList                       BloomFilter.Easy       5.9    0.3
hashIO                         BloomFilter.Hash       4.4    5.3
main                           Main                   4.4    3.8
insert                         BloomFilter.Mutable    2.9    0.0
len                            BloomFilter            2.0    2.4
length                         BloomFilter.Mutable    1.5    1.0
~~~~

Our `doubleHash`{.function} function immediately leaps out as a huge
time and memory sink.

![[Tip]](/support/figs/tip.png)

Always profile before—and while—you tune!

Before our first profiling run, we did not expect
`doubleHash`{.function} to even appear in the top ten of “hot”
functions, much less to dominate it. Without this knowledge, we would
probably have started tuning something entirely irrelevant.

Recall that the body of `doubleHash`{.function} is an innocuous list
comprehension.

~~~~ {#id683758 .programlisting}
-- file: BloomFilter/Hash.hs
doubleHash :: Hashable a => Int -> a -> [Word32]
doubleHash numHashes value = [h1 + h2 * i | i <- [0..num]]
    where h   = hashSalt 0x9150a946c4a8966e value
          h1  = fromIntegral (h `shiftR` 32) .&. maxBound
          h2  = fromIntegral h
          num = fromIntegral numHashes
~~~~

Since the function returns a list, it makes *some* sense that it
allocates so much memory, but when code this simple performs so badly,
we should be suspicious.

Faced with a performance mystery, the suspicious mind will naturally
want to inspect the output of the compiler. We don't need to start
scrabbling through assembly language dumps: it's best to start at a
higher level.

GHC's `-ddump-simpl`{.option} option prints out the code that it
produces after performing all of its high-level optimisations.

~~~~ {#id683803 .screen}
$ ghc -O2 -c -ddump-simpl --make BloomFilter/Hash.hs > dump.txt
[1 of 1] Compiling BloomFilter.Hash ( BloomFilter/Hash.hs )
~~~~

The file thus produced is about a thousand lines long. Most of the names
in it are mangled somewhat from their original Haskell representations.
Even so, searching for `doubleHash`{.function} will immediately drop us
at the definition of the function. For example, here is how we might
start exactly at the right spot from a Unix shell.

~~~~ {#id683839 .screen}
$ less +/doubleHash dump.txt
~~~~

It can be difficult to start reading the output of GHC's simplifier.
There are many automatically generated names, and the code has many
obscure annotations. We can make substantial progress by ignoring things
that we do not understand, focusing on those that look familiar. The
Core language shares some features with regular Haskell, notably type
signatures; `let`{.literal} for variable binding; and `case`{.literal}
for pattern matching.

If we skim through the definition of `doubleHash`{.function}, we will
arrive at a section that looks something like this.

~~~~ {#id683899 .programlisting}
__letrec { 
  go_s1YC :: [GHC.Word.Word32] -> [GHC.Word.Word32] 
  [Arity 1
   Str: DmdType S]
  go_s1YC =
    \ (ds_a1DR :: [GHC.Word.Word32]) ->
      case ds_a1DR of wild_a1DS {
    [] -> GHC.Base.[] @ GHC.Word.Word32; 
    : y_a1DW ys_a1DX -> 
      GHC.Base.: @ GHC.Word.Word32 
        (case h1_s1YA of wild1_a1Mk { GHC.Word.W32# x#_a1Mm -> 
         case h2_s1Yy of wild2_a1Mu { GHC.Word.W32# x#1_a1Mw ->
         case y_a1DW of wild11_a1My { GHC.Word.W32# y#_a1MA ->
         GHC.Word.W32# 
           (GHC.Prim.narrow32Word#
          (GHC.Prim.plusWord# 
             x#_a1Mm (GHC.Prim.narrow32Word#
                              (GHC.Prim.timesWord# x#1_a1Mw y#_a1MA))))
         }
         }
         })
        (go_s1YC ys_a1DX) 
      };
} in 
  go_s1YC 
    (GHC.Word.$w$dmenumFromTo2
       __word 0 (GHC.Prim.narrow32Word# (GHC.Prim.int2Word# ww_s1X3)))
~~~~

This is the body of the list comprehension. It may seem daunting, but we
can look through it piece by piece and find that it is not, after all,
so complicated.

[![1](images/callouts/1.png)](#loop1-letrec-co)

A `__letrec`{.code} is equivalent to a normal Haskell `let`{.literal}.

[![2](images/callouts/2.png)](#loop1-def-co)

GHC compiled the body of our list comprehension into a loop named
`go_s1YC`{.function}.

[![3](images/callouts/3.png)](#loop1-pat-empty-co)

If our `case`{.literal} expression matches the empty list, we return the
empty list. This is reassuringly familiar.

[![4](images/callouts/4.png)](#loop1-pat-colon-co)

This pattern would read in Haskell as `(y_a1DW:ys_a1DX)`{.code}. The
`(:)`{.code} constructor appears before its operands because the Core
language uses prefix notation exclusively for simplicity.

[![5](images/callouts/5.png)](#loop1-cons-colon-co)

This is an application of the `(:)`{.function} constructor. The
`@`{.code} notation indicates that the first operand will have type
Word32.

[![6](images/callouts/6.png)](#loop1-unbox-co)

Each of the three `case`{.literal} expressions *unboxes* a Word32value,
to get at the primitive value inside. First to be unboxed is
`h1`{.varname} (named `h1_s1YA`{.varname} here), then `h2`{.varname},
then the current list element, `y`{.varname}.

The unboxing occurs via pattern matching: W32\# is the constructor that
boxes a primitive value. By convention, primitive types and values, and
functions that use them, always contains a `#`{.code} somewhere in their
name.

[![7](images/callouts/7.png)](#loop1-rebox-co)

Here, we apply the `W32#`{.code} constructor to a value of the primitive
type Word32\#, to give a normal value of type Word32.

[![8](images/callouts/8.png)](#loop1-add-co)

The `plusWord#`{.function} and `timesWord#`{.function} functions add and
multiply primitive unsigned integers.

[![9](images/callouts/9.png)](#loop1-cons-cdr-co)

This is the second argument to the `(:)`{.function} constructor, in
which the `go_s1YC`{.function} function applies itself recursively.

[![10](images/callouts/10.png)](#loop1-apply-co)

Here, we apply our list comprehension loop function. Its argument is the
Core translation of the expression `[0..n]`{.code}.

From reading the Core for this code, we can see two interesting
behaviours.

-   We are creating a list, then immediately deconstructing it in the
    `go_s1YC`{.function} loop.

    GHC can often spot this pattern of production followed immediately
    by consumption, and transform it into a loop in which no allocation
    occurs. This class of transformation is called *fusion*, because the
    producer and consumer become fused together. Unfortunately, it is
    not occurring here.

-   The repeated unboxing of `h1`{.varname} and `h2`{.varname} in the
    body of the loop is wasteful.

To address these problems, we make a few tiny changes to our
`doubleHash`{.function} function.

~~~~ {#Hash.hs:doubleHash_new .programlisting}
-- file: BloomFilter/Hash.hs
doubleHash :: Hashable a => Int -> a -> [Word32]
doubleHash numHashes value = go 0
    where go n | n == num  = []
               | otherwise = h1 + h2 * n : go (n + 1)

          !h1 = fromIntegral (h `shiftR` 32) .&. maxBound
          !h2 = fromIntegral h

          h   = hashSalt 0x9150a946c4a8966e value
          num = fromIntegral numHashes
~~~~

We have manually fused the `[0..num]`{.code} expression and the code
that consumes it into a single loop. We have added strictness
annotations to `h1`{.varname} and `h2`{.varname}. And nothing more. This
has turned a 6-line function into an 8-line function. What effect does
our change have on Core output?

~~~~ {#id684455 .programlisting}
__letrec {
  $wgo_s1UH :: GHC.Prim.Word# -> [GHC.Word.Word32]
  [Arity 1
   Str: DmdType L]
  $wgo_s1UH =
    \ (ww2_s1St :: GHC.Prim.Word#) ->
      case GHC.Prim.eqWord# ww2_s1St a_s1T1 of wild1_X2m {
    GHC.Base.False ->
      GHC.Base.: @ GHC.Word.Word32
        (GHC.Word.W32#
         (GHC.Prim.narrow32Word#
          (GHC.Prim.plusWord#
           ipv_s1B2
           (GHC.Prim.narrow32Word#
        (GHC.Prim.timesWord# ipv1_s1AZ ww2_s1St)))))
        ($wgo_s1UH (GHC.Prim.narrow32Word#
                        (GHC.Prim.plusWord# ww2_s1St __word 1)));
    GHC.Base.True -> GHC.Base.[] @ GHC.Word.Word32
      };
} in  $wgo_s1UH __word 0
~~~~

Our new function has compiled down to a simple counting loop. This is
very encouraging, but how does it actually perform?

~~~~ {#id684484 .screen}
$ touch WordTest.hs
$ ghc -O2 -prof -auto-all --make WordTest
[1 of 1] Compiling Main             ( WordTest.hs, WordTest.o )
Linking WordTest ...

$ ./WordTest +RTS -p
0.304352s to read words
479829 words
suggested sizings: Right (4602978,7)
1.516229s to construct filter
1.069305s to query every element
~/src/darcs/book/examples/ch27/examples $ head -20 WordTest.prof 
total time  =        3.68 secs    (184 ticks @ 20 ms)
total alloc = 2,644,805,536 bytes (excludes profiling overheads)

COST CENTRE                    MODULE               %time %alloc

doubleHash                     BloomFilter.Hash      45.1   65.0
indices                        BloomFilter.Mutable   19.0   16.4
elem                           BloomFilter           12.5    1.3
insert                         BloomFilter.Mutable    7.6    0.0
easyList                       BloomFilter.Easy       4.3    0.3
len                            BloomFilter            3.3    2.5
hashByteString                 BloomFilter.Hash       3.3    4.0
main                           Main                   2.7    4.0
hashIO                         BloomFilter.Hash       2.2    5.5
length                         BloomFilter.Mutable    0.0    1.0
~~~~

Our tweak has improved performance by about 11%. This is a good result
for such a small change.

Exercises
---------

**1.**

Our use of`genericLength`{.function} in `easyList`{.function} will cause
our function to loop infinitely if we supply an infinite list. Fix this.

**2.**

*Difficult.* Write a QuickCheck property that checks whether the
observed false positive rate is close to the requested false positive
rate.

\

* * * * *

^[[58](#id680314)]^The name ST is an acronym of “state transformer”.

^[[59](#id681170)]^Jenkins's hash functions have *much* better mixing
properties than some other popular non-cryptographic hash functions that
you might be familiar with, such as FNV and `hashpjw`{.function}, so we
recommend avoiding them.

^[[60](#id681600)]^Unfortunately, we do not have room to explain why one
of these instances is decidable, but the other is not.

![image](/support/figs/rss.png) Want to stay up to date? Subscribe to
the comment feed for [this chapter](/feeds/comments/), or the [entire
book](/feeds/comments/).

Copyright 2007, 2008 Bryan O'Sullivan, Don Stewart, and John Goerzen.
This work is licensed under a [Creative Commons
Attribution-Noncommercial 3.0
License](http://creativecommons.org/licenses/by-nc/3.0/). Icons by [Paul
Davey](mailto:mattahan@gmail.com) aka
[Mattahan](http://mattahan.deviantart.com/).

  ----------------------------------------- -------------------- ---------------------------------
  [Prev](profiling-and-optimization.html)                        [Next](sockets-and-syslog.html)
  Chapter 25. Profiling and optimization    [Home](index.html)   Chapter 27. Sockets and Syslog
  ----------------------------------------- -------------------- ---------------------------------


