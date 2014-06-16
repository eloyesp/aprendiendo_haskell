[Real World Haskell](/)by Bryan O'Sullivan, Don Stewart, and John Goerzen
-------------------------------------------------------------------------

Chapter 17. Interfacing with C: the FFI

[Prev](using-parsec.html)

[Next](monad-transformers.html)

Chapter 17. Interfacing with C: the FFI
---------------------------------------

**Table of Contents**

[Foreign language bindings: the
basics](interfacing-with-c-the-ffi.html#id653815)

[Be careful of side effects](interfacing-with-c-the-ffi.html#id654076)

[A high level wrapper](interfacing-with-c-the-ffi.html#id654123)

[Regular expressions for Haskell: a binding for
PCRE](interfacing-with-c-the-ffi.html#id654294)

[Simple tasks: using the C
preprocessor](interfacing-with-c-the-ffi.html#id654351)

[Binding Haskell to C with
hsc2hs](interfacing-with-c-the-ffi.html#id654451)

[Adding type safety to PCRE](interfacing-with-c-the-ffi.html#id654503)

[Binding to constants](interfacing-with-c-the-ffi.html#id654629)

[Automating the binding](interfacing-with-c-the-ffi.html#id654840)

[Passing string data between Haskell and
C](interfacing-with-c-the-ffi.html#id654997)

[Typed pointers](interfacing-with-c-the-ffi.html#id655194)

[Memory management: let the garbage collector do the
work](interfacing-with-c-the-ffi.html#id655309)

[A high level interface: marshalling
data](interfacing-with-c-the-ffi.html#id655462)

[Mashalling ByteStrings](interfacing-with-c-the-ffi.html#id655555)

[Allocating local C data: the Storable
class](interfacing-with-c-the-ffi.html#id655666)

[Putting it all together](interfacing-with-c-the-ffi.html#id655783)

[Matching on strings](interfacing-with-c-the-ffi.html#id656122)

[Extracting information about the
pattern](interfacing-with-c-the-ffi.html#id656200)

[Pattern matching with
substrings](interfacing-with-c-the-ffi.html#id656311)

[The real deal: compiling and matching regular
expressions](interfacing-with-c-the-ffi.html#id656609)

Programming languages do not exist in perfect isolation. They inhabit an
ecosystem of tools and libraries, built up over decades, and often
written in a range of programming languages. Good engineering practice
suggests we reuse that effort. The Haskell Foreign Function Interface
(the "FFI") is the means by which Haskell code can use, and be used by,
code written in other languages. In this chapter we'll look at how the
FFI works, and how to produce a Haskell binding to a C library,
including how to use an FFI preprocessor to automate much of the work.
The challenge: take PCRE, the standard Perl-compatible regular
expression library, and make it usable from Haskell in an efficient and
functional way. Throughout, we'll seek to abstract out manual effort
required by the C implementation, delegating that work to Haskell to
make the interface more robust, yielding a clean, high level binding. We
assume only some basic familiarity with regular expressions.

Binding one language to another is a non-trivial task. The binding
language needs to understand the calling conventions, type system, data
structures, memory allocation mechanisms and linking strategy of the
target language, just to get things working. The task is to carefully
align the semantics of both languages, so that both languages can
understand the data that passes between them.

For Haskell, this technology stack is specified by [the Foreign Function
Interface addendum](http://www.cse.unsw.edu.au/~chak/haskell/ffi/) to
the Haskell report. The FFI report describes how to correctly bind
Haskell and C together, and how to extend bindings to other languages.
The standard is designed to be portable, so that FFI bindings will work
reliably across Haskell implementations, operating systems and C
compilers.

All implementations of Haskell support the FFI, and it is a key
technology when using Haskell in a new field. Instead of reimplementing
the standard libraries in a domain, we just bind to existing ones
written in languages other than Haskell.

The FFI adds a new dimension of flexibility to the language: if we need
to access raw hardware for some reason (say we're programming new
hardware, or implementing an operating system), the FFI lets us get
access to that hardware. It also gives us a performance escape hatch: if
we can't get a code hot spot fast enough, there's always the option of
trying again in C. So let's look at what the FFI actually means for
writing code.

Foreign language bindings: the basics
-------------------------------------

The most common operation we'll want to do, unsurprisingly, is to call a
C function from Haskell. So let's do that, by binding to some functions
from the standard C math library. We'll put the binding in a source
file, and then compile it into a Haskell binary that makes use of the C
code.

To start with, we need to enable the foreign function interface
extension, as the FFI addendum support isn't enabled by default. We do
this, as always, via a `LANGUAGE`{.code} pragma at the top of our source
file:

~~~~ {#SimpleFFI.hs:pragma .programlisting}
-- file: ch17/SimpleFFI.hs
{-# LANGUAGE ForeignFunctionInterface #-}
~~~~

The `LANGUAGE`{.code} pragmas indicate which extensions to Haskell 98 a
module uses. We bring just the FFI extension in play this time. It is
important to track which extensions to the language you need. Fewer
extensions generally means more portable, more robust code. Indeed, it
is common for Haskell programs written more than a decade ago to compile
perfectly well today, thanks to standardization, despite changes to the
language's syntax, type system and core libraries.

The next step is to import the `Foreign`{.code} modules, which provide
useful types (such as pointers, numerical types, arrays) and utility
functions (such as `malloc`{.code} and `alloca`{.code}), for writing
bindings to other languages:

~~~~ {#SimpleFFI.hs:imports .programlisting}
-- file: ch17/SimpleFFI.hs
import Foreign
import Foreign.C.Types
~~~~

For extensive work with foreign libraries, a good knowledge of the
`Foreign`{.code} modules is essential. Other useful modules include
`Foreign.C.String`{.code}, `Foreign.Ptr`{.code} and
`Foreign.Marshal.Array`{.code}.

Now we can get down to work calling C functions. To do this, we need to
know three things: the name of the C function, its type, and its
associated header file. Additionally, for code that isn't provided by
the standard C library, we'll need to know the C library's name, for
linking purposes. The actual binding work is done with a
`foreign import`{.code} declaration, like so:

~~~~ {#SimpleFFI.hs:binding .programlisting}
-- file: ch17/SimpleFFI.hs
foreign import ccall "math.h sin"
     c_sin :: CDouble -> CDouble
~~~~

This defines a new Haskell function, `c_sin`{.code}, whose concrete
implementation is in C, via the `sin`{.code} function. When
`c_sin`{.code} is called, a call to the actual `sin`{.code} will be made
(using the standard C calling convention, indicated by `ccall`{.code}
keyword). The Haskell runtime passes control to C, which returns its
results back to Haskell. The result is then wrapped up as a Haskell
value of type `CDouble`{.code}.

A common idiom when writing FFI bindings is to expose the C function
with the prefix "c\_", distinguishing it from more user-friendly, higher
level functions. The raw C function is specified by the `math.h`{.code}
header, where it is declared to have the type:

~~~~ {#math.c:type .programlisting}
double sin(double x);
~~~~

When writing the binding, the programmer has to translate C type
signatures like this into their Haskell FFI equivalents, making sure
that the data representations match up. For example, `double`{.code} in
C corresponds to `CDouble`{.code} in Haskell. We need to be careful
here, since if a mistake is made the Haskell compiler will happily
generate incorrect code to call C! The poor Haskell compiler doesn't
know anything about what types the C function actually requires, so if
instructed to, it will call the C function with the wrong arguments. At
best this will lead to C compiler warnings, and more likely, it will end
with with a runtime crash. At worst the error will silently go unnoticed
until some critical failure occurs. So make sure you use the correct FFI
types, and don't be wary of using QuickCheck to test your C code via the
bindings. ^[[36](#ftn.id654015)]^

The most important primitive C types are represented in Haskell with the
somewhat intuitive names (for signed and unsigned types) `CChar`{.code},
`CUChar`{.code}, `CInt`{.code}, `CUInt`{.code}, `CLong`{.code},
`CULong`{.code}, `CSize`{.code}, `CFloat`{.code}, `CDouble`{.code}. More
are defined in the FFI standard, and can be found in the Haskell base
library under `Foreign.C.Types`{.code}. It is also possible to define
your own Haskell-side representation types for C, as we'll see later.

### Be careful of side effects

One point to note is that we bound `sin`{.code} as a pure function in
Haskell, one with no side effects. That's fine in this case, since the
`sin`{.code} function in C is referentially transparent. By binding pure
C functions to pure Haskell functions, the Haskell compiler is taught
something about the C code, namely that it has no side effects, making
optimisations easier. Pure code is also more flexible code for the
Haskell programmer, as it yields naturally persistent data structures,
and threadsafe functions. However, while pure Haskell code is always
threadsafe, this is harder to guarantee of C. Even if the documentation
indicates the function is likely to expose no side effects, there's
little to ensure it is also threadsafe, unless explicitly documented as
"reentrant". Pure, threadsafe C code, while rare, is a valuable
commodity. It is the easiest flavor of C to use from Haskell.

Of course, code with side effects is more common in imperative
languages, where the explicit sequencing of statements encourages the
use of effects. It is much more common in C for functions to return
different values, given the same arguments, due to changes in global or
local state, or to have other side effects. Typically this is signalled
in C by the function returning only a status value, or some void type,
rather than a useful result value. This indicates that the real work of
the function was in its side effects. For such functions, we'll need to
capture those side effects in the IO monad (by changing the return type
to `IO CDouble`{.code}, for example). We also need to be very careful
with pure C functions that aren't also reentrant, as multiple threads
are extremely common in Haskell code, in comparison to C. We might need
to make non-reentrant code safe for use by moderating access to the FFI
binding with a transactional lock, or duplicating the underlying C
state.

### A high level wrapper

With the foreign imports out of the way, the next step is to convert the
C types we pass to and receive from the foreign language call into
native Haskell types, wrapping the binding so it appears as a normal
Haskell function:

~~~~ {#SimpleFFI.hs:highlevel .programlisting}
-- file: ch17/SimpleFFI.hs
fastsin :: Double -> Double
fastsin x = realToFrac (c_sin (realToFrac x))
~~~~

The main thing to remember when writing convenient wrappers over
bindings like this is to convert input and output back to normal Haskell
types correctly. To convert between floating point values, we can use
`realToFrac`{.code}, which lets us translate different floating point
values to each other (and these conversions, such as from
`CDouble`{.code} to `Double`{.code}, are usually free, as the underlying
representations are unchanged). For integer values `fromIntegral`{.code}
is available. For other common C data types, such as arrays, we may need
to unpack the data to a more workable Haskell type (such as a list), or
possibly leave the C data opaque, and operate on it only indirectly
(perhaps via a `ByteString`{.code}). The choice depends on how costly
the transformation is, and on what functions are available on the source
and destination types.

We can now proceed to use the bound function in a program. For example,
we can apply the C `sin`{.code} function to a Haskell list of tenths:

~~~~ {#SimpleFFI.hs:use .programlisting}
-- file: ch17/SimpleFFI.hs
main = mapM_ (print . fastsin) [0/10, 1/10 .. 10/10]
~~~~

This simple program prints each result as it is computed. Putting the
complete binding in the file `SimpleFFI.hs`{.code} we can run it in
`GHCi`{.code}:

~~~~ {#id654218 .screen}
$ ghci SimpleFFI.hs
*Main> main
0.0
9.983341664682815e-2
0.19866933079506122
0.2955202066613396
0.3894183423086505
0.479425538604203
0.5646424733950354
0.644217687237691
0.7173560908995227
0.7833269096274833
0.8414709848078964
~~~~

Alternatively, we can compile the code to an executable, dynamically
linked against the corresponding C library:

~~~~ {#id654236 .screen}
$ ghc -O --make SimpleFFI.hs
[1 of 1] Compiling Main             ( SimpleFFI.hs, SimpleFFI.o )
Linking SimpleFFI ...
~~~~

and then run that:

~~~~ {#id654260 .screen}
$ ./SimpleFFI 
0.0
9.983341664682815e-2
0.19866933079506122
0.2955202066613396
0.3894183423086505
0.479425538604203
0.5646424733950354
0.644217687237691
0.7173560908995227
0.7833269096274833
0.8414709848078964
~~~~

We're well on our way now, with a full program, statically linked
against C, which interleaves C and Haskell code, and passes data across
the language boundary. Simple bindings like the above are almost
trivial, as the standard `Foreign`{.code} library provides convenient
aliases for common types like `CDouble`{.code}. In the next section
we'll look at a larger engineering task: binding to the PCRE library,
which brings up issues of memory management and type safety.

Regular expressions for Haskell: a binding for PCRE
---------------------------------------------------

As we've seen in previous sections, Haskell programs have something of a
bias towards lists as a foundational data structure. List functions are
a core part of the base library, and convenient syntax for constructing
and taking apart list structures is wired into the language. Strings
are, of course, simply lists of characters (rather than, for example,
flat arrays of characters). This flexibility is all well and good, but
it results in a tendency for the standard library to favour polymorphic
list operations at the expense of string-specific operations.

Indeed, many common tasks can be solved via regular-expression-based
string processing, yet support for regular expressions isn't part of the
Haskell `Prelude`{.literal}. So let's look at how we'd take an
off-the-shelf regular expression library, PCRE, and provide a natural,
convenient Haskell binding to it, giving us useful regular expressions
for Haskell.

PCRE itself is a ubiquitous C library implementing Perl-style regular
expressions. It is widely available, and preinstalled on many systems.
If not, it can be found at [http://www.pcre.org/](http://www.pcre.org/).
In the following sections we'll assume the PCRE library and headers are
available on the machine.

### Simple tasks: using the C preprocessor

The simplest task when setting out to write a new FFI binding from
Haskell to C is to bind constants defined in C headers to equivalent
Haskell values. For example, PCRE provides a set of flags for modifying
how the core pattern matching system works (such as ignoring case, or
allowing matching on newlines). These flags appear as numeric constants
in the PCRE header files:

~~~~ {#PCRE.h:constants .programlisting}
/* Options */

#define PCRE_CASELESS           0x00000001
#define PCRE_MULTILINE          0x00000002
#define PCRE_DOTALL             0x00000004
#define PCRE_EXTENDED           0x00000008
~~~~

To export these values to Haskell we need to insert them into a Haskell
source file somehow. One obvious way to do this is by using the C
preprocessor to substitute definitions from C into the Haskell source,
which we then compile as a normal Haskell source file. Using the
preprocessor we can even declare simple constants, via textual
substitutions on the Haskell source file:

~~~~ {#Enum1.hs:cpp .programlisting}
-- file: ch17/Enum1.hs
{-# LANGUAGE CPP #-}

#define N 16

main = print [ 1 .. N ]
~~~~

The file is processed with the preprocessor in a similar manner to C
source (with CPP run for us by the Haskell compiler, when it spots the
`LANGUAGE`{.code} pragma), resulting in program output:

~~~~ {#id654410 .screen}
$ runhaskell Enum.hs
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
~~~~

However, relying on CPP is a rather fragile approach. The C preprocessor
isn't aware it is processing a Haskell source file, and will happily
include text, or transform source, in such a way as to make our Haskell
code invalid. We need to be careful not to confuse CPP. If we were to
include C headers we risk substituting unwanted symbols, or inserting C
type information and prototypes into the Haskell source, resulting in a
broken mess.

To solve these problems, the binding preprocessor `hsc2hs`{.code} is
distributed with GHC. It provides a convenient syntax for including C
binding information in Haskell, as well as letting us safely operate
with headers. It is the tool of choice for the majority of Haskell FFI
bindings.

### Binding Haskell to C with hsc2hs

To use hsc2hs as an intelligent binding tool for Haskell, we need to
create an `.hsc`{.code} file, `Regex.hsc`{.code}, which will hold the
Haskell source for our binding, along with hsc2hs processing rules, C
headers and C type information. To start off, we need some pragmas and
imports:

~~~~ {#Regex-hsc.hs:headers .programlisting}
-- file: ch17/Regex-hsc.hs
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Regex where

import Foreign
import Foreign.C.Types

#include <pcre.h>
~~~~

The module begins with a typical preamble for an FFI binding: enable
CPP, enable the foreign function interface syntax, declare a module
name, and then import some things from the base library. The unusual
item is the final line, where we include the C header for PCRE. This
wouldn't be valid in a `.hs`{.code} source file, but is fine in
`.hsc`{.code} code.

### Adding type safety to PCRE

Next we need a type to represent PCRE compile-time flags. In C, these
are integer flags to the `compile`{.code} function, so we could just use
`CInt`{.code} to represent them. All we know about the flags is that
they're C numeric constants, so `CInt`{.code} is the appropriate
representation.

As a Haskell library writer though, this feels sloppy. The type of
values that can be used as regex flags contains fewer values than
`CInt`{.code} allows for. Nothing would prevent the end user passing
illegal integer values as arguments, or mixing up flags that should be
passed only at regex compile time, with runtime flags. It is also
possible to do arbitrary math on flags, or make other mistakes where
integers and flags are confused. We really need to more precisely
specify that the type of flags is distinct from its runtime
representation as a numeric value. If we can do this, we can statically
prevent a class of bugs relating to misuse of flags.

Adding such a layer of type safety is relatively easy, and a great use
case for `newtype`{.code}, the type introduction declaration. What
`newtype`{.code} lets us do is create a type with an identical runtime
representation type to another type, but which is treated as a separate
type at compile time. We can represent flags as `CInt`{.code} values,
but at compile time they'll be tagged distinctly for the type checker.
This makes it a type error to use invalid flag values (as we specify
only those valid flags, and prevent access to the data constructor), or
to pass flags to functions expecting integers. We get to use the Haskell
type system to introduce a layer of type safety to the C PCRE API.

To do this, we define a `newtype`{.code} for PCRE compile time options,
whose representation is actually that of a `CInt`{.code} value, like so:

~~~~ {#Regex-hsc.hs:newtype .programlisting}
-- file: ch17/Regex-hsc.hs
-- | A type for PCRE compile-time options. These are newtyped CInts,
-- which can be bitwise-or'd together, using '(Data.Bits..|.)'
--
newtype PCREOption = PCREOption { unPCREOption :: CInt }
    deriving (Eq,Show)
~~~~

The type name is `PCREOption`{.code}, and it has a single constructor,
also named `PCREOption`{.code}, which lifts a `CInt`{.code} value into a
new type by wrapping it in a constructor. We can also happily define an
accessor, `unPCREOption`{.code}, using the Haskell record syntax, to the
underlying `CInt`{.code}. That's a lot of convenience in one line. While
we're here, we can also derive some useful type class operations for
flags (equality and printing). We also need to remember export the data
constructor abstractly from the source module, ensuring users can't
construct their own `PCREOption`{.code} values.

### Binding to constants

Now we've pulled in the required modules, turned on the language
features we need, and defined a type to represent PCRE options, we need
to actually define some Haskell values corresponding to those PCRE
constants.

We can do this in two ways with hsc2hs. The first way is to use the
`#const`{.code} keyword hsc2hs provides. This lets us name constants to
be provided by the C preprocessor. We can bind to the constants
manually, by listing the CPP symbols for them using the `#const`{.code}
keyword:

~~~~ {#Regex-hsc-const.hs:constoptions .programlisting}
-- file: ch17/Regex-hsc-const.hs
caseless       :: PCREOption
caseless       = PCREOption #const PCRE_CASELESS

dollar_endonly :: PCREOption
dollar_endonly = PCREOption #const PCRE_DOLLAR_ENDONLY

dotall         :: PCREOption
dotall         = PCREOption #const PCRE_DOTALL
~~~~

This introduces three new constants on the Haskell side,
`caseless`{.code}, `dollar_endonly`{.code} and `dotall`{.code},
corresponding to the similarly named C definitions. We immediately wrap
the constants in a newtype constructor, so they're exposed to the
programmer as abstract `PCREOption`{.code} types only.

This is the first step, creating a `.hsc`{.code} file. We now need to
actually create a Haskell source file, with the C preprocessing done.
Time to run `hsc2hs`{.code} over the `.hsc`{.code} file:

~~~~ {#id654716 .screen}
$ hsc2hs Regex.hsc
~~~~

This creates a new output file, `Regex.hs`{.code}, where the CPP
variables have been expanded, yielding valid Haskell code:

~~~~ {#Regex-hsc-const-generated.hs:generatedconsts .programlisting}
-- file: ch17/Regex-hsc-const-generated.hs
caseless       :: PCREOption
caseless       = PCREOption 1
{-# LINE 21 "Regex.hsc" #-}

dollar_endonly :: PCREOption
dollar_endonly = PCREOption 32
{-# LINE 24 "Regex.hsc" #-}

dotall         :: PCREOption
dotall         = PCREOption 4
{-# LINE 27 "Regex.hsc" #-}
~~~~

Notice also how the original line in the `.hsc`{.code} is listed next to
each expanded definition via the `LINE`{.code} pragma. The compiler uses
this information to report errors in terms of their source, in the
original file, rather than the generated one. We can load this generated
`.hs`{.code} file into the interpreter, and play with the results:

~~~~ {#id654766 .screen}
$ ghci Regex.hs
*Regex> caseless
PCREOption {unPCREOption = 1}
*Regex> unPCREOption caseless
1
*Regex> unPCREOption caseless + unPCREOption caseless
2
*Regex> caseless + caseless
interactive>:1:0:
    No instance for (Num PCREOption)
~~~~

So things are working as expected. The values are opaque, we get type
errors if we try to break the abstraction, and we can unwrap them and
operate on them if needed. The `unPCREOption`{.code} accessor is used to
unwrap the boxes. That's a good start, but let's see how we can simplify
this task further.

### Automating the binding

Clearly, manually listing all the C defines, and wrapping them is
tedious, and error prone. The work of wrapping all the literals in
`newtype`{.code} constructors is also annoying. This kind of binding is
such a common task that `hsc2hs`{.code} provides convenient syntax to
automate it: the `#enum`{.code} construct.

We can replace our list of top level bindings with the equivalent:

~~~~ {#Regex-hsc.hs:constants .programlisting}
-- file: ch17/Regex-hsc.hs
-- PCRE compile options
#{enum PCREOption, PCREOption
  , caseless             = PCRE_CASELESS
  , dollar_endonly       = PCRE_DOLLAR_ENDONLY
  , dotall               = PCRE_DOTALL
  }
~~~~

This is much more concise! The `#enum`{.code} construct gives us three
fields to work with. The first is the name of the type we'd like the C
defines to be treated as. This lets us pick something other than just
`CInt`{.code} for the binding. We chose `PCREOption`{.code}'s to
construct.

The second field is an optional constructor to place in front of the
symbols. This is specifically for the case we want to construct
`newtype`{.code} values, and where much of the grunt work is saved. The
final part of the `#enum`{.code} syntax is self explanatory: it just
defines Haskell names for constants to be filled in via CPP.

Running this code through hsc2hs, as before, generates a Haskell file
with the following binding code produced (with `LINE`{.code} pragmas
removed for brevity):

~~~~ {#Regex.hs:result .programlisting}
-- file: ch17/Regex.hs
caseless              :: PCREOption
caseless              = PCREOption 1
dollar_endonly        :: PCREOption
dollar_endonly        = PCREOption 32
dotall                :: PCREOption
dotall                = PCREOption 4
~~~~

Perfect. Now we can do something in Haskell with these values. Our aim
here is to treat flags as abstract types, not as bit fields in integers
in C. Passing multiple flags in C would be done by bitwise or-ing
multiple flags together. For an abstract type though, that would expose
too much information. Preserving the abstraction, and giving it a
Haskell flavor, we'd prefer users passed in flags in a list that the
library itself combined. This is achievable with a simple fold:

~~~~ {#Regex.hs:bitwise .programlisting}
-- file: ch17/Regex.hs
-- | Combine a list of options into a single option, using bitwise (.|.)
combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0
~~~~

This simple loop starts with an initial value of 0, unpacks each flag,
and uses bitwise-or, `(.|.)`{.code} on the underlying `CInt`{.code}, to
combine each value with the loop accumulator. The final accumulated
state is then wrapped up in the `PCREOption`{.code} constructor.

Let's turn now to actually compiling some regular expressions.

Passing string data between Haskell and C
-----------------------------------------

The next task is to write a binding to the PCRE regular expression
`compile`{.code} function. Let's look at its type, straight from the
`pcre.h`{.code} header file:

~~~~ {#PCRE-compile.h:pcre_compile .programlisting}
pcre *pcre_compile(const char *pattern,
                   int options,
                   const char **errptr,
                   int *erroffset,
                   const unsigned char *tableptr);
~~~~

This function compiles a regular expression pattern into some internal
format, taking the pattern as an argument, along with some flags, and
some variables for returning status information.

We need to work out what Haskell types to represent each argument with.
Most of these types are covered by equivalents defined for us by the FFI
standard, and available in `Foreign.C.Types`{.code}. The first argument,
the regular expression itself, is passed as a null-terminated char
pointer to C, equivalent to the Haskell `CString`{.code} type. PCRE
compile time options we've already chosen to represent as the abstract
`PCREOption`{.code} newtype, whose runtime representation is a
`CInt`{.code}. As the representations are guaranteed to be identical, we
can pass the `newtype`{.code} safely. The other arguments are a little
more complicated and require some work to construct and take apart.

The third argument, a pointer to a C string, will be used as a reference
to any error message generated when compiling the expression. The value
of the pointer will be modified by the C function to point to a custom
error string. This we can represent with a `Ptr CString`{.code} type.
Pointers in Haskell are heap allocated containers for raw addresses, and
can be created and operated on with a number of allocation primitives in
the FFI library. For example, we can represent a pointer to a C
`int`{.code} as `Ptr CInt`{.code}, and a pointer to an unsigned char as
a `Ptr Word8`{.code}.

![[Note]](/support/figs/note.png)

A note about pointers

Once we have a Haskell `Ptr`{.code} value handy, we can do various
pointer-like things with it. We can compare it for equality with the
null pointer, represented with the special `nullPtr`{.code} constant. We
can cast a pointer from one type to a pointer to another, or we can
advance a pointer by an offset in bytes with `plusPtr`{.code}. We can
even modify the value pointed to, using `poke`{.code}, and of course
dereference a pointer yielding that which it points to, with
`peek`{.code}. In the majority of circumstances, a Haskell programmer
doesn't need to operate on pointers directly, but when they are needed
these tools come in handy.

The question then is how to represent the abstract `pcre`{.code} pointer
returned when we compile the regular expression. We need to find a
Haskell type that is as abstract as the C type. Since the C type is
treated abstractly, we can assign any heap-allocated Haskell type to the
data, as long as it has few or no operations on it. This is a common
trick for arbitrarily typed foreign data. The idiomatic simple type to
use to represent unknown foreign data is a pointer to the `()`{.code}
type. We can use a type synonym to remember the binding:

~~~~ {#PCRE-compile.hs:unit .programlisting}
-- file: ch17/PCRE-compile.hs
type PCRE = ()
~~~~

That is, the foreign data is some unknown, opaque object, and we'll just
treat it as a pointer to `()`{.code}, knowing full well that we'll never
actually dereference that pointer. This gives us the following foreign
import binding for `pcre_compile`{.code}, which must be in `IO`{.code},
as the pointer returned will vary on each call, even if the returned
object is functionally equivalent:

~~~~ {#PCRE-compile.hs:pcre_compile .programlisting}
-- file: ch17/PCRE-compile.hs
foreign import ccall unsafe "pcre.h pcre_compile"
    c_pcre_compile  :: CString
                    -> PCREOption
                    -> Ptr CString
                    -> Ptr CInt
                    -> Ptr Word8
                    -> IO (Ptr PCRE)
~~~~

### Typed pointers

![[Note]](/support/figs/note.png)

A note about safety

When making a foreign import declaration, we can optionally specify a
"safety" level to use when making the call, using either the
`safe`{.code} or `unsafe`{.code} keyword. A safe call is less efficient,
but guarantees that the Haskell system can be safely called into from C.
An "unsafe" call has far less overhead, but the C code that is called
must not call back into Haskell. By default foreign imports are "safe",
but in practice it is rare for C code to call back into Haskell, so for
efficiency we mostly use "unsafe" calls.

We can increase safety in the binding futher by using a "typed" pointer,
instead of using the `()`{.code} type. That is, a unique type, distinct
from the unit type, that has no meaningful runtime representation. A
type for which no data can be constructed, making dereferencing it a
type error. One good way to build such provably uninspectable data types
is with a nullary data type:

~~~~ {#PCRE-nullary.hs:nullary .programlisting}
-- file: ch17/PCRE-nullary.hs
data PCRE
~~~~

This requires the `EmptyDataDecls`{.code} language extension. This type
clearly contains no values! We can only ever construct pointers to such
values, as there are no concrete values (other than bottom) that have
this type.

We can also achieve the same thing, without requiring a language
extension, using a recursive `newtype`{.code}:

~~~~ {#PCRE-recursive.hs:nullary .programlisting}
-- file: ch17/PCRE-recursive.hs
newtype PCRE = PCRE (Ptr PCRE)
~~~~

Again, we can't really do anything with a value of this type, as it has
no runtime representation. Using typed pointers in these ways is just
another way to add safety to a Haskell layer over what C provides. What
would require discipline on the part of the C programmer (remembering
never to dereference a PCRE pointer) can be enforced statically in the
type system of the Haskell binding. If this code compiles, the type
checker has given us a proof that the PCRE objects returned by C are
never dereferenced on the Haskell side.

We have the foreign import declaration sorted out now, the next step is
to marshal data into the right form, so that we can finally call the C
code.

### Memory management: let the garbage collector do the work

One question that isn't resolved yet is how to manage the memory
associated with the abstract `PCRE`{.code} structure returned by the C
library. The caller didn't have to allocate it: the library took care of
that by allocating memory on the C side. At some point though we'll need
to deallocate it. This, again, is an opportunity to abstract the tedium
of using the C library by hiding the complexity inside the Haskell
binding.

We'll use the Haskell garbage collector to automatically deallocate the
C structure once it is no longer in use. To do this, we'll make use of
Haskell garbage collector finalizers, and the `ForeignPtr`{.code} type.

We don't want users to have to manually deallocate the `Ptr PCRE`{.code}
value returned by the foreign call. The PCRE library specifically states
that structures are allocated on the C side with `malloc`{.code}, and
need to be freed when no longer in use, or we risk leaking memory. The
Haskell garbage collector already goes to great lengths to automate the
task of managing memory for Haskell values. Cleverly, we can also assign
our hardworking garbage collector the task of looking after C's memory
for us. The trick is to associate a piece of Haskell data with the
foreign allocator data, and give the Haskell garbage collector an
arbitrary function that is to deallocate the C resource once it notices
that the Haskell data is done with.

We have two tools at our disposal here, the opaque `ForeignPtr`{.code}
data type, and the `newForeignPtr`{.code} function, which has type:

~~~~ {#ForeignPtr.hs:newForeignPtr .programlisting}
-- file: ch17/ForeignPtr.hs
newForeignPtr :: FinalizerPtr a -> Ptr a -> IO (ForeignPtr a)
~~~~

The function takes two arguments, a finalizer to run when the data goes
out of scope, and a pointer to the associated C data. It returns a new
managed pointer which will have its finalizer run once the garbage
collector decides the data is no longer in use. What a lovely
abstraction!

These finalizable pointers are appropriate whenever a C library requires
the user to explicitly deallocate, or otherwise clean up a resource,
when it is no longer in use. It is a simple piece of equipment that goes
a long way towards making the C library binding more natural, more
functional, in flavor.

So with this in mind, we can hide the manually managed `Ptr PCRE`{.code}
type inside an automatically managed data structure, yielding us the
data type used to represent regular expressions that users will see:

~~~~ {#PCRE-compile.hs:data .programlisting}
-- file: ch17/PCRE-compile.hs
data Regex = Regex !(ForeignPtr PCRE)
                   !ByteString
        deriving (Eq, Ord, Show)
~~~~

This new `Regex`{.code} data types consists of two parts. The first is
an abstract `ForeignPtr`{.code}, that we'll use to manage the underlying
`PCRE`{.code} data allocated in C. The second component is a strict
`ByteString`{.code}, which is the string representation of the regular
expression that we compiled. By keeping it the user-level representation
of the regular expression handy inside the `Regex`{.code} type, it'll be
easier to print friendly error messages, and show the `Regex`{.code}
itself in a meaningful way.

### A high level interface: marshalling data

The challenge when writing FFI bindings, once the Haskell types have
been decided upon, is to convert regular data types a Haskell programmer
will be familiar with into low level pointers to arrays and other C
types. What would an ideal Haskell interface to regular expression
compilation look like? We have some design intuitions to guide us.

For starters, the act of compilation should be a referentially
transparent operation: passing the same regex string will yield
functionally the same compiled pattern each time, although the C library
will give us observably different pointers to functionally identical
expressions. If we can hide these memory management details, we should
be able to represent the binding as a pure function. The ability to
represent a C function in Haskell as a pure operation is a key step
towards flexibility, and an indicator the interface will be easy to use
(as it won't require complicated state to be initialized before it can
be used).

Despite being pure, the function can still fail. If the regular
expression input provided by the user is ill-formed an error string is
returned. A good data type to represent optional failure with an error
value, is `Either`{.code}. That is, either we return a valid compiled
regular expression, or we'll return an error string. Encoding the
results of a C function in a familiar, foundational Haskell type like
this is another useful step to make the binding more idiomatic.

For the user-supplied parameters, we've already decided to pass
compilation flags in as a list. We can choose to pass the input regular
expression either as an efficient `ByteString`{.code}, or as a regular
`String`{.code}. An appropriate type signature, then, for referentially
transparent compilation success with a value or failure with an error
string, would be:

~~~~ {#PCRE-compile.hs:compiletype .programlisting}
-- file: ch17/PCRE-compile.hs
compile :: ByteString -> [PCREOption] -> Either String Regex
~~~~

The input is a `ByteString`{.code}, available from the
`Data.ByteString.Char8`{.code} module (and we'll import this
`qualified`{.code} to avoid name clashes), containing the regular
expression, and a list of flags (or the empty list if there are no flags
to pass). The result is either an error string, or a new, compiled
regular expression.

### Mashalling ByteStrings

Given this type, we can sketch out the `compile`{.code} function: the
high level interface to the raw C binding. At its heart, it will call
`c_pcre_compile`{.code}. Before it does that, it has to marshal the
input `ByteString`{.code} into a `CString`{.code}. This is done with the
`ByteString`{.code} library's `useAsCString`{.code} function, which
copies the input `ByteString`{.code} into a null-terminated C array
(there is also an unsafe, zero copy variant, that assumes the
`ByteString`{.code} is already null terminated):

~~~~ {#ForeignPtr.hs:useAsCString .programlisting}
-- file: ch17/ForeignPtr.hs
useAsCString :: ByteString -> (CString -> IO a) -> IO a
~~~~

This function takes a `ByteString`{.code} as input. The second argument
is a user-defined function that will run with the resulting
`CString`{.code}. We see here another useful idiom: data marshalling
functions that are naturally scoped via closures. Our
`useAsCString`{.code} function will convert the input data to a C
string, which we can then pass to C as a pointer. Our burden then is to
supply it with a chunk of code to call C.

Code in this style is often written in a dangling "do-block" notation.
The following pseudocode illustrates this structure:

~~~~ {#DoBlock.hs:doblock .programlisting}
-- file: ch17/DoBlock.hs
useAsCString str $ \cstr -> do
   ... operate on the C string
   ... return a result
~~~~

The second argument here is an anonymous function, a lambda, with a
monadic "do" block for a body. It is common to use the simple
`($)`{.code} application operator to avoid the need for parentheses when
delimiting the code block argument. This is a useful idiom to remember
when dealing with code block parameters like this.

### Allocating local C data: the Storable class

We can happily marshal `ByteString`{.code} data to C compatible types,
but the `pcre_compile`{.code} function also needs some pointers and
arrays in which to place its other return values. These should only
exist briefly, so we don't need complicated allocation strategies. Such
short-lifetime C data can be created with the `alloca`{.code} function:

~~~~ {#ForeignPtr.hs:alloc .programlisting}
-- file: ch17/ForeignPtr.hs
alloca :: Storable a => (Ptr a -> IO b) -> IO b
~~~~

This function takes a code block accepting a pointer to some C type as
an argument and arranges to call that function with the unitialised data
of the right shape, allocated freshly. The allocation mechanism mirrors
local stack variables in other languages. The allocated memory is
released once the argument function exits. In this way we get lexically
scoped allocation of low level data types, that are guaranteed to be
released once the scope is exited. We can use it to allocate any data
types that has an instance of the `Storable`{.code} type class. An
implication of overloading the allocation operator like this is that the
data type allocated can be inferred from type information, based on use!
Haskell will know what to allocate based on the functions we use on that
data.

To allocate a pointer to a `CString`{.code}, for example, which will be
updated to point to a particular `CString`{.code} by the called
function, we would call `alloca`{.code}, in pseudocode as:

~~~~ {#DoBlock.hs:alloc .programlisting}
-- file: ch17/DoBlock.hs
alloca $ \stringptr -> do
   ... call some Ptr CString function
   peek stringptr
~~~~

This locally allocates a `Ptr CString`{.code} and applies the code block
to that pointer, which then calls a C function to modify the pointer
contents. Finally, we dereference the pointer with the `Storable`{.code}
class `peek`{.code} function, yielding a `CString`{.code}.

We can now put it all together, to complete our high level PCRE
compilation wrapper.

### Putting it all together

We've decided what Haskell type to represent the C function with, what
the result data will be represented by, and how its memory will be
managed. We've chosen a representation for flags to the
`pcre_compile`{.code} function, and worked out how to get C strings to
and from code inspecting it. So let's write the complete function for
compiling PCRE regular expressions from Haskell:

~~~~ {#PCRE-compile.hs:compileReal .programlisting}
-- file: ch17/PCRE-compile.hs
compile :: ByteString -> [PCREOption] -> Either String Regex
compile str flags = unsafePerformIO $
  useAsCString str $ \pattern -> do
    alloca $ \errptr       -> do
    alloca $ \erroffset    -> do
        pcre_ptr <- c_pcre_compile pattern (combineOptions flags) errptr erroffset nullPtr
        if pcre_ptr == nullPtr
            then do
                err <- peekCString =<< peek errptr
                return (Left err)
            else do
                reg <- newForeignPtr finalizerFree pcre_ptr -- release with free()
                return (Right (Regex reg str))
~~~~

That's it! Let's carefully walk through the details here, since it is
rather dense. The first thing that stands out is the use of
`unsafePerformIO`{.code}, a rather infamous function, with a very
unusual type, imported from the ominous `System.IO.Unsafe`{.code}:

~~~~ {#ForeignPtr.hs:unsafePerformIO .programlisting}
-- file: ch17/ForeignPtr.hs
unsafePerformIO :: IO a -> a
~~~~

This function does something odd: it takes an IO value and converts it
to a pure one! After warning about the danger of effects for so long,
here we have the very enabler of dangerous effects in one line. Used
unwisely, this function lets us sidestep all safety guarantees the
Haskell type system provides, inserting arbitrary side effects into a
Haskell program, anywhere. The dangers in doing this are significant: we
can break optimizations, modify arbitrary locations in memory, remove
files on the user's machine, or launch nuclear missiles from our
Fibonacci sequences. So why does this function exist at all?

It exists precisely to enable Haskell to bind to C code that we know to
be referentially transparent, but can't prove the case to the Haskell
type system. It lets us say to the compiler, "I know what I'm doing -
this code really is pure". For regular expression compilation, we know
this to be the case: given the same pattern, we should get the same
regular expression matcher every time. However, proving that to the
compiler is beyond the Haskell type system, so we're forced to assert
that this code is pure. Using `unsafePerformIO`{.code} allows us to do
just that.

However, if we know the C code is pure, why don't we just declare it as
such, by giving it a pure type in the import declaration? For the reason
that we have to allocate local memory for the C function to work with,
which must be done in the IO monad, as it is a local side effect. Those
effects won't escape the code surrounding the foreign call, though, so
when wrapped, we use `unsafePerformIO`{.code} to reintroduce purity.

The argument to `unsafePerformIO`{.code} is the actual body of our
compilation function, which consists of four parts: marshalling Haskell
data to C form; calling into the C library; checking the return values;
and finally, constructing a Haskell value from the results.

We marshal with `useAsCString`{.code} and `alloca`{.code}, setting up
the data we need to pass to C, and use `combineOptions`{.code},
developed previously, to collapse the list of flags into a single
`CInt`{.code}. Once that's all in place, we can finally call
`c_pcre_compile`{.code} with the pattern, flags, and pointers for the
results. We use `nullPtr`{.code} for the character encoding table, which
is unused in this case.

The result returned from the C call is a pointer to the abstract
`PCRE`{.code} structure. We then test this against the `nullPtr`{.code}.
If there was a problem with the regular expression, we have to
dereference the error pointer, yielding a `CString`{.code}. We then
unpack that to a normal Haskell list with the library function,
`peekCString`{.code}. The final result of the error path is a value of
`Left err`{.code}, indicating failure to the caller.

If the call succeeded, however, we allocate a new storage-managed
pointer, with the C function using a `ForeignPtr`{.code}. The special
value `finalizerFree`{.code} is bound as the finalizer for this data,
which uses the standard C `free`{.code} to deallocate the data. This is
then wrapped as an opaque `Regex`{.code} value. The successful result is
tagged as such with `Right`{.code}, and returned to the user. And now
we're done!

We need to process our source file with hsc2hs, and then load the
function in GHCi. However, doing this results in an error on the first
attempt:

~~~~ {#id656005 .screen}
$ hsc2hs Regex.hsc
$ ghci Regex.hs

During interactive linking, GHCi couldn't find the following symbol:
  pcre_compile
This may be due to you not asking GHCi to load extra object files,
archives or DLLs needed by your current session.  Restart GHCi, specifying
the missing library using the -L/path/to/object/dir and -lmissinglibname
flags, or simply by naming the relevant files on the GHCi command line.
~~~~

A little scary. However, this is just because we didn't link the C
library we wanted to call to the Haskell code. Assuming the PCRE library
has been installed on the system in the default library location, we can
let GHCi know about it by adding `-lpcre`{.code} to the GHCi command
line. Now we can try out the code on some regular expressions, looking
at the success and error cases:

~~~~ {#id656047 .screen}
$ ghci Regex.hs -lpcre
*Regex> :m + Data.ByteString.Char8
*Regex Data.ByteString.Char8> compile (pack "a.*b") []
Right (Regex 0x00000000028882a0 "a.*b")
*Regex Data.ByteString.Char8> compile (pack "a.*b[xy]+(foo?)") []
Right (Regex 0x0000000002888860 "a.*b[xy]+(foo?)")
*Regex Data.ByteString.Char8> compile (pack "*") []
Left "nothing to repeat"
~~~~

The regular expressions are packed into byte strings and marshalled to
C, where they are compiled by the PCRE library. The result is then
handed back to Haskell, where we display the structure using the default
`Show`{.code} instance. Our next step is to pattern match some strings
with these compiled regular expressions.

Matching on strings
-------------------

The second part of a good regular expression library is the matching
function. Given a compiled regular expression, this function does the
matching of the compiled regex against some input, indicating whether it
matched, and if so, what parts of the string matched. In PCRE this
function is `pcre_exec`{.code}, which has type:

~~~~ {#PCRE-compile.h:pcre_exec .programlisting}
int pcre_exec(const pcre *code,
              const pcre_extra *extra,
              const char *subject,
              int length,
              int startoffset,
              int options,
              int *ovector,
              int ovecsize);
~~~~

The most important arguments are the input `pcre`{.code} pointer
structure, which we obtained from `pcre_compile`{.code}, and the subject
string. The other flags let us provide book keeping structures, and
space for return values. We can directly translate this type to the
Haskell import declaration:

~~~~ {#RegexExec.hs:pcreexec .programlisting}
-- file: ch17/RegexExec.hs
foreign import ccall "pcre.h pcre_exec"
    c_pcre_exec     :: Ptr PCRE
                    -> Ptr PCREExtra
                    -> Ptr Word8
                    -> CInt
                    -> CInt
                    -> PCREExecOption
                    -> Ptr CInt
                    -> CInt
                    -> IO CInt
~~~~

We use the same method as before to create typed pointers for the
`PCREExtra`{.code} structure, and a `newtype`{.code} to represent flags
passed at regex execution time. This lets us ensure users don't pass
compile time flags incorrectly at regex runtime.

### Extracting information about the pattern

The main complication involved in calling `pcre_exec`{.code} is the
array of `int`{.code} pointers used to hold the offsets of matching
substrings found by the pattern matcher. These offsets are held in an
offset vector, whose required size is determined by analysing the input
regular expression to determine the number of captured patterns it
contains. PCRE provides a function, `pcre_fullinfo`{.code}, for
determining much information about the regular expression, including the
number of patterns. We'll need to call this, and now, we can directly
write down the Haskell type for the binding to `pcre_fullinfo`{.code}
as:

~~~~ {#RegexExec.hs:pcrefullinfo .programlisting}
-- file: ch17/RegexExec.hs
foreign import ccall "pcre.h pcre_fullinfo"
    c_pcre_fullinfo :: Ptr PCRE
                    -> Ptr PCREExtra
                    -> PCREInfo
                    -> Ptr a
                    -> IO CInt
~~~~

The most important arguments to this function are the compiled regular
expression, and the `PCREInfo`{.code} flag, indicating which information
we're interested in. In this case, we care about the captured pattern
count. The flags are encoded in numeric constants, and we need to use
specifically the `PCRE_INFO_CAPTURECOUNT`{.code} value. There is a range
of other constants which determine the result type of the function,
which we can bind to using the `#enum`{.code} construct as before. The
final argument is a pointer to a location to store the information about
the pattern (whose size depends on the flag argument passed in!).

Calling `pcre_fullinfo`{.code} to determine the captured pattern count
is pretty easy:

~~~~ {#RegexExec.hs:captured .programlisting}
-- file: ch17/RegexExec.hs
capturedCount :: Ptr PCRE -> IO Int
capturedCount regex_ptr =
    alloca $ \n_ptr -> do
         c_pcre_fullinfo regex_ptr nullPtr info_capturecount n_ptr
         return . fromIntegral =<< peek (n_ptr :: Ptr CInt)
~~~~

This takes a raw PCRE pointer and allocates space for the `CInt`{.code}
count of the matched patterns. We then call the information function and
peek into the result structure, finding a `CInt`{.code}. Finally, we
convert this to a normal Haskell `Int`{.code} and pass it back to the
user.

### Pattern matching with substrings

Let's now write the regex matching function. The Haskell type for
matching is similar to that for compiling regular expressions:

~~~~ {#RegexExec.hs:exectype .programlisting}
-- file: ch17/RegexExec.hs
match :: Regex -> ByteString -> [PCREExecOption] -> Maybe [ByteString]
~~~~

This function is how users will match strings against compiled regular
expressions. Again, the main design point is that it is a pure function.
Matching is a pure function: given the same input regular expression and
subject string, it will always return the same matched substrings. We
convey this information to the user via the type signature, indicating
no side effects will occure when you call this function.

The arguments are a compiled `Regex`{.code}, a strict
`ByteString`{.code}, containing the input data, and a list of flags that
modify the regular expression engine's behaviour at runtime. The result
is either no match at all, indicated by a `Nothing`{.code} value, or
just a list of matched substrings. We use the `Maybe`{.code} type to
clearly indicate in the type that matching may fail. By using strict
`ByteString`{.code}s for the input data we can extract matched
substrings in constant time, without copying, making the interface
rather efficient. If substrings are matched in the input the offset
vector is populated with pairs of integer offsets into the subject
string. We'll need to loop over this result vector, reading offsets, and
building `ByteString`{.code} slices as we go.

The implementation of the match wrapper can be broken into three parts.
At the top level, our function takes apart the compiled `Regex`{.code}
structure, yielding the underlying `PCRE`{.code} pointer:

~~~~ {#RegexExec.hs:matchtop .programlisting}
-- file: ch17/RegexExec.hs
match :: Regex -> ByteString -> [PCREExecOption] -> Maybe [ByteString]
match (Regex pcre_fp _) subject os = unsafePerformIO $ do
  withForeignPtr pcre_fp $ \pcre_ptr -> do
    n_capt <- capturedCount pcre_ptr

    let ovec_size = (n_capt + 1) * 3
        ovec_bytes = ovec_size * sizeOf (undefined :: CInt)
~~~~

As it is pure, we can use `unsafePerformIO`{.code} to hide any
allocation effects internally. After pattern matching on the
`PCRE`{.code} type, we need to take apart the `ForeignPtr`{.code} that
hides our C-allocated raw PCRE data. We can use `withForeignPtr`{.code}.
This holds on to the Haskell data associated with the PCRE value while
the call is being made, preventing it from being collected for at least
the time it is used by this call. We then call the information function,
and use that value to compute the size of the offset vector (the formula
for which is given in the PCRE documentation). The number of bytes we
need is the number of elements multiplied by the size of a
`CInt`{.code}. To portably compute C type sizes, the `Storable`{.code}
class provides a `sizeOf`{.code} function, that takes some arbitrary
value of the required type (and we can use the `undefined`{.code} value
here to do our type dispatch).

The next step is to allocate an offset vector of the size we computed,
to convert the input `ByteString`{.code} into a pointer to a C
`char`{.code} array. Finally, we call `pcre_exec`{.code} with all the
required arguments:

~~~~ {#RegexExec.hs:alloc .programlisting}
-- file: ch17/RegexExec.hs
    allocaBytes ovec_bytes $ \ovec -> do

        let (str_fp, off, len) = toForeignPtr subject
        withForeignPtr str_fp $ \cstr -> do
            r <- c_pcre_exec
                         pcre_ptr
                         nullPtr
                         (cstr `plusPtr` off)
                         (fromIntegral len)
                         0
                         (combineExecOptions os)
                         ovec
                         (fromIntegral ovec_size)
~~~~

For the offset vector, we use `allocaBytes`{.code} to control exactly
the size of the allocated array. It is like `alloca`{.code}, but rather
than using the `Storable`{.code} class to determine the required size,
it takes an explicit size in bytes to allocate. Taking apart
`ByteString`{.code}'s, yielding the underlying pointer to memory they
contain, is done with `toForeignPtr`{.code}, which converts our nice
`ByteString`{.code} type into a managed pointer. Using
`withForeignPtr`{.code} on the result gives us a raw `Ptr CChar`{.code},
which is exactly what we need to pass the input string to C. Programming
in Haskell is often just solving a type puzzle!

We then just call `c_pcre_exec`{.code} with the raw PCRE pointer, the
input string pointer at the correct offset, its length, and the result
vector pointer. A status code is returned, and, finally, we analyse the
result:

~~~~ {#RegexExec.hs:results .programlisting}
-- file: ch17/RegexExec.hs
            if r < 0
                then return Nothing
                else let loop n o acc =
                            if n == r
                              then return (Just (reverse acc))
                              else do
                                    i <- peekElemOff ovec o
                                    j <- peekElemOff ovec (o+1)
                                    let s = substring i j subject
                                    loop (n+1) (o+2) (s : acc)
                     in loop 0 0 []

  where
    substring :: CInt -> CInt -> ByteString -> ByteString
    substring x y _ | x == y = empty
    substring a b s = end
        where
            start = unsafeDrop (fromIntegral a) s
            end   = unsafeTake (fromIntegral (b-a)) start
~~~~

If the result value was less than zero, then there was an error, or no
match, so we return `Nothing`{.code} to the user. Otherwise, we need a
loop peeking pairs of offsets from the offset vector (via
`peekElemOff`{.code}). Those offsets are used to find the matched
substrings. To build substrings we use a helper function that, given a
start and end offset, drops the surrounding portions of the subject
string, yielding just the matched portion. The loop runs until it has
extracted the number of substrings we were told were found by the
matcher.

The substrings are accumulated in a tail recursive loop, building up a
reverse list of each string. Before returning the substrings of the
user, we need to flip that list around and wrap it in a successful
`Just`{.code} tag. Let's try it out!

### The real deal: compiling and matching regular expressions

If we take this function, its surrounding hsc2hs definitions and data
wrappers, and process it with hsc2hs, we can load the resulting Haskell
file in GHCi and try out our code (we need to import
`Data.ByteString.Char8`{.code} so we can build `ByteString`{.code}s from
string literals):

~~~~ {#id656632 .screen}
$ hsc2hs Regex.hsc
$ ghci Regex.hs -lpcre
*Regex> :t compile
compile :: ByteString -> [PCREOption] -> Either String Regex
*Regex> :t match
match :: Regex -> ByteString -> Maybe [ByteString]
~~~~

Things seem to be in order. Now let's try some compilation and matching.
First, something easy:

~~~~ {#id656689 .screen}
*Regex> :m + Data.ByteString.Char8
*Regex Data.ByteString.Char8> let Right r = compile (pack "the quick brown fox") []
*Regex Data.ByteString.Char8> match r (pack "the quick brown fox") []
Just ["the quick brown fox"]
*Regex Data.ByteString.Char8> match r (pack "The Quick Brown Fox") []
Nothing
*Regex Data.ByteString.Char8> match r (pack "What
  do you know about the quick brown fox?") []
Just ["the quick brown fox"]
~~~~

(We could also avoid the `pack`{.code} calls by using the
`OverloadedStrings`{.code} extensions). Or we can be more adventurous:

~~~~ {#id656765 .screen}
*Regex Data.ByteString.Char8> let Right r = compile (pack "a*abc?xyz+pqr{3}ab{2,}xy{4,5}pq{0,6}AB{0,}zz") []
*Regex Data.ByteString.Char8> match r (pack "abxyzpqrrrabbxyyyypqAzz") []
Just ["abxyzpqrrrabbxyyyypqAzz"]
*Regex Data.ByteString.Char8> let Right r = compile (pack "^([^!]+)!(.+)=apquxz\\.ixr\\.zzz\\.ac\\.uk$") []
*Regex Data.ByteString.Char8> match r (pack "abc!pqr=apquxz.ixr.zzz.ac.uk") []
Just ["abc!pqr=apquxz.ixr.zzz.ac.uk","abc","pqr"]
~~~~

That's pretty awesome. The full power of Perl regular expressions, in
Haskell at your fingertips.

In this chapter we've looked at how to declare bindings that let Haskell
code call C functions, how to marshal different data types between the
two languages, how to allocate memory at a low level (by allocating
locally, or via C's memory management), and how to automate much of the
hard work of dealing with C by exploiting the Haskell type system and
garbage collector. Finally, we looked at how FFI preprocessors can ease
much of the labour of constructing new bindings. The result is a natural
Haskell API, that is actually implemented primarily in C.

The majority of FFI tasks fall into the above categories. Other advanced
techniques that we are unable to cover include: linking Haskell into C
programs, registering callbacks from one language to another, and the
`c2hs`{.code} preprocessing tool. More information about these topics
can be found online.

\

* * * * *

^[[36](#id654015)]^Some more advanced binding tools provide greater
degrees of type checking. For example, `c2hs`{.code} is able to parse
the C header, and generate the binding definition for you, and is
especially suited for large projects where the full API is specified.

![image](/support/figs/rss.png) Want to stay up to date? Subscribe to
the comment feed for [this chapter](/feeds/comments/), or the [entire
book](/feeds/comments/).

Copyright 2007, 2008 Bryan O'Sullivan, Don Stewart, and John Goerzen.
This work is licensed under a [Creative Commons
Attribution-Noncommercial 3.0
License](http://creativecommons.org/licenses/by-nc/3.0/). Icons by [Paul
Davey](mailto:mattahan@gmail.com) aka
[Mattahan](http://mattahan.deviantart.com/).

  --------------------------- -------------------- ---------------------------------
  [Prev](using-parsec.html)                        [Next](monad-transformers.html)
  Chapter 16. Using Parsec    [Home](index.html)   Chapter 18. Monad transformers
  --------------------------- -------------------- ---------------------------------


