[Real World Haskell](/)by Bryan O'Sullivan, Don Stewart, and John Goerzen
-------------------------------------------------------------------------

Chapter 8. Efficient file processing, regular expressions, and file name
matching

[Prev](io.html)

[Next](io-case-study-a-library-for-searching-the-filesystem.html)

Chapter 8. Efficient file processing, regular expressions, and file name matching
---------------------------------------------------------------------------------

**Table of Contents**

[Efficient file
processing](efficient-file-processing-regular-expressions-and-file-name-matching.html#glob-bytestring)

[Binary I/O and qualified
imports](efficient-file-processing-regular-expressions-and-file-name-matching.html#id616188)

[Text
I/O](efficient-file-processing-regular-expressions-and-file-name-matching.html#id616460)

[File name
matching](efficient-file-processing-regular-expressions-and-file-name-matching.html#glob.fnmatch)

[Regular expressions in
Haskell](efficient-file-processing-regular-expressions-and-file-name-matching.html#glob.regex)

[The many types of
result](efficient-file-processing-regular-expressions-and-file-name-matching.html#id617251)

[More about regular
expressions](efficient-file-processing-regular-expressions-and-file-name-matching.html#id617813)

[Mixing and matching string
types](efficient-file-processing-regular-expressions-and-file-name-matching.html#id617819)

[Other things you should
know](efficient-file-processing-regular-expressions-and-file-name-matching.html#id618092)

[Translating a glob pattern into a regular
expression](efficient-file-processing-regular-expressions-and-file-name-matching.html#glob.translate)

[Exercises](efficient-file-processing-regular-expressions-and-file-name-matching.html#id618693)

[An important aside: writing lazy
functions](efficient-file-processing-regular-expressions-and-file-name-matching.html#glob.lazyfn)

[Making use of our pattern
matcher](efficient-file-processing-regular-expressions-and-file-name-matching.html#glob.use)

[Exercises](efficient-file-processing-regular-expressions-and-file-name-matching.html#id619751)

[Handling errors through API
design](efficient-file-processing-regular-expressions-and-file-name-matching.html#glob.errapi)

[Exercises](efficient-file-processing-regular-expressions-and-file-name-matching.html#id620049)

[Putting our code to
work](efficient-file-processing-regular-expressions-and-file-name-matching.html#glob.work)

[Exercises](efficient-file-processing-regular-expressions-and-file-name-matching.html#glob.exercises)

Efficient file processing
-------------------------

This simple microbenchmark reads a text file full of numbers, and prints
their sum.

~~~~ {#SumFile.hs:main .programlisting}
-- file: ch08/SumFile.hs
main = do
    contents <- getContents
    print (sumFile contents)
  where sumFile = sum . map read . words
~~~~

Although the String type is the default used for reading and writing
files, it is not efficient, so a simple program like this will perform
badly.

A String is represented as a list of Char values; each element of a list
is allocated individually, and has some book-keeping overhead. These
factors affect the memory consumption and performance of a program that
must read or write text or binary data. On simple benchmarks like this,
even programs written in interpreted languages such as Python can
outperform Haskell code that uses String by an order of magnitude.

The `bytestring`{.code} library provides a fast, cheap alternative to
the String type. Code written with `bytestring`{.code} can often match
or exceed the performance and memory footprint of C, while maintaining
Haskell's expressivity and conciseness.

The library supplies two modules. Each defines functions that are nearly
drop-in replacements for their String counterparts.

-   The `Data.ByteString`{.code} module defines a *strict* type named
    ByteString. This represents a string of binary or text data in a
    single array.

-   The `Data.ByteString.Lazy`{.code} module provides a *lazy* type,
    also named ByteString. This represents a string of data as a list of
    *chunks*, arrays of up to 64KB in size.

Each ByteString type performs better under particular circumstances. For
streaming a large quantity (hundreds of megabytes to terabytes) of data,
the lazy ByteString type is usually best. Its chunk size is tuned to be
friendly to a modern CPU's L1 cache, and a garbage collector can quickly
discard chunks of streamed data that are no longer being used.

The strict ByteString type performs best for applications that are less
concerned with memory footprint, or that need to access data randomly.

### Binary I/O and qualified imports

Let's develop a small function to illustrate some of the
`ByteString`{.code} API. We will determine if a file is an ELF object
file: this is the format used for executables on almost all modern
Unix-like systems.

This is a simple matter of looking at the first four bytes in the file,
and seeing if they match a specific sequence of bytes. A byte sequence
that identifies a file's type is often known as a *magic number*.

~~~~ {#ElfMagic.hs:hasElfMagic .programlisting}
-- file: ch08/ElfMagic.hs
import qualified Data.ByteString.Lazy as L

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
    where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]
~~~~

We import the `ByteString`{.code} modules using Haskell's *qualified
import* syntax, the `import qualified`{.code} that we see above. This
lets us refer to a module with a name of our choosing.

For instance, when we want to refer to the lazy `ByteString`{.code}
module's `take`{.function} function, we must write `L.take`{.function},
since we imported the module under the name `L`{.code}. If we are not
explicit about which version of e.g. `take`{.function} we want, the
compiler will report an error.

We will always use qualified import syntax with the `ByteString`{.code}
modules, because they provide many functions that have the same names as
Prelude functions.

![[Tip]](/support/figs/tip.png)

Tip

Qualified imports make it easy to switch between ByteString types. All
you should need to do is modify an `import`{.code} declaration at the
top of your source file; the rest of your code will probably not need
any changes. You can thus handily benchmark the two types, to see which
is best suited to your application's needs

Whether or not we use qualified imports, we can always use the entire
name of a module to identify something unambiguously. For instance, both
`Data.ByteString.Lazy.length`{.function} and `L.length`{.function}
identify the same function, as do `Prelude.sum`{.function} and
`sum`{.function}.

The lazy and strict `ByteString`{.code} modules are intended for binary
I/O. The Haskell data type for representing bytes is Word8; if we need
to refer to it by name, we import it from the `Data.Word`{.code} module.

The `L.pack`{.function} function takes a list of Word8 values, and packs
them into a lazy ByteString. (The `L.unpack`{.function} function
performs the reverse conversion.) Our `hasElfMagic`{.function} function
simply compares the first four bytes of a `ByteString`{.code} against a
magic number.

We are writing in classic Haskell style, where our
`hasElfMagic`{.function} function does not perform I/O. Here is the
function that uses it on a file.

~~~~ {#ElfMagic.hs:isElfFile .programlisting}
-- file: ch08/ElfMagic.hs
isElfFile :: FilePath -> IO Bool
isElfFile path = do
  content <- L.readFile path
  return (hasElfMagic content)
~~~~

The `L.readFile`{.function} function is the lazy ByteString equivalent
of `readFile`{.function}. It operates lazily, reading the file as data
is demanded. It is also efficient, reading chunks of up to 64KB at once.
The lazy ByteString is a good choice for our task: since we only need to
read at most the first four bytes of the file, we can safely use this
function on a file of any size.

### Text I/O

For convenience, the `bytestring`{.code} library provides two other
modules with limited text I/O capabilities,
`Data.ByteString.Char8`{.code} and `Data.ByteString.Lazy.Char8`{.code}.
These expose individual string elements as Char instead of Word8.

![[Warning]](/support/figs/warning.png)

Warning

The functions in these modules only work with byte-sized Char values, so
they are only suitable for use with ASCII and some European character
sets. Values above 255 are truncated.

The character-oriented `bytestring`{.code} modules provide useful
functions for text processing. Here is a file that contains monthly
stock prices for a well-known Internet company from mid-2008.

~~~~ {#highestClose.ghci:readFile .screen}
ghci> putStr =<< readFile "prices.csv"
Date,Open,High,Low,Close,Volume,Adj Close
2008-08-01,20.09,20.12,19.53,19.80,19777000,19.80
2008-06-30,21.12,21.20,20.60,20.66,17173500,20.66
2008-05-30,27.07,27.10,26.63,26.76,17754100,26.76
2008-04-30,27.17,27.78,26.76,27.41,30597400,27.41
~~~~

How can we find the highest closing price from a series of entries like
this? Closing prices are in the fourth comma-separated column. This
function obtains a closing price from one line of data.

~~~~ {#HighestClose.hs:closing .programlisting}
-- file: ch08/HighestClose.hs
import qualified Data.ByteString.Lazy.Char8 as L

closing = readPrice . (!!4) . L.split ','
~~~~

Since this function is written in point-free style, we read from right
to left. The `L.split`{.function} function splits a lazy ByteString into
a list of them, every time it finds a matching character. The
`(!!)`{.function} operator retrieves the *k*th element of a list. Our
`readPrice`{.function} function turns a string representing a fractional
price into a whole number.

~~~~ {#HighestClose.hs:readPrice .programlisting}
-- file: ch08/HighestClose.hs
readPrice :: L.ByteString -> Maybe Int
readPrice str =
    case L.readInt str of
      Nothing             -> Nothing
      Just (dollars,rest) ->
        case L.readInt (L.tail rest) of
          Nothing           -> Nothing
          Just (cents,more) ->
            Just (dollars * 100 + cents)
~~~~

We use the `L.readInt`{.function} function, which parses an integer. It
returns both the integer and the remainder of the string once a run of
digits is consumed. Our definition is slightly complicated by
`L.readInt`{.function} returning `Nothing`{.code} if parsing fails.

Our function for finding the highest closing price is straightforward.

~~~~ {#HighestClose.hs:highestClose .programlisting}
-- file: ch08/HighestClose.hs
highestClose = maximum . (Nothing:) . map closing . L.lines

highestCloseFrom path = do
    contents <- L.readFile path
    print (highestClose contents)
~~~~

We use one trick to work around the fact that we cannot supply an empty
list to the `maximum`{.function} function.

~~~~ {#highestClose.ghci:maximum .screen}
ghci> maximum [3,6,2,9]
9
ghci> maximum []
*** Exception: Prelude.maximum: empty list
~~~~

Since we do not want our code to throw an exception if we have no stock
data, the `(Nothing:)`{.code} expression ensures that the list of Maybe
Int values that we supply to `maximum`{.function} will never be empty.

~~~~ {#highestClose.ghci:maxList .screen}
ghci> maximum [Nothing, Just 1]
Just 1
ghci> maximum [Nothing]
Nothing
~~~~

Does our function work?

~~~~ {#highestClose.ghci:highestClose .screen}
ghci> :load HighestClose
[1 of 1] Compiling Main             ( HighestClose.hs, interpreted )
Ok, modules loaded: Main.
ghci> highestCloseFrom "prices.csv"
Loading package array-0.1.0.0 ... linking ... done.
Loading package bytestring-0.9.0.1 ... linking ... done.
Just 2741
~~~~

Since we have separated our I/O from our logic, we can test the no-data
case without having to create an empty file.

~~~~ {#highestClose.ghci:highestEmpty .screen}
ghci> highestClose L.empty
Nothing
~~~~

File name matching
------------------

Many systems-oriented programming languages provide library routines
that let us match a file name against a pattern, or that will give a
list of files that match the pattern. In other languages, this function
is often named `fnmatch`{.function}.) Although Haskell's standard
library generally has good systems programming facilities, it doesn't
provide these kinds of pattern matching functions. We'll take this as an
opportunity to develop our own.

The kinds of patterns we'll be dealing with are commonly referred to as
*glob patterns* (the term we'll use), wild card patterns, or shell-style
patterns. They have just a few simple rules. You probably already know
them, but we'll quickly recap here.

-   Matching a string against a pattern starts at the beginning of the
    string, and finishes at the end.

-   Most literal characters match themselves. For example, the text
    `foo`{.literal} in a pattern will match `foo`{.literal}, and only
    `foo`{.literal}, in an input string.

-   The `*`{.literal} (asterisk) character means “match anything”; it
    will match any text, including the empty string. For instance, the
    pattern `foo*`{.code} will match any string that begins with
    `foo`{.code}, such as `foo`{.code} itself, `foobar`{.code}, or
    `foo.c`{.code}. The pattern `quux*.c`{.code} will match any string
    that begins with `quux`{.code} and ends in `.c`{.code}, such as
    `quuxbaz.c`{.code}.

-   The `?`{.literal} (question mark) character matches any single
    character. The pattern `pic??.jpg`{.code} will match names like
    `picaa.jpg`{.code} or `pic01.jpg`{.code}.

-   A `[`{.literal} (open square bracket) character begins a *character
    class*, which is ended by a `]`{.literal}. Its meaning is “match any
    character in this class”. A character class can be *negated* by
    following the opening `[`{.literal} with a `!`{.literal}, so that it
    means “match any character *not* in this class”.

    As a shorthand, a character followed by a `-`{.literal} (dash),
    followed by another character, denotes a *range*: “match any
    character within this set”.

    Character classes have an added subtlety; they can't be empty. The
    first character after the opening `[`{.literal} or `[!`{.literal} is
    part of the class, so we can write a class containing the
    `]`{.literal} character as `[]aeiou]`{.literal}. The pattern
    `pic[0-9].[pP][nN][gG]`{.code} will match a name consisting of the
    string `pic`{.code}, followed by a single digit, followed by any
    capitalization of the strig `.png`{.code}.

While Haskell doesn't provide a way to match glob patterns among its
standard libraries, it provides a good regular expression matching
library. Glob patterns are nothing more than cut-down regular
expressions with slightly different syntax. It's easy to convert glob
patterns into regular expressions, but to do so, we must first
understand how to use regular expressions in Haskell.

Regular expressions in Haskell
------------------------------

In this section, we will be assume that you are already familiar with
regular expressions by way of some other language, such as Python, Perl,
or Java^[[26](#ftn.id617121)]^.

For brevity, we will abbreviate “regular expression” as *regexp* from
here on.

Rather than introduce regexps as something new, we will focus on what's
different about regexp handling in Haskell, compared to other languages.
Haskell's regular expression matching libraries are a lot more
expressive than those of other languages, so there's plenty to talk
about.

To begin our exploration of the regexp libraries, the only module we'll
need to work with is `Text.Regex.Posix`{.classname}. As usual, the most
convenient way to explore this module is by interacting with it via
**ghci**.

~~~~ {#regexp.ghci:load .screen}
ghci> :module +Text.Regex.Posix
~~~~

The only function that we're likely to need for normal use is the regexp
matching function, an infix operator named `(=~)`{.function} (borrowed
from Perl). The first hurdle to overcome is that Haskell's regexp
libraries make heavy use of polymorphism. As a result, the type
signature of the `(=~)`{.function} operator is difficult to understand,
so we will not explain it here.

The `=~`{.function} operator uses typeclasses for both of its arguments,
and also for its return type. The first argument (on the left of the
`=~`{.function}) is the text to match; the second (on the right) is the
regular expression to match against. We can pass either a String or a
ByteString as either argument.

### The many types of result

The `=~`{.function} operator is polymorphic in its return type, so the
Haskell compiler needs some way to know what type of result we would
like. In real code, it may be able to infer the right type, due to the
way we subsequently use the result. But such cues are often lacking when
we're exploring with **ghci**. If we omit a specific type for the
result, we'll get an error from the interpreter, as it does not have
enough information to successfuly infer the result type.

When **ghci** can't infer the `target`{.varname} type, we tell it what
we'd like the type to be. If we want a result of type Bool, we'll get a
pass/fail answer.

~~~~ {#regexp.ghci:bool.passfail .screen}
ghci> "my left foot" =~ "foo" :: Bool
Loading package array-0.1.0.0 ... linking ... done.
Loading package containers-0.1.0.1 ... linking ... done.
Loading package bytestring-0.9.0.1 ... linking ... done.
Loading package mtl-1.1.0.0 ... linking ... done.
Loading package regex-base-0.93.1 ... linking ... done.
Loading package regex-posix-0.93.1 ... linking ... done.
True
ghci> "your right hand" =~ "bar" :: Bool
False
ghci> "your right hand" =~ "(hand|foot)" :: Bool
True
~~~~

In the bowels of the regexp libraries, there's a typeclass named
`RegexContext`{.classname} that describes how a `target`{.varname} type
should behave; the base library defines many instances of this typeclass
for us. The Bool type is an instance of this typeclass, so we get back a
usable result. Another such instance is Int, which gives us a count of
the number of times the regexp matches.

~~~~ {#regexp.ghci:int.count .screen}
ghci> "a star called henry" =~ "planet" :: Int
0
ghci> "honorificabilitudinitatibus" =~ "[aeiou]" :: Int
13
~~~~

If we ask for a String result, we'll get the first substring that
matches, or an empty string if nothing matches.

~~~~ {#regexp.ghci:string .screen}
ghci> "I, B. Ionsonii, uurit a lift'd batch" =~ "(uu|ii)" :: String
"ii"
ghci> "hi ludi, F. Baconis nati, tuiti orbi" =~ "Shakespeare" :: String
""
~~~~

Another valid type of result is [String], which returns a list of *all*
matching strings.

~~~~ {#regexp.ghci:list.string .screen}
ghci> "I, B. Ionsonii, uurit a lift'd batch" =~ "(uu|ii)" :: [String]

<interactive>:1:0:
    No instance for (RegexContext Regex [Char] [String])
      arising from a use of `=~' at <interactive>:1:0-50
    Possible fix:
      add an instance declaration for
      (RegexContext Regex [Char] [String])
    In the expression:
            "I, B. Ionsonii, uurit a lift'd batch" =~ "(uu|ii)" :: [String]
    In the definition of `it':
        it = "I, B. Ionsonii, uurit a lift'd batch" =~ "(uu|ii)" ::
             [String]
ghci> "hi ludi, F. Baconis nati, tuiti orbi" =~ "Shakespeare" :: [String]

<interactive>:1:0:
    No instance for (RegexContext Regex [Char] [String])
      arising from a use of `=~' at <interactive>:1:0-54
    Possible fix:
      add an instance declaration for
      (RegexContext Regex [Char] [String])
    In the expression:
            "hi ludi, F. Baconis nati, tuiti orbi" =~ "Shakespeare" :: [String]
    In the definition of `it':
        it = "hi ludi, F. Baconis nati, tuiti orbi" =~ "Shakespeare" ::
             [String]
~~~~

![[Note]](/support/figs/note.png)

Watch out for String results

If you want a result that's a plain String, beware. Since
`(=~)`{.function} returns an empty string to signify “no match”, this
poses an obvious difficulty if the empty string could also be a valid
match for the regexp. If such a case arises, you should use a different
return type instead, such as [String].

That's about it for “simple” result types, but we're not by any means
finished. Before we continue, let's use a single pattern for our
remaining examples. We can define this pattern as a variable in
**ghci**, to save a little typing.

~~~~ {#regexp.ghci:let .screen}
ghci> let pat = "(foo[a-z]*bar|quux)"
~~~~

We can obtain quite a lot of information about the context in which a
match occurs. If we ask for a (String, String, String) tuple, we'll get
back the text *before* the first match, the text *of* that match, and
the text that *follows* it.

~~~~ {#regexp.ghci:3tuple.match .screen}
ghci> "before foodiebar after" =~ pat :: (String,String,String)
("before ","foodiebar"," after")
~~~~

If the match fails, the entire text is returned as the “before” element
of the tuple, with the other two elements left empty.

~~~~ {#regexp.ghci:3tuple.nomatch .screen}
ghci> "no match here" =~ pat :: (String,String,String)
("no match here","","")
~~~~

Asking for a four-element tuple gives us a fourth element that's a list
of all groups in the pattern that matched.

~~~~ {#regexp.ghci:4tuple .screen}
ghci> "before foodiebar after" =~ pat :: (String,String,String,[String])
("before ","foodiebar"," after",["foodiebar"])
~~~~

We can get numeric information about matches, too. A pair of Ints gives
us the starting offset of the first match, and its length. If we ask for
a list of these pairs, we'll get this information for all matches.

~~~~ {#regexp.ghci:2tuple.match .screen}
ghci> "before foodiebar after" =~ pat :: (Int,Int)
(7,9)
ghci> "i foobarbar a quux" =~ pat :: [(Int,Int)]

<interactive>:1:0:
    No instance for (RegexContext Regex [Char] [(Int, Int)])
      arising from a use of `=~' at <interactive>:1:0-26
    Possible fix:
      add an instance declaration for
      (RegexContext Regex [Char] [(Int, Int)])
    In the expression: "i foobarbar a quux" =~ pat :: [(Int, Int)]
    In the definition of `it':
        it = "i foobarbar a quux" =~ pat :: [(Int, Int)]
~~~~

A failed match is represented by the value `-1`{.literal} as the first
element of the tuple (the match offset) if we've asked for a single
tuple, or an empty list if we've asked for a list of tuples.

~~~~ {#regexp.ghci:2tuple.nomatch .screen}
ghci> "eleemosynary" =~ pat :: (Int,Int)
(-1,0)
ghci> "mondegreen" =~ pat :: [(Int,Int)]

<interactive>:1:0:
    No instance for (RegexContext Regex [Char] [(Int, Int)])
      arising from a use of `=~' at <interactive>:1:0-18
    Possible fix:
      add an instance declaration for
      (RegexContext Regex [Char] [(Int, Int)])
    In the expression: "mondegreen" =~ pat :: [(Int, Int)]
    In the definition of `it': it = "mondegreen" =~ pat :: [(Int, Int)]
~~~~

This is not a comprehensive list of built-in instances of the
`RegexContext`{.classname} typeclass. For a complete list, see the
documentation for the `Text.Regex.Base.Context`{.classname} module.

This ability to make a function polymorphic in its result type is an
unusual feature for a statically typed language.

More about regular expressions
------------------------------

### Mixing and matching string types

As we noted earlier, the `=~`{.function} operator uses typeclasses for
its argument types and its return type. We can use either String or
strict ByteString values for both the regular expression and the text to
match against.

~~~~ {#regexp.ghci:mix.pack .screen}
ghci> :module +Data.ByteString.Char8
ghci> :type pack "foo"
pack "foo" :: ByteString
~~~~

We can then try using different combinations of String and ByteString.

~~~~ {#regexp.ghci:mix.simple .screen}
ghci> pack "foo" =~ "bar" :: Bool
False
ghci> "foo" =~ pack "bar" :: Int
0
ghci> pack "foo" =~ pack "o" :: [(Int, Int)]

<interactive>:1:0:
    No instance for (RegexContext Regex ByteString [(Int, Int)])
      arising from a use of `=~' at <interactive>:1:0-21
    Possible fix:
      add an instance declaration for
      (RegexContext Regex ByteString [(Int, Int)])
    In the expression: pack "foo" =~ pack "o" :: [(Int, Int)]
    In the definition of `it':
        it = pack "foo" =~ pack "o" :: [(Int, Int)]
~~~~

However, we need to be aware that if we want a string value in the
result of a match, the text we're matching against must be the same type
of string. Let's see what this means in practice.

~~~~ {#regexp.ghci:mix.match .screen}
ghci> pack "good food" =~ ".ood" :: [ByteString]

<interactive>:1:0:
    No instance for (RegexContext Regex ByteString [ByteString])
      arising from a use of `=~' at <interactive>:1:0-25
    Possible fix:
      add an instance declaration for
      (RegexContext Regex ByteString [ByteString])
    In the expression: pack "good food" =~ ".ood" :: [ByteString]
    In the definition of `it':
        it = pack "good food" =~ ".ood" :: [ByteString]
~~~~

In the above example, we've used the `pack`{.function} to turn a String
into a ByteString. The type checker accepts this because ByteString
appears in the result type. But if we try getting a String out, that
*won't* work.

~~~~ {#regexp.ghci:mix.nomatch .screen}
ghci> "good food" =~ ".ood" :: [ByteString]

<interactive>:1:0:
    No instance for (RegexContext Regex [Char] [ByteString])
      arising from a use of `=~' at <interactive>:1:0-20
    Possible fix:
      add an instance declaration for
      (RegexContext Regex [Char] [ByteString])
    In the expression: "good food" =~ ".ood" :: [ByteString]
    In the definition of `it':
        it = "good food" =~ ".ood" :: [ByteString]
~~~~

We can easily fix this problem by making the string types of the left
hand side and the result match once again.

~~~~ {#regexp.ghci:mix.rematch .screen}
ghci> "good food" =~ ".ood" :: [String]

<interactive>:1:0:
    No instance for (RegexContext Regex [Char] [String])
      arising from a use of `=~' at <interactive>:1:0-20
    Possible fix:
      add an instance declaration for
      (RegexContext Regex [Char] [String])
    In the expression: "good food" =~ ".ood" :: [String]
    In the definition of `it': it = "good food" =~ ".ood" :: [String]
~~~~

This restriction does *not* apply to the type of the regexp we're
matching against. It can be either a String or ByteString, unconstrained
by the other types in use.

### Other things you should know

When you look through Haskell library documentation, you'll see several
regexp-related modules. The modules under `Text.Regex.Base`{.classname}
define the common API adhered to by all of the other regexp modules.
It's possible to have multiple implementations of the regexp API
installed at one time. At the time of writing, GHC is bundled with one
implementation, `Text.Regex.Posix`{.classname}. As its name suggests,
this package provides POSIX regexp semantics.

![[Note]](/support/figs/note.png)

Perl and POSIX regular expressions

If you're coming to Haskell from a language like Perl, Python, or Java,
and you've used regular expressions in one of those languages, you
should be aware that the POSIX regexps handled by the
`Text.Regex.Posix`{.classname} module are different in some significant
ways from Perl-style regexps. Here are a few of the more notable
differences.

Perl regexp engines perform left-biased matching when matching
alternatives, whereas POSIX engines choose the greediest match. What
this means is that given a regexp of `(foo|fo*)`{.literal} and a text
string of `foooooo`{.literal}, a Perl-style engine will give a match of
`foo`{.literal} (the leftmost match), while a POSIX engine will match
the entire string (the greediest match).

POSIX regexps have less uniform syntax than Perl-style regexps. They
also lack a number of capabilities provided by Perl-style regexps, such
as zero-width assertions and control over greedy matching.

Other Haskell regexp packages are available for download from Hackage.
Some provide better performance than the current POSIX engine (e.g.
`regex-tdfa`{.code}); others provide the Perl-style matching that most
programmers are now familiar with (e.g. `regex-pcre`{.code}). All follow
the standard API that we have covered in this section.

Translating a glob pattern into a regular expression
----------------------------------------------------

Now that we've seen the myriad of ways to match text against regular
expressions, let's turn our attention back to glob patterns. We want to
write a function that will take a glob pattern and return its
representation as a regular expression. Both glob patterns and regexps
are text strings, so the type that our function ought to have seems
clear.

~~~~ {#GlobRegex.hs:type .programlisting}
-- file: ch08/GlobRegex.hs
module GlobRegex
    (
      globToRegex
    , matchesGlob
    ) where

import Text.Regex.Posix ((=~))

globToRegex :: String -> String
~~~~

The regular expression that we generate must be *anchored*, so that it
starts matching from the beginning of a string and finishes at the end.

~~~~ {#GlobRegex.hs:rooted .programlisting}
-- file: ch08/GlobRegex.hs
globToRegex cs = '^' : globToRegex' cs ++ "$"
~~~~

Recall that the String is just a synonym for [Char], a list of Chars.
The `:`{.function} operator puts a value (the `^`{.literal} character in
this case) onto the front of a list, where the list is the value
returned by the yet-to-be-seen `globToRegex'`{.function} function.

![[Note]](/support/figs/note.png)

Using a value before defining it

Haskell does not require that a value or function be declared or defined
in a source file before it's used. It's perfectly normal for a
definition to come *after* the first place it's used. The Haskell
compiler doesn't care about ordering at this level. This grants us the
flexibility to structure our code in the manner that makes most logical
sense to us, rather than follow an order that makes the compiler
writer's life easiest.

Haskell module writers often use this flexibility to put “more
important” code earlier in a source file, relegating “plumbing” to
later. This is exactly how we are presenting the
`globToRegex`{.function} function and its helpers here.

With the regular expression rooted, the `globToRegex'`{.function}
function will do the bulk of the translation work. We'll use the
convenience of Haskell's pattern matching to enumerate each of the cases
we'll need to cover.

~~~~ {#GlobRegex.hs:asterisk .programlisting}
-- file: ch08/GlobRegex.hs
globToRegex' :: String -> String
globToRegex' "" = ""

globToRegex' ('*':cs) = ".*" ++ globToRegex' cs

globToRegex' ('?':cs) = '.' : globToRegex' cs

globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs)     = '['  :  c : charClass cs
globToRegex' ('[':_)        = error "unterminated character class"

globToRegex' (c:cs) = escape c ++ globToRegex' cs
~~~~

Our first clause stipulates that if we hit the end of our glob pattern
(by which time we'll be looking at the empty string), we return
`$`{.literal}, the regular expression symbol for “match end-of-line”.
Following this is a series of clauses that switch our pattern from glob
syntax to regexp syntax. The last clause passes every other character
through, possibly escaping it first.

The `escape`{.function} function ensures that the regexp engine will not
interpret certain characters as pieces of regular expression syntax.

~~~~ {#GlobRegex.hs:escape .programlisting}
-- file: ch08/GlobRegex.hs
escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"
~~~~

The `charClass`{.function} helper function only checks that a character
class is correctly terminated. It passes its input through unmodified
until it hits a `]`{.literal}, when it hands control back to
`globToRegex'`{.function}.

~~~~ {#GlobRegex.hs:charClass .programlisting}
-- file: ch08/GlobRegex.hs
charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "unterminated character class"
~~~~

Now that we've finished defining `globToRegex`{.function} and its
helpers, let's load it into **ghci** and try it out.

~~~~ {#glob-regexp.ghci:real .screen}
ghci> :load GlobRegex.hs
[1 of 1] Compiling GlobRegex        ( GlobRegex.hs, interpreted )
Ok, modules loaded: GlobRegex.
ghci> :module +Text.Regex.Posix
ghci> globToRegex "f??.c"
Loading package array-0.1.0.0 ... linking ... done.
Loading package containers-0.1.0.1 ... linking ... done.
Loading package bytestring-0.9.0.1 ... linking ... done.
Loading package mtl-1.1.0.0 ... linking ... done.
Loading package regex-base-0.93.1 ... linking ... done.
Loading package regex-posix-0.93.1 ... linking ... done.
"^f..\\.c$"
~~~~

Sure enough, that looks like a reasonable regexp. Can we use it to match
against a string?

~~~~ {#glob-regexp.ghci:matches .screen}
ghci> "foo.c" =~ globToRegex "f??.c" :: Bool
True
ghci> "test.c" =~ globToRegex "t[ea]s*" :: Bool
True
ghci> "taste.txt" =~ globToRegex "t[ea]s*" :: Bool
True
~~~~

It works! Now let's play around a little with **ghci**. We can create a
temporary definition for `fnmatch`{.function} and try it out.

~~~~ {#glob-regexp.ghci:fnmatch .screen}
ghci> let fnmatch pat name  =  name =~ globToRegex pat :: Bool
ghci> :type fnmatch
fnmatch :: (RegexLike Regex source1) => String -> source1 -> Bool
ghci> fnmatch "d*" "myname"
False
~~~~

The name `fnmatch`{.function} doesn't really have the “Haskell nature”,
though. By far the most common Haskell style is for functions to have
descriptive, “camel cased” names. Camel casing concatenates words,
capitalising all but possibly the first word. For instance, the words
“file name matches” would become the name `fileNameMatches`{.code}. The
name “camel case” comes from the “humps” introduced by the capital
letters. In our library, we'll give this function the name
`matchesGlob`{.function}.

~~~~ {#GlobRegex.hs:matchesGlob .programlisting}
-- file: ch08/GlobRegex.hs
matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = name =~ globToRegex pat
~~~~

You may have noticed that most of the names that we have used for
variables so far have been short. As a rule of thumb, descriptive
variable names are more useful in longer function definitions, as they
aid readability. For a two-line function, a long variable name has less
value.

### Exercises

**1.**

Use **ghci** to explore what happens if you pass a malformed pattern,
such as `[`{.literal}, to `globToRegex`{.function}. Write a small
function that calls `globToRegex`{.function}, and pass it a malformed
pattern. What happens?

**2.**

While filesystems on Unix are usually sensitive to case (e.g. “G” vs.
“g”) in file names, Windows filesystems are not. Add a parameter to the
`globToRegex`{.function} and `matchesGlob`{.function} functions that
allows control over case sensitive matching.

An important aside: writing lazy functions
------------------------------------------

In an imperative language, the `globToRegex'`{.function} function is one
that we'd usually express as a loop. For example, Python's standard
fnmatch module includes a function named `translate`{.function} that
does exactly the same job as our `globToRegex`{.function} function. It's
written as a loop.

If you've been exposed to functional programming through a language such
as Scheme or ML, you've probably had drilled into your head the notion
that “the way to emulate a loop is via tail recursion”.

Looking at the `globToRegex'`{.function} function, we can see that it is
*not* tail recursive. To see why, examine its final clause again
(several of its other clauses are structured similarly).

~~~~ {#GlobRegex.hs:last .programlisting}
-- file: ch08/GlobRegex.hs
globToRegex' (c:cs) = escape c ++ globToRegex' cs
~~~~

It applies itself recursively, and the result of the recursive
application is used as a parameter to the `(++)`{.function} function.
Since the recursive application *isn't* the last thing the function
does, `globToRegex'`{.function} is not tail recursive.

Why is our definition of this function not tail recursive? The answer
lies with Haskell's non-strict evaluation strategy. Before we start
talking about that, let's quickly talk about why, in a traditional
language, we'd try to avoid this kind of recursive definition. Here is a
simpler definition, of the `(++)`{.function} operator. It is recursivem,
but not tail recursive.

~~~~ {#append.hs:append .programlisting}
-- file: ch08/append.hs
(++) :: [a] -> [a] -> [a]

(x:xs) ++ ys = x : (xs ++ ys)
[]     ++ ys = ys
~~~~

In a strict language, if we evaluate `"foo" ++     "bar"`{.code}, the
entire list is constructed, then returned. Non-strict evaluation defers
much of the work until it is needed.

If we demand an element of the expression `"foo" ++ "bar"`{.code}, the
first pattern of the function's definition matches, and we return the
expression `x : (xs     ++ ys)`{.code}. Because the `(:)`{.code}
constructor is non-strict, the evaluation of `xs ++ ys`{.code} can be
deferred: we generate more elements of the result at whatever rate they
are demanded. When we generate more of the result, we will no longer be
using `x`{.varname}, so the garbage collector can reclaim it. Since we
generate elements of the result on demand, and do not hold onto parts
that we are done with, the compiler can evaluate our code in constant
space.

Making use of our pattern matcher
---------------------------------

It's all very well to have a function that can match glob patterns, but
we'd like to be able to put this to practical use. On Unix-like systems,
the `glob`{.function} function returns the names of all files and
directories that match a given glob pattern. Let's build a similar
function in Haskell. Following the Haskell norm of descriptive naming,
we'll call our function `namesMatching`{.function}.

~~~~ {#Glob.hs:module .programlisting}
-- file: ch08/Glob.hs
module Glob (namesMatching) where
~~~~

We specify that `namesMatching`{.function} is the only name that users
of our `Glob`{.code} module will be able to see.

This function will obviously have to manipulate filesystem paths a lot,
splicing and joining them as it goes. We'll need to use a few previously
unfamiliar modules along the way.

The `System.Directory`{.classname} module provides standard functions
for working with directories and their contents.

~~~~ {#Glob.hs:import.directory .programlisting}
-- file: ch08/Glob.hs
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)
~~~~

The `System.FilePath`{.classname} module abstracts the details of an
operating system's path name conventions. The `(</>)`{.function}
function joins two path components.

~~~~ {#filepath.ghci:join .screen}
ghci> :m +System.FilePath
ghci> "foo" </> "bar"
Loading package filepath-1.1.0.0 ... linking ... done.
"foo/bar"
~~~~

The name of the `dropTrailingPathSeparator`{.function} function is
perfectly descriptive.

~~~~ {#filepath.ghci:dropTrailingPathSeparator .screen}
ghci> dropTrailingPathSeparator "foo/"
"foo"
~~~~

The `splitFileName`{.function} function splits a path at the last slash.

~~~~ {#filepath.ghci:splitFileName .screen}
ghci> splitFileName "foo/bar/Quux.hs"
("foo/bar/","Quux.hs")
ghci> splitFileName "zippity"
("","zippity")
~~~~

Using `System.FilePath`{.code} together with the
`System.Directory`{.classname} module, we can write a portable
`namesMatching`{.function} function that will run on both Unix-like and
Windows systems.

~~~~ {#Glob.hs:import.filepath .programlisting}
-- file: ch08/Glob.hs
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))
~~~~

In this module, we'll be emulating a “for” loop; getting our first taste
of exception handling in Haskell; and of course using the
`matchesGlob`{.function} function we just wrote.

~~~~ {#Glob.hs:import.rest .programlisting}
-- file: ch08/Glob.hs
import Control.Exception (handle)
import Control.Monad (forM)
import GlobRegex (matchesGlob)
~~~~

Since directories and files live in the “real world” of activities that
have effects, our globbing function will have to have `IO`{.classname}
in its result type.

If the string we're passed contains no pattern characters, we simply
check that the given name exists in the filesystem. (Notice that we use
Haskell's function guard syntax here to write a nice tidy definition. An
“if” would do, but isn't as aesthetically pleasing.)

~~~~ {#Glob.hs:namesMatching .programlisting}
-- file: ch08/Glob.hs
isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching pat
  | not (isPattern pat) = do
    exists <- doesNameExist pat
    return (if exists then [pat] else [])
~~~~

The name `doesNameExist`{.function} refers to a function that we will
define shortly.

What if the string *is* a glob pattern? Our function definition
continues.

~~~~ {#Glob.hs:namesMatching2 .programlisting}
-- file: ch08/Glob.hs
  | otherwise = do
    case splitFileName pat of
      ("", baseName) -> do
          curDir <- getCurrentDirectory
          listMatches curDir baseName
      (dirName, baseName) -> do
          dirs <- if isPattern dirName
                  then namesMatching (dropTrailingPathSeparator dirName)
                  else return [dirName]
          let listDir = if isPattern baseName
                        then listMatches
                        else listPlain
          pathNames <- forM dirs $ \dir -> do
                           baseNames <- listDir dir baseName
                           return (map (dir </>) baseNames)
          return (concat pathNames)
~~~~

We use `splitFileName`{.function} to split the string into a pair of
“everything but the final name” and “the final name”. If the first
element is empty, we're looking for a pattern in the current directory.
Otherwise, we must check the directory name and see if it contains
patterns. If it does not, we create a singleton list of the directory
name. If it contains a pattern, we list all of the matching directories.

![[Note]](/support/figs/note.png)

Things to watch out for

The `System.FilePath`{.classname} module can be a little tricky. Above
is a case in point; the `splitFileName`{.function} function leaves a
trailing slash on the end of the directory name that it returns.

~~~~ {#glob.ghci:split .screen}
ghci> :module +System.FilePath
ghci> splitFileName "foo/bar"
Loading package filepath-1.1.0.0 ... linking ... done.
("foo/","bar")
~~~~

If we didn't remember (or know enough) to remove that slash, we'd
recurse endlessly in `namesMatching`{.function}, because of the
following behaviour of `splitFileName`{.function}.

~~~~ {#glob.ghci:tricksy .screen}
ghci> splitFileName "foo/"
("foo/","")
~~~~

You can guess what happened to us that led us to add this note!

Finally, we collect all matches in every directory, giving us a list of
lists, and concatenate them into a single list of names.

The unfamiliar `forM`{.function} function above acts a little like a
“for” loop: it maps its second argument (an action) over its first (a
list), and returns the list of results.

We have a few loose ends to clean up. The first is the definition of the
`doesNameExist`{.function} function, used above. The
`System.Directory`{.classname} module doesn't let us check to see if a
name exists in the filesystem. It forces us to decide whether we want to
check for a file or a directory. This API is ungainly, so we roll the
two checks into a single function. In the name of performance, we make
the check for a file first, since files are far more common than
directories.

~~~~ {#Glob.hs:doesNameExist .programlisting}
-- file: ch08/Glob.hs
doesNameExist :: FilePath -> IO Bool

doesNameExist name = do
    fileExists <- doesFileExist name
    if fileExists
      then return True
      else doesDirectoryExist name
~~~~

We have two other functions to define, each of which returns a list of
names in a directory. The `listMatches`{.function} function returns a
list of all files matching the given glob pattern in a directory.

~~~~ {#Glob.hs:listMatches .programlisting}
-- file: ch08/Glob.hs
listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
    dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
    handle (const (return [])) $ do
        names <- getDirectoryContents dirName'
        let names' = if isHidden pat
                     then filter isHidden names
                     else filter (not . isHidden) names
        return (filter (`matchesGlob` pat) names')

isHidden ('.':_) = True
isHidden _       = False
~~~~

The `listPlain`{.function} function returns either an empty or singleton
list, depending on whether the single name it's passed exists.

~~~~ {#Glob.hs:listPlain .programlisting}
-- file: ch08/Glob.hs
listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
    exists <- if null baseName
              then doesDirectoryExist dirName
              else doesNameExist (dirName </> baseName)
    return (if exists then [baseName] else [])
~~~~

If we look closely at the definition of `listMatches`{.function} above,
we'll see a call to a function named `handle`{.function}. Earlier on, we
imported this from the `Control.Exception`{.classname} module; as that
import implies, this gives us our first taste of exception handling in
Haskell. Let's drop into **ghci** and see what we can find out.

~~~~ {#handle.ghci:type.handle .screen}
ghci> :module +Control.Exception
ghci> :type handle
handle :: (Exception -> IO a) -> IO a -> IO a
~~~~

This is telling us that `handle`{.function} takes two arguments. The
first is a function that is passed an exception value, and can have side
effects (see the IO type in its return value); this is the handler to
run if an exception is thrown. The second argument is the code that
might throw an exception.

As for the exception handler, the type of the `handle`{.function}
constrains it to return the same type of value as the body of code that
threw the exception. So its choices are to either throw an exception or,
as in our case, return a list of Strings.

The `const`{.function} function takes two arguments; it always returns
its first argument, no matter what its second argument is.

~~~~ {#handle.ghci:type.const .screen}
ghci> :type const
const :: a -> b -> a
ghci> :type return []
return [] :: (Monad m) => m [a]
ghci> :type handle (const (return []))
handle (const (return [])) :: IO [a] -> IO [a]
~~~~

We use `const`{.function} to write an exception handler that ignores the
exception it is passed. Instead, it causes our code to return an empty
list if we catch an exception.

We won't have anything more to say about exception handling here.
There's plenty more to cover, though, so we'll be returning to the
subject of exceptions in chapter [Chapter 19, *Error
handling*](error-handling.html "Chapter 19. Error handling").

### Exercises

**1.**

Although we've gone to some lengths to write a portable
`namesMatching`{.function} function, the function uses our case
sensitive `globToRegex`{.function} function. Find a way to modify
`namesMatching`{.function} to be case sensitive on Unix, and case
insensitive on Windows, without modifying its type signature.

*Hint*: consider reading the documentation for
`System.FilePath`{.classname} to look for a variable that tells us
whether we're running on a Unix-like system, or on Windows.

**2.**

If you're on a Unix-like system, look through the documentation for the
`System.Posix.Files`{.classname} module, and see if you can find a
replacement for the `doesNameExist`{.function} function.

**3.**

The `*`{.literal} wild card only matches names within a single
directory. Many shells have an extended wild card syntax,
`**`{.literal}, that matches names recursively in all directories. For
example, `**.c`{.literal} would mean “match a name ending in
`.c`{.literal} in this directory or any subdirectory at any depth”.
Implement matching on `**`{.literal} wildcards.

Handling errors through API design
----------------------------------

It's not necessarily a disaster if our `globToRegex`{.function} is
passed a malformed pattern. Perhaps a user mistyped a pattern, in which
case we'd like to be able to report a meaningful error message.

Calling the `error`{.function} function when this kind of problem occurs
can be a drastic response (exploring its consequences was the focus of
exercise [Q:
1](efficient-file-processing-regular-expressions-and-file-name-matching.html#ch07.q.error "Question")).
The `error`{.function} throws an exception. Pure Haskell code cannot
deal with exceptions, so control is going to rocket out of our pure code
into the nearest caller that lives in `IO`{.classname} and has an
appropriate exception handler installed. If no such handler is
installed, the Haskell runtime will default to terminating our program
(or print a nasty error message, in **ghci**).

So calling `error`{.function} is a little like pulling the handle of a
fighter plane's ejection seat. We're bailing out of a catastrophic
situation that we can't deal with gracefully, and there's likely to be a
lot of flaming wreckage strewn about by the time we hit the ground.

We've established that `error`{.function} is for disasters, but we're
still using it in `globToRegex`{.function}. In that case, malformed
input should be rejected, but not turned into a big deal. What would be
a better way to handle this?

Haskell's type system and libraries to the rescue! We can encode the
possibility of failure in the type signature of
`globToRegex`{.function}, using the predefined Either type.

~~~~ {#GlobRegexEither.hs:type .programlisting}
-- file: ch08/GlobRegexEither.hs
type GlobError = String

globToRegex :: String -> Either GlobError String
~~~~

A value returned by `globToRegex`{.function} will now be either
`Left "an error message"`{.literal} or
`Right "a valid regexp"`{.literal}. This return type forces our callers
to deal with the possibility of error. (You'll find that this use of the
Either type occurs frequently in Haskell code.)

### Exercises

**1.**

Write a version of `globToRegex`{.function} that uses the type signature
above.

**2.**

Modify the type signature of `namesMatching`{.function} so that it
encodes the possibility of a bad pattern, and make it use your rewritten
`globToRegex`{.function} function.

![[Tip]](/support/figs/tip.png)

Tip

You may find the amount of work involved to be surprisingly large. Don't
worry; we will introduce more concise and sophisticated ways of dealing
with errors in later chapters.

Putting our code to work
------------------------

The `namesMatching`{.function} function isn't very exciting by itself,
but it's a useful building block. Combine it with a few more functions,
and we can start to do interesting things.

Here's one such example. Let's define a `renameWith`{.function} function
that, instead of simply renaming a file, applies a function to the
file's name, and renames the file to whatever that function returns.

~~~~ {#Useful.hs:renameWith .programlisting}
-- file: ch08/Useful.hs
import System.FilePath (replaceExtension)
import System.Directory (doesFileExist, renameDirectory, renameFile)
import Glob (namesMatching)

renameWith :: (FilePath -> FilePath)
           -> FilePath
           -> IO FilePath

renameWith f path = do
    let path' = f path
    rename path path'
    return path'
~~~~

Once again, we work around the ungainly file/directory split in
`System.Directory`{.classname} with a helper function.

~~~~ {#Useful.hs:rename .programlisting}
-- file: ch08/Useful.hs
rename :: FilePath -> FilePath -> IO ()

rename old new = do
    isFile <- doesFileExist old
    let f = if isFile then renameFile else renameDirectory
    f old new
~~~~

The `System.FilePath`{.classname} module provides many useful functions
for manipulating file names. These functions mesh nicely with our
`renameWith`{.function} and `namesMatching`{.function} functions, so
that we can quickly use them to create functions with complex behaviour.
As an example, this terse function changes the file name suffixing
convention for C++ source files.

~~~~ {#Useful.hs:cc2cpp .programlisting}
-- file: ch08/Useful.hs
cc2cpp =
  mapM (renameWith (flip replaceExtension ".cpp")) =<< namesMatching "*.cc"
~~~~

The `cc2cpp`{.function} function uses a few functions we'll be seeing
over and over. The `flip`{.function} function takes another function as
argument, and swaps the order of its arguments (inspect the type of
`replaceExtension`{.function} in **ghci** to see why). The
`=<<`{.function} function feeds the result of the action on its right
side to the action on its left.

Exercises
---------

**1.**

Glob patterns are simple enough to interpret that it's easy to write a
matcher directly in Haskell, rather than going through the regexp
machinery. Give it a try.

\

* * * * *

^[[26](#id617121)]^If you are not acquainted with regular expressions,
we recommend Jeffrey Friedl's book *Mastering Regular Expressions*.

![image](/support/figs/rss.png) Want to stay up to date? Subscribe to
the comment feed for [this chapter](/feeds/comments/), or the [entire
book](/feeds/comments/).

Copyright 2007, 2008 Bryan O'Sullivan, Don Stewart, and John Goerzen.
This work is licensed under a [Creative Commons
Attribution-Noncommercial 3.0
License](http://creativecommons.org/licenses/by-nc/3.0/). Icons by [Paul
Davey](mailto:mattahan@gmail.com) aka
[Mattahan](http://mattahan.deviantart.com/).

  ----------------- -------------------- -------------------------------------------------------------------
  [Prev](io.html)                        [Next](io-case-study-a-library-for-searching-the-filesystem.html)
  Chapter 7. I/O    [Home](index.html)   Chapter 9. I/O case study: a library for searching the filesystem
  ----------------- -------------------- -------------------------------------------------------------------


