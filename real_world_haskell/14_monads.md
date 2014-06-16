[Real World Haskell](/)by Bryan O'Sullivan, Don Stewart, and John Goerzen
-------------------------------------------------------------------------

Chapter 14. Monads

[Prev](data-structures.html)

[Next](programming-with-monads.html)

Chapter 14. Monads
------------------

**Table of Contents**

[Introduction](monads.html#id638503)

[Revisiting earlier code examples](monads.html#id638567)

[Maybe chaining](monads.html#id638573)

[Implicit state](monads.html#id638674)

[Looking for shared patterns](monads.html#id638775)

[The Monad typeclass](monads.html#monads.class)

[And now, a jargon moment](monads.html#id639479)

[Using a new monad: show your work!](monads.html#monads.logger)

[Information hiding](monads.html#id639671)

[Controlled escape](monads.html#monads.escape)

[Leaving a trace](monads.html#id639855)

[Using the Logger monad](monads.html#id640003)

[Mixing pure and monadic code](monads.html#monads.liftM)

[Putting a few misconceptions to rest](monads.html#id640702)

[Building the Logger monad](monads.html#id640791)

[Sequential logging, not sequential evaluation](monads.html#id640993)

[The writer monad](monads.html#id641037)

[The Maybe monad](monads.html#id641078)

[Executing the Maybe monad](monads.html#id641174)

[Maybe at work, and good API design](monads.html#id641251)

[The list monad](monads.html#id641620)

[Understanding the list monad](monads.html#id642195)

[Putting the list monad to work](monads.html#id642436)

[Desugaring of do blocks](monads.html#monads.do)

[Monads as a programmable semicolon](monads.html#id642960)

[Why go sugar-free?](monads.html#monads.do.avoid)

[The state monad](monads.html#monads.state)

[Almost a state monad](monads.html#id643325)

[Reading and modifying the state](monads.html#id643585)

[Will the real state monad please stand up?](monads.html#id643643)

[Using the state monad: generating random
values](monads.html#monads.state.random)

[A first attempt at purity](monads.html#id644027)

[Random values in the state monad](monads.html#id644179)

[Exercises](monads.html#id644258)

[Running the state monad](monads.html#id644297)

[What about a bit more state?](monads.html#id644470)

[Monads and functors](monads.html#id644617)

[Another way of looking at monads](monads.html#monads.join)

[The monad laws, and good coding style](monads.html#id645043)

Introduction
------------

In [Chapter 7, *I/O*](io.html "Chapter 7. I/O"), we talked about the IO
monad, but we intentionally kept the discussion narrowly focused on how
to communicate with the outside world. We didn't discuss what a monad
*is*.

We've already seen in [Chapter 7, *I/O*](io.html "Chapter 7. I/O") that
the IO monad is easy to work with. Notational differences aside, writing
code in the IO monad isn't much different from coding in any other
imperative language.

When we had practical problems to solve in earlier chapters, we
introduced structures that, as we will soon see, are actually monads. We
aim to show you that a monad is often an *obvious* and *useful* tool to
help solve a problem. We'll define a few monads in this chapter, to show
how easy it is.

Revisiting earlier code examples
--------------------------------

### Maybe chaining

Let's take another look at the `parseP5`{.function} function that we
wrote in [Chapter 10, *Code case study: parsing a binary data
format*](code-case-study-parsing-a-binary-data-format.html "Chapter 10. Code case study: parsing a binary data format").

~~~~ {#id638598 .programlisting}
-- file: ch10/PNM.hs
matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString

-- "nat" here is short for "natural number"
getNat :: L.ByteString -> Maybe (Int, L.ByteString)

getBytes :: Int -> L.ByteString
         -> Maybe (L.ByteString, L.ByteString)

parseP5 s =
  case matchHeader (L8.pack "P5") s of
    Nothing -> Nothing
    Just s1 ->
      case getNat s1 of
        Nothing -> Nothing
        Just (width, s2) ->
          case getNat (L8.dropWhile isSpace s2) of
            Nothing -> Nothing
            Just (height, s3) ->
              case getNat (L8.dropWhile isSpace s3) of
                Nothing -> Nothing
                Just (maxGrey, s4)
                  | maxGrey > 255 -> Nothing
                  | otherwise ->
                      case getBytes 1 s4 of
                        Nothing -> Nothing
                        Just (_, s5) ->
                          case getBytes (width * height) s5 of
                            Nothing -> Nothing
                            Just (bitmap, s6) ->
                              Just (Greymap width height maxGrey bitmap, s6)
~~~~

When we introduced this function, it threatened to march off the right
side of the page if it got much more complicated. We brought the
staircasing under control using the `(>>?)`{.function} function.

~~~~ {#id638633 .programlisting}
-- file: ch10/PNM.hs
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v  >>? f = f v
~~~~

We carefully chose the type of `(>>?)`{.function} to let us chain
together functions that return a Maybe value. So long as the result type
of one function matches the parameter of the next, we can chain
functions returning Maybe together indefinitely. The body of
`(>>?)`{.function} hides the details of whether the chain of functions
we build is short-circuited somewhere, due to one returning
`Nothing`{.code}, or completely evaluated.

### Implicit state

Useful as `(>>?)`{.function} was for cleaning up the structure of
`parseP5`{.function}, we had to incrementally consume pieces of a string
as we parsed it. This forced us to pass the current value of the string
down our chain of Maybes, wrapped up in a tuple. Each function in the
chain put a result into one element of the tuple, and the unconsumed
remainder of the string into the other.

~~~~ {#id638704 .programlisting}
-- file: ch10/PNM.hs
parseP5_take2 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_take2 s =
    matchHeader (L8.pack "P5") s       >>?
    \s -> skipSpace ((), s)           >>?
    (getNat . snd)                    >>?
    skipSpace                         >>?
    \(width, s) ->   getNat s         >>?
    skipSpace                         >>?
    \(height, s) ->  getNat s         >>?
    \(maxGrey, s) -> getBytes 1 s     >>?
    (getBytes (width * height) . snd) >>?
    \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)
~~~~

Once again, we were faced with a pattern of repeated behaviour: consume
some string, return a result, and return the remaining string for the
next function to consume. However, this pattern was more insidious: if
we wanted to pass another piece of information down the chain, we'd have
to modify nearly every element of the chain, turning each two-tuple into
a three-tuple!

We addressed this by moving the responsibility for managing the current
piece of string out of the individual functions in the chain, and into
the function that we used to chain them together.

~~~~ {#id638736 .programlisting}
-- file: ch10/Parse.hs
(==>) :: Parse a -> (a -> Parse b) -> Parse b

firstParser ==> secondParser  =  Parse chainedParser
  where chainedParser initState   =
          case runParse firstParser initState of
            Left errMessage ->
                Left errMessage
            Right (firstResult, newState) ->
                runParse (secondParser firstResult) newState
~~~~

We also hid the details of the parsing state in the ParseState type.
Even the `getState`{.function} and `putState`{.function} functions don't
inspect the parsing state, so any modification to ParseState will have
no effect on any existing code.

Looking for shared patterns
---------------------------

When we look at the above examples in detail, they don't seem to have
much in common. Obviously, they're both concerned with chaining
functions together, and with hiding details to let us write tidier code.
However, let's take a step back and consider them in *less* detail.

First, let's look at the type definitions.

~~~~ {#id638802 .programlisting}
-- file: ch14/Maybe.hs
data Maybe a = Nothing
             | Just a
~~~~

~~~~ {#id638808 .programlisting}
-- file: ch10/Parse.hs
newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
    }
~~~~

The common feature of these two types is that each has a single type
parameter on the left of the definition, which appears somewhere on the
right. These are thus generic types, which know nothing about their
payloads.

Next, we'll examine the chaining functions that we wrote for the two
types.

~~~~ {#monadness.ghci:Maybe .screen}
ghci> :type (>>?)
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
~~~~

~~~~ {#monadness.ghci:Parse .screen}
ghci> :type (==>)
(==>) :: Parse a -> (a -> Parse b) -> Parse b
~~~~

These functions have strikingly similar types. If we were to turn those
type constructors into a type variable, we'd end up with a single more
abstract type.

~~~~ {#Maybe.hs:chain .programlisting}
-- file: ch14/Maybe.hs
chain :: m a -> (a -> m b) -> m b
~~~~

Finally, in each case we have a function that takes a “plain” value, and
“injects” it into the target type. For Maybe, this function is simply
the value constructor `Just`{.code}, but the injector for Parse is more
complicated.

~~~~ {#id638916 .programlisting}
-- file: ch10/Parse.hs
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))
~~~~

Again, it's not the details or complexity that we're interested in, it's
the fact that each of these types has an “injector” function, which
looks like this.

~~~~ {#Maybe.hs:inject .programlisting}
-- file: ch14/Maybe.hs
inject :: a -> m a
~~~~

It is *exactly* these three properties, and a few rules about how we can
use them together, that define a monad in Haskell. Let's revisit the
above list in condensed form.

-   A type constructor `m`{.varname}.

-   A function of type m a -\> (a -\> m b) -\> m b for chaining the
    output of one function into the input of another.

-   A function of type a -\> m a for injecting a normal value into the
    chain, i.e. it wraps a type a with the type constructor m.

The properties that make the Maybe type a monad are its type constructor
Maybe a, our chaining function `(>>?)`{.function}, and the injector
function `Just`{.function}.

For Parse, the corresponding properties are the type constructor Parse
a, the chaining function `(==>)`{.function}, and the injector function
`identity`{.function}.

We have intentionally said nothing about how the chaining and injection
functions of a monad should behave, and that's because this almost
doesn't matter. In fact, monads are ubiquitous in Haskell code precisely
because they are so simple. Many common programming patterns have a
monadic structure: passing around implicit data, or short-circuiting a
chain of evaluations if one fails, to choose but two.

The Monad typeclass
-------------------

We can capture the notions of chaining and injection, and the types that
we want them to have, in a Haskell typeclass. The standard Prelude
already defines just such a typeclass, named `Monad`{.code}.

~~~~ {#Maybe.hs:Monad .programlisting}
-- file: ch14/Maybe.hs
class Monad m where
    -- chain
    (>>=)  :: m a -> (a -> m b) -> m b
    -- inject
    return :: a -> m a
~~~~

Here, `(>>=)`{.function} is our chaining function. We've already been
introduced to it in [the section called
“Sequencing”](io.html#io.bind "Sequencing"). It's often referred to as
“bind”, as it binds the result of the computation on the left to the
parameter of the one on the right.

Our injection function is `return`{.literal}. As we noted in [the
section called “The True Nature of
Return”](io.html#io.return "The True Nature of Return"), the choice of
the name `return`{.literal} is a little unfortunate. That name is widely
used in imperative languages, where it has a fairly well understood
meaning. In Haskell, its behaviour is much less constrained. In
particular, calling `return`{.literal} in the middle of a chain of
functions won't cause the chain to exit early. A useful way to link its
behavior to its name is that it *returns* a pure value (of type a) into
a monad (of type m a).

While `(>>=)`{.function} and `return`{.literal} are the core functions
of the `Monad`{.code} typeclass, it also defines two other functions.
The first is `(>>)`{.function}. Like `(>>=)`{.function}, it performs
chaining, but it ignores the value on the left.

~~~~ {#Maybe.hs:bind_ .programlisting}
-- file: ch14/Maybe.hs
    (>>) :: m a -> m b -> m b
    a >> f = a >>= \_ -> f
~~~~

We use this function when we want to perform actions in a certain order,
but don't care what the result of one is. This might seem pointless: why
would we not care what a function's return value is? Recall, though,
that we defined a `(==>&)`{.function} combinator earlier to express
exactly this. Alternatively, consider a function like
`print`{.function}, which provides a placeholder result that we do not
need to inspect.

~~~~ {#bind.ghci:print .screen}
ghci> :type print "foo"
print "foo" :: IO ()
~~~~

If we use plain `(>>=)`{.function}, we have to provide as its right hand
side a function that ignores its argument.

~~~~ {#bind.ghci:bind .screen}
ghci> print "foo" >>= \_ -> print "bar"
"foo"
"bar"
~~~~

But if we use `(>>)`{.function}, we can omit the needless function.

~~~~ {#bind.ghci:bind_ .screen}
ghci> print "baz" >> print "quux"
"baz"
"quux"
~~~~

As we showed above, the default implementation of `(>>)`{.function} is
defined in terms of `(>>=)`{.function}.

The second non-core `Monad`{.code} function is `fail`{.function}, which
takes an error message and does something to make the chain of functions
fail.

~~~~ {#Maybe.hs:fail .programlisting}
-- file: ch14/Maybe.hs
    fail :: String -> m a
    fail = error
~~~~

![[Warning]](/support/figs/warning.png)

Beware of fail

Many `Monad`{.code} instances don't override the default implementation
of `fail`{.function} that we show here, so in those monads,
`fail`{.function} uses `error`{.function}. Calling `error`{.function} is
usually highly undesirable, since it throws an exception that callers
either cannot catch or will not expect.

Even if you know that right now you're executing in a monad that has
`fail`{.function} do something more sensible, we still recommend
avoiding it. It's far too easy to cause yourself a problem later when
you refactor your code and forget that a previously safe use of
`fail`{.function} might be dangerous in its new context.

To revisit the parser that we developed in [Chapter 10, *Code case
study: parsing a binary data
format*](code-case-study-parsing-a-binary-data-format.html "Chapter 10. Code case study: parsing a binary data format"),
here is its `Monad`{.code} instance.

~~~~ {#Parse.hs:Monad .programlisting}
-- file: ch10/Parse.hs
instance Monad Parse where
    return = identity
    (>>=) = (==>)
    fail = bail
~~~~

And now, a jargon moment
------------------------

There are a few terms of jargon around monads that you may not be
familiar with. These aren't formal terms, but they're in common use, so
it's helpful to know about them.

-   “Monadic” simply means “pertaining to monads”. A monadic *type* is
    an instance of the `Monad`{.code} typeclass; a monadic *value* has a
    monadic type.

-   When we say that a type “is a monad”, this is really a shorthand way
    of saying that it's an instance of the `Monad`{.code} typeclass.
    Being an instance of `Monad`{.code} gives us the necessary monadic
    triple of type constructor, injection function, and chaining
    function.

-   In the same way, a reference to “the Foo monad” implies that we're
    talking about the type named Foo, and that it's an instance of
    `Monad`{.code}.

-   An “action” is another name for a monadic value. This use of the
    word probably originated with the introduction of monads for I/O,
    where a monadic value like `print "foo"`{.code} can have an
    observable side effect. A function with a monadic return type might
    also be referred to as an action, though this is a little less
    common.

Using a new monad: show your work!
----------------------------------

In our introduction to monads, we showed how some pre-existing code was
already monadic in form. Now that we are beginning to grasp what a monad
is, and we've seen the `Monad`{.code} typeclass, let's build a monad
with foreknowledge of what we're doing. We'll start out by defining its
interface, then we'll put it to use. Once we have those out of the way,
we'll finally build it.

Pure Haskell code is wonderfully clean to write, but of course it can't
perform I/O. Sometimes, we'd like to have a record of decisions we made,
without writing log information to a file. Let's develop a small library
to help with this.

Recall the `globToRegex`{.function} function that we developed in [the
section called “Translating a glob pattern into a regular
expression”](efficient-file-processing-regular-expressions-and-file-name-matching.html#glob.translate "Translating a glob pattern into a regular expression").
We will modify it so that it keeps a record of each of the special
pattern sequences that it translates. We are revisiting familiar
territory for a reason: it lets us compare non-monadic and monadic
versions of the same code.

To start off, we'll wrap our result type with a `Logger`{.code} type
constructor.

~~~~ {#Logger.hs:globToRegex.type .programlisting}
-- file: ch14/Logger.hs
globToRegex :: String -> Logger String
~~~~

### Information hiding

We'll intentionally keep the internals of the Logger module abstract.

~~~~ {#Logger.hs:module .programlisting}
-- file: ch14/Logger.hs
module Logger
    (
      Logger
    , Log
    , runLogger
    , record
    ) where
~~~~

Hiding the details like this has two benefits: it grants us considerable
flexibility in how we implement our monad, and more importantly, it
gives users a simple interface.

Our Logger type is purely a *type* constructor. We don't export the
*value* constructor that a user would need to create a value of this
type. All they can use Logger for is writing type signatures.

The Log type is just a synonym for a list of strings, to make a few
signatures more readable. We use a list of strings to keep the
implementation simple.

~~~~ {#Logger.hs:Log .programlisting}
-- file: ch14/Logger.hs
type Log = [String]
~~~~

Instead of giving our users a value constructor, we provide them with a
function, `runLogger`{.function}, that evaluates a logged action. This
returns both the result of an action and whatever was logged while the
result was being computed.

~~~~ {#Logger.hs:runLogger.type .programlisting}
-- file: ch14/Logger.hs
runLogger :: Logger a -> (a, Log)
~~~~

### Controlled escape

The `Monad`{.code} typeclass doesn't provide any means for values to
escape their monadic shackles. We can inject a value into a monad using
`return`{.literal}. We can extract a value from a monad using
`(>>=)`{.function} but the function on the right, which can see an
unwrapped value, has to wrap its own result back up again.

Most monads have one or more `runLogger`{.function}-like functions. The
notable exception is of course IO, which we usually only escape from by
exiting a program.

A monad execution function runs the code inside the monad and unwraps
its result. Such functions are usually the only means provided for a
value to escape from its monadic wrapper. The author of a monad thus has
complete control over how whatever happens inside the monad gets out.

Some monads have several execution functions. In our case, we can
imagine a few alternatives to `runLogger`{.function}: one might only
return the log messages, while another might return just the result and
drop the log messages.

### Leaving a trace

When executing inside a Logger action, user code calls
`record`{.function} to record something.

~~~~ {#Logger.hs:record.type .programlisting}
-- file: ch14/Logger.hs
record :: String -> Logger ()
~~~~

Since recording occurs in the plumbing of our monad, our action's result
supplies no information.

Usually, a monad will provide one or more helper functions like our
`record`{.function}. These are our means for accessing the special
behaviors of that monad.

Our module also defines the `Monad`{.code} instance for the Logger type.
These definitions are all that a client module needs in order to be able
to use this monad.

Here is a preview, in **ghci**, of how our monad will behave.

~~~~ {#logger.ghci:simple .screen}
ghci> let simple = return True :: Logger Bool
ghci> runLogger simple
(True,[])
~~~~

When we run the logged action using `runLogger`{.function}, we get back
a pair. The first element is the result of our code; the second is the
list of items logged while the action executed. We haven't logged
anything, so the list is empty. Let's fix that.

~~~~ {#logger.ghci:logged .screen}
ghci> runLogger (record "hi mom!" >> return 3.1337)
(3.1337,["hi mom!"])
~~~~

### Using the Logger monad

Here's how we kick off our glob-to-regexp conversion inside the Logger
monad.

~~~~ {#Logger.hs:rooted .programlisting}
-- file: ch14/Logger.hs
globToRegex cs =
    globToRegex' cs >>= \ds ->
    return ('^':ds)
~~~~

There are a few coding style issues worth mentioning here. The body of
the function starts on the line after its name. By doing this, we gain
some horizontal white space. We've also “hung” the parameter of the
anonymous function at the end of the line. This is common practice in
monadic code.

Remember the type of `(>>=)`{.function}: it extracts the value on the
left from its Logger wrapper, and passes the unwrapped value to the
function on the right. The function on the right must, in turn, wrap
*its* result with the Logger wrapper. This is exactly what
`return`{.literal} does: it takes a pure value, and wraps it in the
monad's type constructor.

~~~~ {#logger.ghci:bind.type .screen}
ghci> :type (>>=)
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
ghci> :type (globToRegex "" >>=)
(globToRegex "" >>=) :: (String -> Logger b) -> Logger b
~~~~

Even when we write a function that does almost nothing, we must call
`return`{.function} to wrap the result with the correct type.

~~~~ {#Logger.hs:eof .programlisting}
-- file: ch14/Logger.hs
globToRegex' :: String -> Logger String
globToRegex' "" = return "$"
~~~~

When we call `record`{.function} to save a log entry, we use
`(>>)`{.function} instead of `(>>=)`{.function} to chain it with the
following action.

~~~~ {#Logger.hs:question .programlisting}
-- file: ch14/Logger.hs
globToRegex' ('?':cs) =
    record "any" >>
    globToRegex' cs >>= \ds ->
    return ('.':ds)
~~~~

Recall that this is a variant of `(>>=)`{.function} that ignores the
result on the left. We know that the result of `record`{.function} will
always be `()`{.code}, so there's no point in capturing it.

We can use `do`{.literal} notation, which we first encountered in [the
section called “Sequencing”](io.html#io.bind "Sequencing"), to somewhat
tidy up our code.

~~~~ {#Logger.hs:asterisk .programlisting}
-- file: ch14/Logger.hs
globToRegex' ('*':cs) = do
    record "kleene star"
    ds <- globToRegex' cs
    return (".*" ++ ds)
~~~~

The choice of `do`{.literal} notation versus explicit `(>>=)`{.function}
with anonymous functions is mostly a matter of taste, though almost
everyone's taste is to use `do`{.literal} notation for anything longer
than about two lines. There is one significant difference between the
two styles, though, which we'll return to in [the section called
“Desugaring of do
blocks”](monads.html#monads.do "Desugaring of do blocks").

Parsing a character class mostly follows the same pattern that we've
already seen.

~~~~ {#Logger.hs:class .programlisting}
-- file: ch14/Logger.hs
globToRegex' ('[':'!':c:cs) =
    record "character class, negative" >>
    charClass cs >>= \ds ->
    return ("[^" ++ c : ds)
globToRegex' ('[':c:cs) =
    record "character class" >>
    charClass cs >>= \ds ->
    return ("[" ++ c : ds)
globToRegex' ('[':_) =
    fail "unterminated character class"
~~~~

Mixing pure and monadic code
----------------------------

Based on the code we've seen so far, monads seem to have a substantial
shortcoming: the type constructor that wraps a monadic value makes it
tricky to use a normal, pure function on a value trapped inside a
monadic wrapper. Here's a simple illustration of the apparent problem.
Let's say we have a trivial piece of code that runs in the Logger monad
and returns a string.

~~~~ {#logger.ghci:m .screen}
ghci> let m = return "foo" :: Logger String
~~~~

If we want to find out the length of that string, we can't simply call
`length`{.function}: the string is wrapped, so the types don't match up.

~~~~ {#logger.ghci:m.length .screen}
ghci> length m

<interactive>:1:7:
    Couldn't match expected type `[a]'
           against inferred type `Logger String'
    In the first argument of `length', namely `m'
    In the expression: length m
    In the definition of `it': it = length m
~~~~

What we've done so far to work around this is something like the
following.

~~~~ {#logger.ghci:m.length2 .screen}
ghci> :type   m >>= \s -> return (length s)
m >>= \s -> return (length s) :: Logger Int
~~~~

We use `(>>=)`{.function} to unwrap the string, then write a small
anonymous function that calls `length`{.function} and rewraps the result
using `return`{.function}.

This need crops up often in Haskell code. We won't be surprised to learn
that a shorthand already exists: we use the *lifting* technique that we
introduced for functors in [the section called “Introducing
functors”](code-case-study-parsing-a-binary-data-format.html#binary.functor "Introducing functors").
Lifting a pure function into a functor usually involves unwrapping the
value inside the functor, calling the function on it, and rewrapping the
result with the same constructor.

We do exactly the same thing with a monad. Because the `Monad`{.code}
typeclass already provides the `(>>=)`{.function} and `return`{.literal}
functions that know how to unwrap and wrap a value, the
`liftM`{.function} function doesn't need to know any details of a
monad's implementation.

~~~~ {#Logger.hs:liftM .programlisting}
-- file: ch14/Logger.hs
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= \i ->
            return (f i)
~~~~

When we declare a type to be an instance of the `Functor`{.code}
typeclass, we have to write our own version of `fmap`{.function}
specially tailored to that type. By contrast, `liftM`{.function} doesn't
need to know anything of a monad's internals, because they're abstracted
by `(>>=)`{.function} and `return`{.literal}. We only need to write it
once, with the appropriate type constraint.

The `liftM`{.function} function is predefined for us in the standard
`Control.Monad`{.code} module.

To see how `liftM`{.function} can help readability, we'll compare two
otherwise identical pieces of code. First, the familiar kind that does
not use `liftM`{.function}.

~~~~ {#Logger.hs:charClass_wordy .programlisting}
-- file: ch14/Logger.hs
charClass_wordy (']':cs) =
    globToRegex' cs >>= \ds ->
    return (']':ds)
charClass_wordy (c:cs) =
    charClass_wordy cs >>= \ds ->
    return (c:ds)
~~~~

Now we can eliminate the `(>>=)`{.function} and anonymous function cruft
with `liftM`{.function}.

~~~~ {#Logger.hs:charClass .programlisting}
-- file: ch14/Logger.hs
charClass (']':cs) = (']':) `liftM` globToRegex' cs
charClass (c:cs) = (c:) `liftM` charClass cs
~~~~

As with `fmap`{.function}, we often use `liftM`{.function} in infix
form. An easy way to read such an expression is “apply the pure function
on the left to the result of the monadic action on the right”.

The `liftM`{.function} function is so useful that `Control.Monad`{.code}
defines several variants, which combine longer chains of actions. We can
see one in the last clause of our `globToRegex'`{.function} function.

~~~~ {#Logger.hs:last .programlisting}
-- file: ch14/Logger.hs
globToRegex' (c:cs) = liftM2 (++) (escape c) (globToRegex' cs)

escape :: Char -> Logger String
escape c
    | c `elem` regexChars = record "escape" >> return ['\\',c]
    | otherwise           = return [c]
  where regexChars = "\\+()^$.{}]|"
~~~~

The `liftM2`{.function} function that we use above is defined as
follows.

~~~~ {#Logger.hs:liftM2 .programlisting}
-- file: ch14/Logger.hs
liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m1 m2 =
    m1 >>= \a ->
    m2 >>= \b ->
    return (f a b)
~~~~

It executes the first action, then the second, then combines their
results using the pure function `f`{.varname}, and wraps that result. In
addition to `liftM2`{.function}, the variants in `Control.Monad`{.code}
go up to `liftM5`{.function}.

Putting a few misconceptions to rest
------------------------------------

We've now seen enough examples of monads in action to have some feel for
what's going on. Before we continue, there are a few oft-repeated myths
about monads that we're going to address. You're bound to encounter
these assertions “in the wild”, so you might as well be prepared with a
few good retorts.

-   *Monads can be hard to understand.* We've already shown that monads
    “fall out naturally” from several problems. We've found that the
    best key to understanding them is to explain several concrete
    examples, then talk about what they have in common.

-   *Monads are only useful for I/O and imperative coding.* While we use
    monads for I/O in Haskell, they're valuable for many other purposes
    besides. We've already used them for short-circuiting a chain of
    computations, hiding complicated state, and logging. Even so, we've
    barely scratched the surface.

-   *Monads are unique to Haskell.* Haskell is probably the language
    that makes the most explicit use of monads, but people write them in
    other languages, too, ranging from C++ to OCaml. They happen to be
    particularly tractable in Haskell, due to `do`{.literal} notation,
    the power and inference of the type system, and the language's
    syntax.

-   *Monads are for controlling the order of evaluation.*

Building the Logger monad
-------------------------

The definition of our Logger type is very simple.

~~~~ {#Logger.hs:Logger .programlisting}
-- file: ch14/Logger.hs
newtype Logger a = Logger { execLogger :: (a, Log) }
~~~~

It's a pair, where the first element is the result of an action, and the
second is a list of messages logged while that action was run.

We've wrapped the tuple in a `newtype`{.code} to make it a distinct
type. The `runLogger`{.function} function extracts the tuple from its
wrapper. The function that we're exporting to execute a logged action,
`runLogger`{.function}, is just a synonym for `execLogger`{.function}.

~~~~ {#Logger.hs:runLogger .programlisting}
-- file: ch14/Logger.hs
runLogger = execLogger
~~~~

Our `record`{.function} helper function creates a singleton list of the
message we pass it.

~~~~ {#Logger.hs:record .programlisting}
-- file: ch14/Logger.hs
record s = Logger ((), [s])
~~~~

The result of this action is `()`{.code}, so that's the value we put in
the result slot.

Let's begin our `Monad`{.code} instance with `return`{.literal}, which
is trivial: it logs nothing, and stores its input in the result slot of
the tuple.

~~~~ {#Logger.hs:return .programlisting}
-- file: ch14/Logger.hs
instance Monad Logger where
    return a = Logger (a, [])
~~~~

Slightly more interesting is `(>>=)`{.function}, which is the heart of
the monad. It combines an action and a monadic function to give a new
result and a new log.

~~~~ {#Logger.hs:bind .programlisting}
-- file: ch14/Logger.hs
    -- (>>=) :: Logger a -> (a -> Logger b) -> Logger b
    m >>= k = let (a, w) = execLogger m
                  n      = k a
                  (b, x) = execLogger n
              in Logger (b, w ++ x)
~~~~

Let's spell out explicitly what is going on. We use
`runLogger`{.function} to extract the result `a`{.varname} from the
action `m`{.varname}, and we pass it to the monadic function
`k`{.varname}. We extract the result `b`{.varname} from that in turn,
and put it into the result slot of the final action. We concatenate the
logs `w`{.varname} and `x`{.varname} to give the new log.

### Sequential logging, not sequential evaluation

Our definition of `(>>=)`{.function} ensures that messages logged on the
left will appear in the new log before those on the right. However, it
says nothing about when the values `a`{.varname} and `b`{.varname} are
evaluated: `(>>=)`{.function} is lazy.

Like most other aspects of a monad's behaviour, strictness is under the
control of the monad's implementor. It is not a constant shared by all
monads. Indeed, some monads come in multiple flavours, each with
different levels of strictness.

### The writer monad

Our Logger monad is a specialised version of the standard Writer monad,
which can be found in the Control.Monad.Writer module of the
`mtl`{.code} package. We will present a Writer example in [the section
called “Using
typeclasses”](programming-with-monads.html#monadcase.io.class "Using typeclasses").

The Maybe monad
---------------

The Maybe type is very nearly the simplest instance of `Monad`{.code}.
It represents a computation that might not produce a result.

~~~~ {#Maybe.hs:instance .programlisting}
-- file: ch14/Maybe.hs
instance Monad Maybe where
    Just x >>= k  =  k x
    Nothing >>= _ =  Nothing

    Just _ >> k   =  k
    Nothing >> _  =  Nothing

    return x      =  Just x

    fail _        =  Nothing
~~~~

When we chain together a number of computations over Maybe using
`(>>=)`{.function} or `(>>)`{.function}, if any of them returns
`Nothing`{.code}, then we don't evaluate any of the remaining
computations.

Note, though, that the chain is not completely short-circuited. Each
`(>>=)`{.function} or `(>>)`{.function} in the chain will still match a
`Nothing`{.code} on its left, and produce a `Nothing`{.function} on its
right, all the way to the end. It's easy to forget this point: when a
computation in the chain fails, the subsequent production, chaining, and
consumption of `Nothing`{.code} values is cheap at runtime, but it's not
free.

### Executing the Maybe monad

A function suitable for executing the Maybe monad is `maybe`{.function}.
(Remember that “executing” a monad involves evaluating it and returning
a result that's had the monad's type wrapper removed.)

~~~~ {#Maybe.hs:maybe .programlisting}
-- file: ch14/Maybe.hs
maybe :: b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing  = n
maybe _ f (Just x) = f x
~~~~

Its first parameter is the value to return if the result is
`Nothing`{.code}. The second is a function to apply to a result wrapped
in the `Just`{.code} constructor; the result of that application is then
returned.

Since the Maybe type is so simple, it's about as common to simply
pattern-match on a Maybe value as it is to call `maybe`{.function}. Each
one is more readable in different circumstances.

### Maybe at work, and good API design

Here's an example of Maybe in use as a monad. Given a customer's name,
we want to find the billing address of their mobile phone carrier.

~~~~ {#Carrier.hs:findCarrierBillingAddress .programlisting}
-- file: ch14/Carrier.hs
import qualified Data.Map as M

type PersonName = String
type PhoneNumber = String
type BillingAddress = String
data MobileCarrier = Honest_Bobs_Phone_Network
                   | Morrisas_Marvelous_Mobiles
                   | Petes_Plutocratic_Phones
                     deriving (Eq, Ord)

findCarrierBillingAddress :: PersonName
                          -> M.Map PersonName PhoneNumber
                          -> M.Map PhoneNumber MobileCarrier
                          -> M.Map MobileCarrier BillingAddress
                          -> Maybe BillingAddress
~~~~

Our first version is the dreaded ladder of code marching off the right
of the screen, with many boilerplate `case`{.literal} expressions.

~~~~ {#Carrier.hs:variation1 .programlisting}
-- file: ch14/Carrier.hs
variation1 person phoneMap carrierMap addressMap =
    case M.lookup person phoneMap of
      Nothing -> Nothing
      Just number ->
          case M.lookup number carrierMap of
            Nothing -> Nothing
            Just carrier -> M.lookup carrier addressMap
~~~~

The `Data.Map`{.code} module's `lookup`{.function} function has a
monadic return type.

~~~~ {#carrier.ghci:lookup .screen}
ghci> :module +Data.Map
ghci> :type Data.Map.lookup
Data.Map.lookup :: (Ord k, Monad m) => k -> Map k a -> m a
~~~~

In other words, if the given key is present in the map,
`lookup`{.function} injects it into the monad using `return`{.literal}.
Otherwise, it calls `fail`{.function}. This is an interesting piece of
API design, though one that we think was a poor choice.

-   On the positive side, the behaviours of success and failure are
    automatically customised to our needs, based on the monad we're
    calling `lookup`{.function} from. Better yet, `lookup`{.function}
    itself doesn't know or care what those behaviours are.

    The `case`{.literal} expressions above typecheck because we're
    comparing the result of `lookup`{.function} against values of type
    Maybe.

-   The hitch is, of course, that using `fail`{.function} in the wrong
    monad throws a bothersome exception. We have already warned against
    the use of `fail`{.function}, so we will not repeat ourselves here.

In practice, *everyone* uses Maybe as the result type for
`lookup`{.function}. The result type of such a conceptually simple
function provides generality where it is not needed: `lookup`{.function}
should have been written to return Maybe.

Let's set aside the API question, and deal with the ugliness of our
code. We can make more sensible use of Maybe's status as a monad.

~~~~ {#Carrier.hs:variation2 .programlisting}
-- file: ch14/Carrier.hs
variation2 person phoneMap carrierMap addressMap = do
  number <- M.lookup person phoneMap
  carrier <- M.lookup number carrierMap
  address <- M.lookup carrier addressMap
  return address
~~~~

If any of these lookups fails, the definitions of `(>>=)`{.function} and
`(>>)`{.function} mean that the result of the function as a whole will
be `Nothing`{.code}, just as it was for our first attempt that used
`case`{.literal} explicitly.

This version is much tidier, but the `return`{.literal} isn't necessary.
Stylistically, it makes the code look more regular, and perhaps more
familiar to the eyes of an imperative programmer, but behaviourally it's
redundant. Here's an equivalent piece of code.

~~~~ {#Carrier.hs:variation2a .programlisting}
-- file: ch14/Carrier.hs
variation2a person phoneMap carrierMap addressMap = do
  number <- M.lookup person phoneMap
  carrier <- M.lookup number carrierMap
  M.lookup carrier addressMap
~~~~

When we introduced maps, we mentioned in [the section called “Partial
application
awkwardness”](barcode-recognition.html#barcode.map.partial "Partial application awkwardness")
that the type signatures of functions in the `Data.Map`{.code} module
often make them awkward to partially apply. The `lookup`{.function}
function is a good example. If we `flip`{.function} its arguments, we
can write the function body as a one-liner.

~~~~ {#Carrier.hs:variation3 .programlisting}
-- file: ch14/Carrier.hs
variation3 person phoneMap carrierMap addressMap =
    lookup phoneMap person >>= lookup carrierMap >>= lookup addressMap
  where lookup = flip M.lookup
~~~~

The list monad
--------------

While the `Maybe`{.code} type can represent either no value or one,
there are many situations where we might want to return some number of
results that we do not know in advance. Obviously, a list is well suited
to this purpose. The type of a list suggests that we might be able to
use it as a monad, because its type constructor has one free variable.
And sure enough, we can use a list as a monad.

Rather than simply present the Prelude's `Monad`{.code} instance for the
list type, let's try to figure out what an instance *ought* to look
like. This is easy to do: we'll look at the types of `(>>=)`{.function}
and `return`{.literal}, and perform some substitutions, and see if we
can use a few familiar list functions.

The more obvious of the two functions is `return`{.literal}. We know
that it takes a type `a`{.varname}, and wraps it in a type constructor
`m`{.varname} to give the type `m a`{.varname}. We also know that the
type constructor here is []. Substituting this type constructor for the
type variable `m`{.varname} gives us the type [] a (yes, this really is
valid notation!), which we can rewrite in more familiar form as [a].

We now know that `return`{.literal} for lists should have the type
`a ->`{.code} [a]. There are only a few sensible possibilities for an
implementation of this function. It might return the empty list, a
singleton list, or an infinite list. The most appealing behaviour, based
on what we know so far about monads, is the singleton list: it doesn't
throw information away, nor does it repeat it infinitely.

~~~~ {#ListMonad.hs:returnSingleton .programlisting}
-- file: ch14/ListMonad.hs
returnSingleton :: a -> [a]
returnSingleton x = [x]
~~~~

If we perform the same substitution trick on the type of
`(>>=)`{.function} as we did with `return`{.literal}, we discover that
it should have the type `[a] ->`{.code} (a `->`{.literal} [b])
`->`{.literal} [b]. This seems close to the type of `map`{.function}.

~~~~ {#listmonad.ghci:map .screen}
ghci> :type (>>=)
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
ghci> :type map
map :: (a -> b) -> [a] -> [b]
~~~~

The ordering of the types in `map`{.function}'s arguments doesn't match,
but that's easy to fix.

~~~~ {#listmonad.ghci:flipMap .screen}
ghci> :type (>>=)
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
ghci> :type flip map
flip map :: [a] -> (a -> b) -> [b]
~~~~

We've still got a problem: the second argument of `flip     map`{.code}
has the type `a ->`{.code} b, whereas the second argument of
`(>>=)`{.function} for lists has the type `a       ->`{.code} [b]. What
do we do about this?

Let's do a little more substitution and see what happens with the types.
The function `flip map`{.code} can return any type `b`{.varname} as its
result. If we substitute `[b]`{.varname} for `b`{.varname} in both
places where it appears in `flip map`{.code}'s type signature, its type
signature reads as `a ->`{.code} (a `->`{.literal} [b]) `->`{.literal}
[[b]]. In other words, if we map a function that returns a list over a
list, we get a list of lists back.

~~~~ {#listmonad.ghci:flipMapApp .screen}
ghci> flip map [1,2,3] (\a -> [a,a+100])
[[1,101],[2,102],[3,103]]
~~~~

Interestingly, we haven't really changed how closely our type signatures
match. The type of `(>>=)`{.function} is `[a] ->`{.code} (a
`->`{.literal} [b]) `->`{.literal} [b], while that of
`flip     map`{.code} when the mapped function returns a list is
`[a] ->`{.code} (a `->`{.literal} [b]) `->`{.literal} [[b]]. There's
still a mismatch in one type term; we've just moved that term from the
middle of the type signature to the end. However, our juggling wasn't in
vain: we now need a function that takes a [[b]] and returns a [b], and
one readily suggests itself in the form of `concat`{.function}.

~~~~ {#listmonad.ghci:concat .screen}
ghci> :type concat
concat :: [[a]] -> [a]
~~~~

The types suggest that we should flip the arguments to `map`{.function},
then `concat`{.function} the results to give a single list.

~~~~ {#listmonad.ghci:bind .screen}
ghci> :type \xs f -> concat (map f xs)
\xs f -> concat (map f xs) :: [a] -> (a -> [a1]) -> [a1]
~~~~

This is exactly the definition of `(>>=)`{.function} for lists.

~~~~ {#ListMonad.hs:instance .programlisting}
-- file: ch14/ListMonad.hs
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
~~~~

It applies `f`{.varname} to every element in the list `xs`{.varname},
and concatenates the results to return a single list.

With our two core `Monad`{.code} definitions in hand, the
implementations of the non-core definitions that remain,
`(>>)`{.function} and `fail`{.function}, ought to be obvious.

~~~~ {#ListMonad.hs:rest .programlisting}
-- file: ch14/ListMonad.hs
    xs >> f = concat (map (\_ -> f) xs)
    fail _ = []
~~~~

### Understanding the list monad

The list monad is similar to a familiar Haskell tool, the list
comprehension. We can illustrate this similarity by computing the
Cartesian product of two lists. First, we'll write a list comprehension.

~~~~ {#CartesianProduct.hs:comprehensive .programlisting}
-- file: ch14/CartesianProduct.hs
comprehensive xs ys = [(x,y) | x <- xs, y <- ys]
~~~~

For once, we'll use bracketed notation for the monadic code instead of
layout notation. This will highlight how structurally similar the
monadic code is to the list comprehension.

~~~~ {#CartesianProduct.hs:monadic .programlisting}
-- file: ch14/CartesianProduct.hs
monadic xs ys = do { x <- xs; y <- ys; return (x,y) }
~~~~

The only real difference is that the value we're constructing comes at
the end of the sequence of expressions, instead of the beginning as in
the list comprehension. Also, the results of the two functions are
identical.

~~~~ {#cartesian.ghci:comparison .screen}
ghci> comprehensive [1,2] "bar"
[(1,'b'),(1,'a'),(1,'r'),(2,'b'),(2,'a'),(2,'r')]
ghci> comprehensive [1,2] "bar" == monadic [1,2] "bar"
True
~~~~

It's easy to be baffled by the list monad early on, so let's walk
through our monadic Cartesian product code again in more detail. This
time, we'll rearrange the function to use layout instead of brackets.

~~~~ {#CartesianProduct.hs:blockyDo .programlisting}
-- file: ch14/CartesianProduct.hs
blockyDo xs ys = do
    x <- xs
    y <- ys
    return (x, y)
~~~~

For every element in the list `xs`{.varname}, the rest of the function
is evaluated once, with `x`{.varname} bound to a different value from
the list each time. Then for every element in the list `ys`{.varname},
the remainder of the function is evaluated once, with `y`{.varname}
bound to a different value from the list each time.

What we really have here is a doubly nested loop! This highlights an
important fact about monads: you *cannot* predict how a block of monadic
code will behave unless you know what monad it will execute in.

We'll now walk through the code even more explicitly, but first let's
get rid of the `do`{.literal} notation, to make the underlying structure
clearer. We've indented the code a little unusually to make the loop
nesting more obvious.

~~~~ {#CartesianProduct.hs:blockyPlain .programlisting}
-- file: ch14/CartesianProduct.hs
blockyPlain xs ys =
    xs >>=
    \x -> ys >>=
    \y -> return (x, y)

blockyPlain_reloaded xs ys =
    concat (map (\x ->
                 concat (map (\y ->
                              return (x, y))
                         ys))
            xs)
~~~~

If `xs`{.varname} has the value `[1,2,3]`{.code}, the two lines that
follow are evaluated with `x`{.varname} bound to `1`{.code}, then to
`2`{.code}, and finally to `3`{.varname}. If `ys`{.varname} has the
value `[True,       False]`{.code}, the final line is evaluated *six*
times: once with `x`{.varname} as `1`{.code} and `y`{.varname} as
`True`{.code}; again with `x`{.varname} as `1`{.code} and `y`{.varname}
as `False`{.code}; and so on. The `return`{.literal} expression wraps
each tuple in a single-element list.

### Putting the list monad to work

Here is a simple brute force constraint solver. Given an integer, it
finds all pairs of positive integers that, when multiplied, give that
value (this is the constraint being solved).

~~~~ {#MultiplyTo.hs:multiplyTo .programlisting}
-- file: ch14/MultiplyTo.hs
guarded :: Bool -> [a] -> [a]
guarded True  xs = xs
guarded False _  = []

multiplyTo :: Int -> [(Int, Int)]
multiplyTo n = do
  x <- [1..n]
  y <- [x..n]
  guarded (x * y == n) $
    return (x, y)
~~~~

Let's try this in **ghci**.

~~~~ {#multiplyTo.ghci:multiplyTo .screen}
ghci> multiplyTo 8
[(1,8),(2,4)]
ghci> multiplyTo 100
[(1,100),(2,50),(4,25),(5,20),(10,10)]
ghci> multiplyTo 891
[(1,891),(3,297),(9,99),(11,81),(27,33)]
~~~~

Desugaring of do blocks
-----------------------

Haskell's `do`{.literal} syntax is an example of *syntactic sugar*: it
provides an alternative way of writing monadic code, without using
`(>>=)`{.function} and anonymous functions. *Desugaring* is the
translation of syntactic sugar back to the core language.

The rules for desugaring a `do`{.literal} block are easy to follow. We
can think of a compiler as applying these rules mechanically and
repeatedly to a `do`{.literal} block until no more `do`{.literal}
keywords remain.

A `do`{.literal} keyword followed by a single action is translated to
that action by itself.

~~~~ {#Do.hs:doNotation1 .programlisting}
-- file: ch14/Do.hs
doNotation1 =
    do act
~~~~

~~~~ {#Do.hs:translated1 .programlisting}
-- file: ch14/Do.hs
translated1 =
    act
~~~~

A `do`{.literal} keyword followed by more than one action is translated
to the first action, then `(>>)`{.function}, followed by a
`do`{.literal} keyword and the remaining actions. When we apply this
rule repeatedly, the entire `do`{.literal} block ends up chained
together by applications of `(>>)`{.function}.

~~~~ {#Do.hs:doNotation2 .programlisting}
-- file: ch14/Do.hs
doNotation2 =
    do act1
       act2
       {- ... etc. -}
       actN
~~~~

~~~~ {#Do.hs:translated2 .programlisting}
-- file: ch14/Do.hs
translated2 =
    act1 >>
    do act2
       {- ... etc. -}
       actN

finalTranslation2 =
    act1 >>
    act2 >>
    {- ... etc. -}
    actN
~~~~

The `<-`{.literal} notation has a translation that's worth paying close
attention to. On the left of the `<-`{.literal} is a normal Haskell
pattern. This can be a single variable or something more complicated. A
guard expression is not allowed.

~~~~ {#Do.hs:doNotation3 .programlisting}
-- file: ch14/Do.hs
doNotation3 =
    do pattern <- act1
       act2
       {- ... etc. -}
       actN
~~~~

~~~~ {#Do.hs:translated3 .programlisting}
-- file: ch14/Do.hs
translated3 =
    let f pattern = do act2
                       {- ... etc. -}
                       actN
        f _     = fail "..."
    in act1 >>= f
~~~~

This pattern is translated into a `let`{.literal} binding that declares
a local function with a unique name (we're just using `f`{.varname} as
an example above). The action on the right of the `<-`{.literal} is then
chained with this function using `(>>=)`{.function}.

What's noteworthy about this translation is that if the pattern match
fails, the local function calls the monad's `fail`{.function}
implementation. Here's an example using the Maybe monad.

~~~~ {#Do.hs:robust .programlisting}
-- file: ch14/Do.hs
robust :: [a] -> Maybe a
robust xs = do (_:x:_) <- Just xs
               return x
~~~~

The `fail`{.function} implementation in the Maybe monad simply returns
`Nothing`{.code}. If the pattern match in the above function fails, we
thus get `Nothing`{.code} as our result.

~~~~ {#do.ghci:robust .screen}
ghci> robust [1,2,3]
Just 2
ghci> robust [1]
Nothing
~~~~

Finally, when we write a `let`{.literal} expression in a `do`{.literal}
block, we can omit the usual `in`{.literal} keyword. Subsequent actions
in the block must be lined up with the `let`{.literal} keyword.

~~~~ {#Do.hs:doNotation4 .programlisting}
-- file: ch14/Do.hs
doNotation4 =
    do let val1 = expr1
           val2 = expr2
           {- ... etc. -}
           valN = exprN
       act1
       act2
       {- ... etc. -}
       actN
~~~~

~~~~ {#Do.hs:translated4 .programlisting}
-- file: ch14/Do.hs
translated4 =
    let val1 = expr1
        val2 = expr2
        valN = exprN
    in do act1
          act2
          {- ... etc. -}
          actN
~~~~

### Monads as a programmable semicolon

Back in [the section called “The offside rule is not
mandatory”](defining-types-streamlining-functions.html#deftypes.block "The offside rule is not mandatory"),
we mentioned that layout is the norm in Haskell, but it's not
*required*. We can write a `do`{.literal} block using explicit structure
instead of layout.

~~~~ {#Do.hs:semicolon .programlisting}
-- file: ch14/Do.hs
semicolon = do
  {
    act1;
    val1 <- act2;
    let { val2 = expr1 };
    actN;
  }
~~~~

~~~~ {#Do.hs:semicolonTranslated .programlisting}
-- file: ch14/Do.hs
semicolonTranslated =
    act1 >>
    let f val1 = let val2 = expr1
                 in actN
        f _ = fail "..."
    in act2 >>= f
~~~~

Even though this use of explicit structure is rare, the fact that it
uses semicolons to separate expressions has given rise to an apt slogan:
monads are a kind of “programmable semicolon”, because the behaviours of
`(>>)`{.function} and `(>>=)`{.function} are different in each monad.

### Why go sugar-free?

When we write `(>>=)`{.function} explicitly in our code, it reminds us
that we're stitching functions together using combinators, not simply
sequencing actions.

As long as you feel like a novice with monads, we think you should
prefer to explicitly write `(>>=)`{.function} over the syntactic sugar
of `do`{.literal} notation. The repeated reinforcement of what's really
happening seems, for many programmers, to help to keep things clear. (It
can be easy for an imperative programmer to relax a little too much from
exposure to the IO monad, and assume that a `do`{.literal} block means
nothing more than a simple sequence of actions.)

Once you're feeling more familiar with monads, you can choose whichever
style seems more appropriate for writing a particular function. Indeed,
when you read other people's monadic code, you'll see that it's unusual,
but by no means rare, to mix *both* `do`{.literal} notation and
`(>>=)`{.function} in a single function.

The `(=<<)`{.function} function shows up frequently whether or not we
use `do`{.literal} notiation. It is a flipped version of
`(>>=)`{.function}.

~~~~ {#cartesian.ghci:rbind .screen}
ghci> :type (>>=)
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
ghci> :type (=<<)
(=<<) :: (Monad m) => (a -> m b) -> m a -> m b
~~~~

It comes in handy if we want to compose monadic functions in the usual
Haskell right-to-left style.

~~~~ {#CartesianProduct.hs:wordCount .programlisting}
-- file: ch14/CartesianProduct.hs
wordCount = print . length . words =<< getContents
~~~~

The state monad
---------------

We discovered earlier in this chapter that the Parse from [Chapter 10,
*Code case study: parsing a binary data
format*](code-case-study-parsing-a-binary-data-format.html "Chapter 10. Code case study: parsing a binary data format")
was a monad. It has two logically distinct aspects. One is the idea of a
parse failing, and providing a message with the details: we represented
this using the Either type. The other involves carrying around a piece
of implicit state, in our case the partially consumed ByteString.

This need for a way to read and write state is common enough in Haskell
programs that the standard libraries provide a monad named State that is
dedicated to this purpose. This monad lives in the
`Control.Monad.State`{.code} module.

Where our Parse type carried around a ByteString as its piece of state,
the State monad can carry any type of state. We'll refer to the state's
unknown type as `s`{.varname}.

What's an obvious and general thing we might want to do with a state?
Given a state value, we inspect it, then produce a result and a new
state value. Let's say the result can be of any type `a`{.varname}. A
type signature that captures this idea is s -\> (a, s): take a state
`s`{.varname}, do something with it, and return a result `a`{.varname}
and possibly a new state `s`{.varname}.

### Almost a state monad

Let's develop some simple code that's *almost* the State monad, then
we'll take a look at the real thing. We'll start with our type
definition, which has exactly the obvious type we described above.

~~~~ {#SimpleState.hs:SimpleState .programlisting}
-- file: ch14/SimpleState.hs
type SimpleState s a = s -> (a, s)
~~~~

Our monad is a function that transforms one state into another, yielding
a result when it does so. Because of this, the state monad is sometimes
called the state transformer monad.

Yes, this is a type synonym, not a new type, and so we're cheating a
little. Bear with us for now; this simplifies the description that
follows.

Earlier in this chapter, we said that a monad has a type constructor
with a single type variable, and yet here we have a type with two
parameters. The key here is to understand that we can partially apply a
*type* just as we can partially apply a normal function. This is easiest
to follow with an example.

~~~~ {#SimpleState.hs:StringState .programlisting}
-- file: ch14/SimpleState.hs
type StringState a = SimpleState String a
~~~~

Here, we've bound the type variable `s`{.varname} to String. The type
StringState still has a type parameter `a`{.varname}, though. It's now
more obvious that we have a suitable type constructor for a monad. In
other words, our monad's type constructor is SimpleState s, not
SimpleState alone.

The next ingredient we need to make a monad is a definition for the
`return`{.literal} function.

~~~~ {#SimpleState.hs:returnSt .programlisting}
-- file: ch14/SimpleState.hs
returnSt :: a -> SimpleState s a
returnSt a = \s -> (a, s)
~~~~

All this does is take the result and the current state, and “tuple them
up”. You may by now be used to the idea that a Haskell function with
multiple parameters is just a chain of single-parameter functions, but
just in case you're not, here's a more familiar way of writing
`returnSt`{.function} that makes it more obvious how simple this
function is.

~~~~ {#SimpleState.hs:returnAlt .programlisting}
-- file: ch14/SimpleState.hs
returnAlt :: a -> SimpleState s a
returnAlt a s = (a, s)
~~~~

Our final piece of the monadic puzzle is a definition for
`(>>=)`{.function}. Here it is, using the actual variable names from the
standard library's definition of `(>>=)`{.function} for State.

~~~~ {#SimpleState.hs:bindSt .programlisting}
-- file: ch14/SimpleState.hs
bindSt :: (SimpleState s a) -> (a -> SimpleState s b) -> SimpleState s b
bindSt m k = \s -> let (a, s') = m s
                   in (k a) s'
~~~~

Those single-letter variable names aren't exactly a boon to readability,
so let's see if we can substitute some more meaningful names.

~~~~ {#SimpleState.hs:bindAlt .programlisting}
-- file: ch14/SimpleState.hs
-- m == step
-- k == makeStep
-- s == oldState

bindAlt step makeStep oldState =
    let (result, newState) = step oldState
    in  (makeStep result) newState
~~~~

To understand this definition, remember that `step`{.varname} is a
function with the type s -\> (a, s). When we evaluate this, we get a
tuple, and we have to use this to return a new function of type s -\>
(a, s). This is perhaps easier to follow if we get rid of the
SimpleState type synonyms from `bindAlt`{.function}'s type signature,
and examine the types of its parameters and result.

~~~~ {#SimpleState.hs:bindAlt.type .programlisting}
-- file: ch14/SimpleState.hs
bindAlt :: (s -> (a, s))        -- step
        -> (a -> s -> (b, s))   -- makeStep
        -> (s -> (b, s))        -- (makeStep result) newState
~~~~

### Reading and modifying the state

The definitions of `(>>=)`{.function} and `return`{.literal} for the
state monad simply act as plumbing: they move a piece of state around,
but they don't touch it in any way. We need a few other simple functions
to actually do useful work with the state.

~~~~ {#SimpleState.hs:getPut .programlisting}
-- file: ch14/SimpleState.hs
getSt :: SimpleState s s
getSt = \s -> (s, s)

putSt :: s -> SimpleState s ()
putSt s = \_ -> ((), s)
~~~~

The `getSt`{.function} function simply takes the current state and
returns it as the result, while `putSt`{.function} ignores the current
state and replaces it with a new state.

### Will the real state monad please stand up?

The only simplifying trick we played in the previous section was to use
a type synonym instead of a type definition for SimpleState. If we had
introduced a `newtype`{.code} wrapper at the same time, the extra
wrapping and unwrapping would have made our code harder to follow.

In order to define a `Monad`{.code} instance, we have to provide a
proper type constructor as well as definitions for `(>>=)`{.function}
and `return`{.literal}. This leads us to the *real* definition of State.

~~~~ {#State.hs:State .programlisting}
-- file: ch14/State.hs
newtype State s a = State {
      runState :: s -> (a, s)
    }
~~~~

All we've done is wrap our s -\> (a, s) type in a `State`{.code}
constructor. By using Haskell's record syntax to define the type, we're
automatically given a `runState`{.function} function that will unwrap a
State value from its constructor. The type of `runState`{.function} is
State s a -\> s -\> (a, s).

The definition of `return`{.literal} is almost the same as for
SimpleState, except we wrap our function with a `State`{.code}
constructor.

~~~~ {#State.hs:returnState .programlisting}
-- file: ch14/State.hs
returnState :: a -> State s a
returnState a = State $ \s -> (a, s)
~~~~

The definition of `(>>=)`{.function} is a little more complicated,
because it has to use `runState`{.function} to remove the `State`{.code}
wrappers.

~~~~ {#State.hs:bindState .programlisting}
-- file: ch14/State.hs
bindState :: State s a -> (a -> State s b) -> State s b
bindState m k = State $ \s -> let (a, s') = runState m s
                              in runState (k a) s'
~~~~

This function differs from our earlier `bindSt`{.function} only in
adding the wrapping and unwrapping of a few values. By separating the
“real work” from the bookkeeping, we've hopefully made it clearer what's
really happening.

We modify the functions for reading and modifying the state in the same
way, by adding a little wrapping.

~~~~ {#State.hs:getPut .programlisting}
-- file: ch14/State.hs
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)
~~~~

### Using the state monad: generating random values

We've already used Parse, our precursor to the state monad, to parse
binary data. In that case, we wired the type of the state we were
manipulating directly into the Parse type.

The State monad, by contrast, accepts any type of state as a parameter.
We supply the type of the state, to give e.g. State ByteString.

The State monad will probably feel more familiar to you than many other
monads if you have a background in imperative languages. After all,
imperative languages are all about carrying around some implicit state,
reading some parts, and modifying others through assignment, and this is
just what the state monad is for.

So instead of unnecessarily cheerleading for the idea of using the state
monad, we'll begin by demonstrating how to use it for something simple:
pseudorandom value generation. In an imperative language, there's
usually an easily available source of uniformly distributed pseudorandom
numbers. For example, in C, there's a standard `rand`{.function}
function that generates a pseudorandom number, using a global state that
it updates.

Haskell's standard random value generation module is named
`System.Random`{.code}. It allows the generation of random values of any
type, not just numbers. The module contains several handy functions that
live in the IO monad. For example, a rough equivalent of C's
`rand`{.function} function would be the following:

~~~~ {#Random.hs:rand .programlisting}
-- file: ch14/Random.hs
import System.Random

rand :: IO Int
rand = getStdRandom (randomR (0, maxBound))
~~~~

(The `randomR`{.function} function takes an inclusive range within which
the generated random value should lie.)

The `System.Random`{.code} module provides a typeclass,
`RandomGen`{.code}, that lets us define new sources of random Int
values. The type StdGen is the standard `RandomGen`{.code} instance. It
generates pseudorandom values. If we had an external source of truly
random data, we could make it an instance of `RandomGen`{.code} and get
truly random, instead of merely pseudorandom, values.

Another typeclass, `Random`{.code}, indicates how to generate random
values of a particular type. The module defines `Random`{.code}
instances for all of the usual simple types.

Incidentally, the definition of `rand`{.function} above reads and
modifies a built-in global random generator that inhabits the IO monad.

### A first attempt at purity

After all of our emphasis so far on avoiding the IO monad wherever
possible, it would be a shame if we were dragged back into it just to
generate some random values. Indeed, `System.Random`{.code} contains
pure random number generation functions.

The traditional downside of purity is that we have to get or create a
random number generator, then ship it from the point we created it to
the place where it's needed. When we finally call it, it returns a *new*
random number generator: we're in pure code, remember, so we can't
modify the state of the existing generator.

If we forget about immutability and reuse the same generator within a
function, we get back exactly the same “random” number every time.

~~~~ {#Random.hs:twoBadRandoms .programlisting}
-- file: ch14/Random.hs
twoBadRandoms :: RandomGen g => g -> (Int, Int)
twoBadRandoms gen = (fst $ random gen, fst $ random gen)
~~~~

Needless to say, this has unpleasant consequences.

~~~~ {#random.ghci:twoBadRandoms .screen}
ghci> twoBadRandoms `fmap` getStdGen
Loading package old-locale-1.0.0.0 ... linking ... done.
Loading package old-time-1.0.0.0 ... linking ... done.
Loading package random-1.0.0.0 ... linking ... done.
Loading package mtl-1.1.0.0 ... linking ... done.
(945769311181683171,945769311181683171)
~~~~

The `random`{.function} function uses an implicit range instead of the
user-supplied range used by `randomR`{.function}. The
`getStdGen`{.function} function retrieves the current value of the
global standard number generator from the IO monad.

Unfortunately, correctly passing around and using successive versions of
the generator does not make for palatable reading. Here's a simple
example.

~~~~ {#Random.hs:twoGoodRandoms .programlisting}
-- file: ch14/Random.hs
twoGoodRandoms :: RandomGen g => g -> ((Int, Int), g)
twoGoodRandoms gen = let (a, gen') = random gen
                         (b, gen'') = random gen'
                     in ((a, b), gen'')
~~~~

Now that we know about the state monad, though, it looks like a fine
candidate to hide the generator. The state monad lets us manage our
mutable state tidily, while guaranteeing that our code will be free of
other unexpected side effects, such as modifying files or making network
connections. This makes it easier to reason about the behavior of our
code.

### Random values in the state monad

Here's a state monad that carries around a StdGen as its piece of state.

~~~~ {#Random.hs:RandomState .programlisting}
-- file: ch14/Random.hs
type RandomState a = State StdGen a
~~~~

The type synonym is of course not necessary, but it's handy. It saves a
little keyboarding, and if we wanted to swap another random generator
for StdGen, it would reduce the number of type signatures we'd need to
change.

Generating a random value is now a matter of fetching the current
generator, using it, then modifying the state to replace it with the new
generator.

~~~~ {#Random.hs:getRandom .programlisting}
-- file: ch14/Random.hs
getRandom :: Random a => RandomState a
getRandom =
  get >>= \gen ->
  let (val, gen') = random gen in
  put gen' >>
  return val
~~~~

We can now use some of the monadic machinery that we saw earlier to
write a much more concise function for giving us a pair of random
numbers.

~~~~ {#Random.hs:getTwoRandoms .programlisting}
-- file: ch14/Random.hs
getTwoRandoms :: Random a => RandomState (a, a)
getTwoRandoms = liftM2 (,) getRandom getRandom
~~~~

#### Exercises

**1.**

Rewrite `getRandom`{.function} to use `do`{.literal} notation.

### Running the state monad

As we've already mentioned, each monad has its own specialised
evaluation functions. In the case of the state monad, we have several to
choose from.

-   `runState`{.function} returns both the result and the final state.

-   `evalState`{.function} returns only the result, throwing away the
    final state.

-   `execState`{.function} throws the result away, returning only the
    final state.

The `evalState`{.function} and `execState`{.function} functions are
simply compositions of `fst`{.function} and `snd`{.function} with
`runState`{.function}, respectively. Thus, of the three,
`runState`{.function} is the one most worth remembering.

Here's a complete example of how to implement our
`getTwoRandoms`{.function} function.

~~~~ {#Random.hs:runTwoRandoms .programlisting}
-- file: ch14/Random.hs
runTwoRandoms :: IO (Int, Int)
runTwoRandoms = do
  oldState <- getStdGen
  let (result, newState) = runState getTwoRandoms oldState
  setStdGen newState
  return result
~~~~

The call to `runState`{.function} follows a standard pattern: we pass it
a function in the state monad and an initial state. It returns the
result of the function and the final state.

The code surrounding the call to `runState`{.function} merely obtains
the current global StdGen value, then replaces it afterwards so that
subsequent calls to `runTwoRandoms`{.function} or other random
generation functions will pick up the updated state.

### What about a bit more state?

It's a little hard to imagine writing much interesting code in which
there's only a single state value to pass around. When we want to track
multiple pieces of state at once, the usual trick is to maintain them in
a data type. Here's an example: keeping track of the number of random
numbers we are handing out.

~~~~ {#Random.hs:CountedRandom .programlisting}
-- file: ch14/Random.hs
data CountedRandom = CountedRandom {
      crGen :: StdGen
    , crCount :: Int
    }

type CRState = State CountedRandom

getCountedRandom :: Random a => CRState a
getCountedRandom = do
  st <- get
  let (val, gen') = random (crGen st)
  put CountedRandom { crGen = gen', crCount = crCount st + 1 }
  return val
~~~~

This example happens to consume both elements of the state, and
construct a completely new state, every time we call into it. More
frequently, we're likely to read or modify only part of a state. This
function gets the number of random values generated so far.

~~~~ {#Random.hs:getCount .programlisting}
-- file: ch14/Random.hs
getCount :: CRState Int
getCount = crCount `liftM` get
~~~~

This example illustrates why we used record syntax to define our
CountedRandom state. It gives us accessor functions that we can glue
together with `get`{.function} to read specific pieces of the state.

If we want to partially update a state, the code doesn't come out quite
so appealingly.

~~~~ {#Random.hs:putCount .programlisting}
-- file: ch14/Random.hs
putCount :: Int -> CRState ()
putCount a = do
  st <- get
  put st { crCount = a }
~~~~

Here, instead of a function, we're using record update syntax. The
expression `st { crCount = a }`{.code} creates a new value that's an
identical copy of `st`{.varname}, except in its `crCount`{.code} field,
which is given the value `a`{.varname}. Because this is a syntactic
hack, we don't get the same kind of flexibility as with a function.
Record syntax may not exhibit Haskell's usual elegance, but it at least
gets the job done.

There exists a function named `modify`{.function} that combines the
`get`{.function} and `put`{.function} steps. It takes as argument a
state transformation function, but it's hardly more satisfactory: we
still can't escape from the clumsiness of record update syntax.

~~~~ {#Random.hs:putCountModify .programlisting}
-- file: ch14/Random.hs
putCountModify :: Int -> CRState ()
putCountModify a = modify $ \st -> st { crCount = a }
~~~~

Monads and functors
-------------------

Functors and monads are closely related. The terms are borrowed from a
branch of mathematics called category theory, but they did not make the
transition completely unscathed.

In category theory, a monad is built from a functor. You might expect
that in Haskell, the `Monad`{.code} typeclass would thus be a subclass
of `Functor`{.code}, but it isn't defined as such in the standard
Prelude. This is an unfortunate oversight.

However, authors of Haskell libraries use a workaround: when someone
defines an instance of `Monad`{.code} for a type, they almost always
write a `Functor`{.code} instance for it, too. You can expect that
you'll be able to use the `Functor`{.code} typeclass's `fmap`{.function}
function with any monad.

If we compare the type signature of `fmap`{.function} with those of some
of the standard monad functions that we've already seen, we get a hint
as to what `fmap`{.function} on a monad does.

~~~~ {#monadness.ghci:fmap .screen}
ghci> :type fmap
fmap :: (Functor f) => (a -> b) -> f a -> f b
ghci> :module +Control.Monad
ghci> :type liftM
liftM :: (Monad m) => (a1 -> r) -> m a1 -> m r
~~~~

Sure enough, `fmap`{.function} lifts a pure function into the monad,
just as `liftM`{.function} does.

### Another way of looking at monads

Now that we know about the relationship between functors and monads, If
we look back at the list monad, we can see something interesting.
Specifically, take a look at the definition of `(>>=)`{.function} for
lists.

~~~~ {#id644778 .programlisting}
-- file: ch14/ListMonad.hs
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
~~~~

Recall that `f`{.varname} has type a -\> [a]. When we call
`map f xs`{.code}, we get back a value of type [[a]], which we have to
“flatten” using `concat`{.code}.

Consider what we could do if `Monad`{.code} was a subclass of
`Functor`{.function}. Since `fmap`{.function} for lists is defined to be
`map`{.function}, we could replace `map`{.function} with
`fmap`{.function} in the definition of `(>>=)`{.function}. This is not
very interesting by itself, but suppose we could go further.

The `concat`{.function} function is of type [[a]] -\> [a]: as we
mentioned, it flattens the nesting of lists. We could generalise this
type signature from lists to monads, giving us the “remove a level of
nesting” type m (m a) -\> m a. The function that has this type is
conventionally named `join`{.function}.

If we had definitions of `join`{.function} and `fmap`{.function}, we
wouldn't need to write a definition of `(>>=)`{.function} for every
monad, because it would be completely generic. Here's what an
alternative definition of the `Monad`{.code} typeclass might look like,
along with a definition of `(>>=)`{.function}.

~~~~ {#AltMonad.hs:AltMonad .programlisting}
-- file: ch14/AltMonad.hs
import Prelude hiding ((>>=), return)

class Functor m => AltMonad m where
    join :: m (m a) -> m a
    return :: a -> m a

(>>=) :: AltMonad m => m a -> (a -> m b) -> m b
xs >>= f = join (fmap f xs)
~~~~

Neither definition of a monad is “better”, since if we have
`join`{.function} we can write `(>>=)`{.function}, and vice versa, but
the different perspectives can be refreshing.

Removing a layer of monadic wrapping can, in fact, be useful in
realistic circumstances. We can find a generic definition of
`join`{.function} in the `Control.Monad`{.code} module.

~~~~ {#MonadJoin.hs:join .programlisting}
-- file: ch14/MonadJoin.hs
join :: Monad m => m (m a) -> m a
join x = x >>= id
~~~~

Here are some examples of what it does.

~~~~ {#monadjoin.ghci:examples .screen}
ghci> join (Just (Just 1))
Just 1
ghci> join Nothing
Nothing
ghci> join [[1],[2,3]]
[1,2,3]
~~~~

The monad laws, and good coding style
-------------------------------------

In [the section called “Thinking more about
functors”](code-case-study-parsing-a-binary-data-format.html#binary.functor.laws "Thinking more about functors"),
we introduced two rules for how functors should always behave.

~~~~ {#MonadLaws.hs:functor .programlisting}
-- file: ch14/MonadLaws.hs
fmap id        ==   id 
fmap (f . g)   ==   fmap f . fmap g
~~~~

There are also rules for how monads ought to behave. The three laws
below are referred to as the monad laws. A Haskell implementation
doesn't enforce these laws: it's up to the author of a `Monad`{.code}
instance to follow them.

The monad laws are simply formal ways of saying “a monad shouldn't
surprise me”. In principle, we could probably get away with skipping
over them entirely. It would be a shame if we did, however, because the
laws contain gems of wisdom that we might otherwise overlook.

![[Tip]](/support/figs/tip.png)

Reading the laws

You can read each law below as “the expression on the left of the
`==`{.code} is equivalent to that on the right.”

The first law states that `return`{.literal} is a *left identity* for
`(>>=)`{.function}.

~~~~ {#MonadLaws.hs:leftIdentity .programlisting}
-- file: ch14/MonadLaws.hs
return x >>= f            ===   f x
~~~~

Another way to phrase this is that there's no reason to use
`return`{.literal} to wrap up a pure value if all you're going to do is
unwrap it again with `(>>=)`{.function}. It's actually a common style
error among programmers new to monads to wrap a value with
`return`{.literal}, then unwrap it with `(>>=)`{.function} a few lines
later in the same function. Here's the same law written with
`do`{.literal} notation.

~~~~ {#MonadLaws.hs:leftIdentityDo .programlisting}
-- file: ch14/MonadLaws.hs
do y <- return x
   f y                    ===   f x
~~~~

This law has practical consequences for our coding style: we don't want
to write unnecessary code, and the law lets us assume that the terse
code will be identical in its effect to the more verbose version.

The second monad law states that `return`{.literal} is a *right
identity* for `(>>=)`{.function}.

~~~~ {#MonadLaws.hs:rightIdentity .programlisting}
-- file: ch14/MonadLaws.hs
m >>= return              ===   m
~~~~

This law also has style consequences in real programs, particularly if
you're coming from an imperative language: there's no need to use
`return`{.literal} if the last action in a block would otherwise be
returning the correct result. Let's look at this law in `do`{.literal}
notation.

~~~~ {#MonadLaws.hs:rightIdentityDo .programlisting}
-- file: ch14/MonadLaws.hs
do y <- m
   return y               ===   m
~~~~

Once again, if we assume that a monad obeys this law, we can write the
shorter code in the knowledge that it will have the same effect as the
longer code.

The final law is concerned with associativity.

~~~~ {#MonadLaws.hs:associativity .programlisting}
-- file: ch14/MonadLaws.hs
m >>= (\x -> f x >>= g)   ===   (m >>= f) >>= g
~~~~

This law can be a little more difficult to follow, so let's look at the
contents of the parentheses on each side of the equation. We can rewrite
the expression on the left as follows.

~~~~ {#MonadLaws.hs:associativityLeft .programlisting}
-- file: ch14/MonadLaws.hs
m >>= s
  where s x = f x >>= g
~~~~

On the right, we can also rearrange things.

~~~~ {#MonadLaws.hs:associativityRight .programlisting}
-- file: ch14/MonadLaws.hs
t >>= g
  where t = m >>= f
~~~~

We're now claiming that the following two expressions are equivalent.

~~~~ {#MonadLaws.hs:associativityRewrite .programlisting}
-- file: ch14/MonadLaws.hs
m >>= s                   ===   t >>= g
~~~~

What this means is if we want to break up an action into smaller pieces,
it doesn't matter which sub-actions we hoist out to make new actions
with, provided we preserve their ordering. If we have three actions
chained together, we can substitute the first two and leave the third in
place, or we can replace the second two and leave the first in place.

Even this more complicated law has a practical consequence. In the
terminology of software refactoring, the “extract method” technique is a
fancy term for snipping out a piece of inline code, turning it into a
function, and calling the function from the site of the snipped code.
This law essentially states that this technique can be applied to
monadic Haskell code.

We've now seen how each of the monad laws offers us an insight into
writing better monadic code. The first two laws show us how to avoid
unnecessary use of `return`{.literal}. The third suggests that we can
safely refactor a complicated action into several simpler ones. We can
now safely let the details fade, in the knowledge that our “do what I
mean” intuitions won't be violated when we use properly written monads.

Incidentally, a Haskell compiler cannot guarantee that a monad actually
follows the monad laws. It is the responsibility of a monad's author to
satisfy—or, preferably, prove to—themselves that their code follows the
laws.

![image](/support/figs/rss.png) Want to stay up to date? Subscribe to
the comment feed for [this chapter](/feeds/comments/), or the [entire
book](/feeds/comments/).

Copyright 2007, 2008 Bryan O'Sullivan, Don Stewart, and John Goerzen.
This work is licensed under a [Creative Commons
Attribution-Noncommercial 3.0
License](http://creativecommons.org/licenses/by-nc/3.0/). Icons by [Paul
Davey](mailto:mattahan@gmail.com) aka
[Mattahan](http://mattahan.deviantart.com/).

  ------------------------------ -------------------- --------------------------------------
  [Prev](data-structures.html)                        [Next](programming-with-monads.html)
  Chapter 13. Data Structures    [Home](index.html)   Chapter 15. Programming with monads
  ------------------------------ -------------------- --------------------------------------


