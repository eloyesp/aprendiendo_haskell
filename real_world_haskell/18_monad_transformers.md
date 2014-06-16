[Real World Haskell](/)by Bryan O'Sullivan, Don Stewart, and John Goerzen
-------------------------------------------------------------------------

Chapter 18. Monad transformers

[Prev](interfacing-with-c-the-ffi.html)

[Next](error-handling.html)

Chapter 18. Monad transformers
------------------------------

**Table of Contents**

[Motivation: boilerplate avoidance](monad-transformers.html#id656888)

[A simple monad transformer example](monad-transformers.html#id657034)

[Common patterns in monads and monad
transformers](monad-transformers.html#id657356)

[Stacking multiple monad transformers](monad-transformers.html#id657557)

[Hiding our work](monad-transformers.html#id657944)

[Exercises](monad-transformers.html#id658064)

[Moving down the stack](monad-transformers.html#id658162)

[When explicit lifting is necessary](monad-transformers.html#id658511)

[Understanding monad transformers by building
one](monad-transformers.html#monadtrans.maybet)

[Creating a monad transformer](monad-transformers.html#id658976)

[More typeclass instances](monad-transformers.html#id659032)

[Replacing the Parse type with a monad
stack](monad-transformers.html#id659118)

[Exercises](monad-transformers.html#id659161)

[Transformer stacking order is
important](monad-transformers.html#id659259)

[Putting monads and monad transformers into
perspective](monad-transformers.html#id659474)

[Interference with pure code](monad-transformers.html#id659488)

[Overdetermined ordering](monad-transformers.html#id659632)

[Runtime overhead](monad-transformers.html#id659676)

[Unwieldy interfaces](monad-transformers.html#id659760)

[Pulling it all together](monad-transformers.html#id659884)

Motivation: boilerplate avoidance
---------------------------------

Monads provide a powerful way to build computations with effects. Each
of the standard monads is specialised to do exactly one thing. In real
code, we often need to be able to use several effects at once.

Recall the Parse type that we developed in [Chapter 10, *Code case
study: parsing a binary data
format*](code-case-study-parsing-a-binary-data-format.html "Chapter 10. Code case study: parsing a binary data format"),
for instance. When we introduced monads, we mentioned that this type was
a state monad in disguise. Our monad is more complex than the standard
State monad, because it uses the Either type to allow the possibility of
a parsing failure. In our case, if a parse fails early on, we want to
stop parsing, not continue in some broken state. Our monad combines the
effect of carrying state around with the effect of early exit.

The normal State monad doesn't let us escape in this way; it only
carries state. It uses the default implementation of `fail`{.function}:
this calls `error`{.function}, which throws an exception that we can't
catch in pure code. The State monad thus *appears* to allow for failure,
without that capability actually being any use. (Once again, we
recommend that you almost always avoid using `fail`{.function}!)

It would be ideal if we could somehow take the standard State monad and
add failure handling to it, without resorting to the wholesale
construction of custom monads by hand. The standard monads in the
`mtl`{.code} library don't allow us to combine them. Instead, the
library provides a set of *monad transformers*^[[37](#ftn.id656986)]^ to
achieve the same result.

A monad transformer is similar to a regular monad, but it's not a
standalone entity: instead, it modifies the behaviour of an underlying
monad. Most of the monads in the `mtl`{.code} library have transformer
equivalents. By convention, the transformer version of a monad has the
same name, with a `T`{.code} stuck on the end. For example, the
transformer equivalent of State is StateT; it adds mutable state to an
underlying monad. The WriterT monad transformer makes it possible to
write data when stacked on top of another monad.

A simple monad transformer example
----------------------------------

Before we introduce monad transformers, let's look at a function written
using techniques we are already familiar with. The function below
recurses into a directory tree, and returns a list of the number of
entries it finds at each level of the tree.

~~~~ {#CountEntries.hs:countEntriesTrad .programlisting}
-- file: ch18/CountEntries.hs
module CountEntries (listDirectory, countEntriesTrad) where

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad (forM, liftM)

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /= ".."

countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
  contents <- listDirectory path
  rest <- forM contents $ \name -> do
            let newName = path </> name
            isDir <- doesDirectoryExist newName
            if isDir
              then countEntriesTrad newName
              else return []
  return $ (path, length contents) : concat rest
~~~~

We'll now look at using the writer monad to achieve the same goal. Since
this monad lets us record a value wherever we want, we don't need to
explicitly build up a result.

As our function must execute in the IO monad so that it can traverse
directories, we can't use the Writer monad directly. Instead, we use
WriterT to add the recording capability to IO. We will find the going
easier if we look at the types involved.

The normal Writer monad has two type parameters, so it's more properly
written Writer w a. The first parameter `w`{.varname} is the type of the
values to be recorded, and `a`{.varname} is the usual type that the
Monad typeclass requires. Thus Writer [(FilePath, Int)] a is a writer
monad that records a list of directory names and sizes.

The WriterT transformer has a similar structure, but it adds another
type parameter `m`{.varname}: this is the underlying monad whose
behaviour we are augmenting. The full signature of WriterT is WriterT w
m a.

Because we need to traverse directories, which requires access to the IO
monad, we'll stack our writer on top of the IO monad. Our combination of
monad transformer and underlying monad will thus have the type WriterT
[(FilePath, Int)] IO a. This stack of monad transformer and monad is
itself a monad.

~~~~ {#CountEntriesT.hs:countEntries .programlisting}
-- file: ch18/CountEntriesT.hs
module CountEntriesT (listDirectory, countEntries) where

import CountEntries (listDirectory)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import Control.Monad (forM_, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell)

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName
    when isDir $ countEntries newName
~~~~

This code is not terribly different from our earlier version. We use
`liftIO`{.function} to expose the IO monad where necessary, and
`tell`{.function} to record a visit to a directory.

To run our code, we must use one of WriterT's execution functions.

~~~~ {#countEntries.ghci:runWriterT .screen}
ghci> :type runWriterT
runWriterT :: WriterT w m a -> m (a, w)
ghci> :type execWriterT
execWriterT :: (Monad m) => WriterT w m a -> m w
~~~~

These functions execute the action, then remove the WriterT wrapper and
give a result that is wrapped in the underlying monad. The
`runWriterT`{.function} function gives both the result of the action and
whatever was recorded as it ran, while `execWriterT`{.function} throws
away the result and just gives us what was recorded.

~~~~ {#countEntries.ghci:countEntries .screen}
ghci> :type countEntries ".."
countEntries ".." :: WriterT [(FilePath, Int)] IO ()
ghci> :type execWriterT (countEntries "..")
execWriterT (countEntries "..") :: IO [(FilePath, Int)]
ghci> take 4 `liftM` execWriterT (countEntries "..")
[("..",30),("../ch15",23),("../ch07",26),("../ch01",3)]
~~~~

We use a WriterT on top of IO because there is no IOT monad transformer.
Whenever we use the IO monad with one or more monad transformers, IO
will always be at the bottom of the stack.

Common patterns in monads and monad transformers
------------------------------------------------

Most of the monads and monad transformers in the `mtl`{.code} library
follow a few common patterns around naming and typeclasses.

To illustrate these rules, we will focus on a single straightforward
monad: the reader monad. The reader monad's API is detailed by the
MonadReader typeclass. Most `mtl`{.code} monads have similarly named
typeclasses: MonadWriter defines the API of the writer monad, and so on.

~~~~ {#Reader.hs:class .programlisting}
-- file: ch18/Reader.hs
class (Monad m) => MonadReader r m | m -> r where
    ask   :: m r
    local :: (r -> r) -> m a -> m a
~~~~

The type variable `r`{.varname} represents the immutable state that the
reader monad carries around. The Reader r monad is an instance of the
MonadReader class, as is the ReaderT r m monad transformer. Again, this
pattern is repeated by other mtl monads: there usually exist both a
concrete monad and a transformer, each of which are instances of the
typeclass that defines the monad's API.

Returning to the specifics of the reader monad, we haven't touched upon
the `local`{.function} function before. It temporarily modifies the
current environment using the r -\> r function, and executes its action
in the modified environment. To make this idea more concrete, here is a
simple example.

~~~~ {#LocalReader.hs:localExample .programlisting}
-- file: ch18/LocalReader.hs
import Control.Monad.Reader

myName step = do
  name <- ask
  return (step ++ ", I am " ++ name)

localExample :: Reader String (String, String, String)
localExample = do
  a <- myName "First"
  b <- local (++"dy") (myName "Second")
  c <- myName "Third"
  return (a, b, c)
~~~~

If we execute the `localExample`{.function} action in **ghci**, we can
see that the effect of modifying the environment is confined to one
place.

~~~~ {#localReader.ghci:localExample .screen}
ghci> runReader localExample "Fred"
Loading package mtl-1.1.0.0 ... linking ... done.
("First, I am Fred","Second, I am Freddy","Third, I am Fred")
~~~~

When the underlying monad `m`{.varname} is an instance of MonadIO, the
`mtl`{.code} library provides an instance for ReaderT r m, and also for
a number of other typeclasses. Here are a few.

~~~~ {#Reader.hs:instances .programlisting}
-- file: ch18/Reader.hs
instance (Monad m) => Functor (ReaderT r m) where
    ...

instance (MonadIO m) => MonadIO (ReaderT r m) where
    ...

instance (MonadPlus m) => MonadPlus (ReaderT r m) where
    ...
~~~~

Once again, most `mtl`{.code} monad transformers define instances like
these, to make it easier for us to work with them.

Stacking multiple monad transformers
------------------------------------

As we have already mentioned, when we stack a monad transformer on a
normal monad, the result is another monad. This suggests the possibility
that we can again stack a monad transformer on top of our combined
monad, to give a new monad, and in fact this is a common thing to do.
Under what circumstances might we want to create such a stack?

-   If we need to talk to the outside world, we'll have IO at the base
    of the stack. Otherwise, we will have some normal monad.

-   If we add a ReaderT layer, we give ourselves access to read-only
    configuration information.

-   Add a StateT layer, and we gain global state that we can modify.

-   Should we need the ability to log events, we can add a WriterT
    layer.

The power of this approach is that we can customise the stack to our
exact needs, specifying which kinds of effects we want to support.

As a small example of stacked monad transformers in action, here is a
reworking of the `countEntries`{.function} function we developed
earlier. We will modify it to recurse no deeper into a directory tree
than a given amount, and to record the maximum depth it reaches.

~~~~ {#UglyStack.hs:AppData .programlisting}
-- file: ch18/UglyStack.hs
import System.Directory
import System.FilePath
import Control.Monad.Reader
import Control.Monad.State

data AppConfig = AppConfig {
      cfgMaxDepth :: Int
    } deriving (Show)

data AppState = AppState {
      stDeepestReached :: Int
    } deriving (Show)
~~~~

We use ReaderT to store configuration data, in the form of the maximum
depth of recursion we will perform. We also use StateT to record the
maximum depth we reach during an actual traversal.

~~~~ {#UglyStack.hs:App .programlisting}
-- file: ch18/UglyStack.hs
type App = ReaderT AppConfig (StateT AppState IO)
~~~~

Our transformer stack has IO on the bottom, then StateT, with ReaderT on
top. In this particular case, it doesn't matter whether we have ReaderT
or WriterT on top, but IO must be on the bottom.

Even a small stack of monad transformers quickly develops an unwieldy
type name. We can use a `type`{.literal} alias to reduce the lengths of
the type signatures that we write.

![[Note]](/support/figs/note.png)

Where's the missing type parameter?

You might have noticed that our `type`{.literal} synonym doesn't have
the usual type parameter `a`{.varname} that we associate with a monadic
type:

~~~~ {#UglyStack.hs:App2 .programlisting}
-- file: ch18/UglyStack.hs
type App2 a = ReaderT AppConfig (StateT AppState IO) a
~~~~

Both App and App2 work fine in normal type signatures. The difference
arises when we try to construct another type from one of these. Say we
want to add another monad transformer to the stack: the compiler will
allow WriterT [String] App a, but reject WriterT [String] App2 a.

The reason for this is that Haskell does not allow us to partially apply
a type synonym. The synonym App doesn't take a type parameter, so it
doesn't pose a problem. However, because App2 takes a type parameter, we
must supply some type for that parameter if we want to use App2 to
create another type.

This restriction is limited to type synonyms. When we create a monad
transformer stack, we usually wrap it with a `newtype`{.code} (as we
will see below). As a result, we will rarely run into this problem in
practice.

The execution function for our monad stack is simple.

~~~~ {#UglyStack.hs:runApp .programlisting}
-- file: ch18/UglyStack.hs
runApp :: App a -> Int -> IO (a, AppState)
runApp k maxDepth =
    let config = AppConfig maxDepth
        state = AppState 0
    in runStateT (runReaderT k config) state
~~~~

Our application of `runReaderT`{.function} removes the ReaderT
transformer wrapper, while `runStateT`{.function} removes the StateT
wrapper, leaving us with a result in the IO monad.

Compared to earlier versions, the only complications we have added to
our traversal function are slight: we track our current depth, and
record the maximum depth we reach.

~~~~ {#UglyStack.hs:constrainedCount .programlisting}
-- file: ch18/UglyStack.hs
constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path
  cfg <- ask
  rest <- forM contents $ \name -> do
            let newPath = path </> name
            isDir <- liftIO $ doesDirectoryExist newPath
            if isDir && curDepth < cfgMaxDepth cfg
              then do
                let newDepth = curDepth + 1
                st <- get
                when (stDeepestReached st < newDepth) $
                  put st { stDeepestReached = newDepth }
                constrainedCount newDepth newPath
              else return []
  return $ (path, length contents) : concat rest
~~~~

Our use of monad transformers here is admittedly a little contrived.
Because we're writing a single straightforward function, we're not
really winning anything. What's useful about this approach, though, is
that it *scales* to bigger programs.

We can write most of an application's imperative-style code in a monad
stack similar to our App monad. In a real program, we'd carry around
more complex configuration data, but we'd still use ReaderT to keep it
read-only and hidden except when needed. We'd have more mutable state to
manage, but we'd still use StateT to encapsulate it.

### Hiding our work

We can use the usual `newtype`{.code} technique to erect a solid barrier
between the implementation of our custom monad and its interface.

~~~~ {#UglyStack.hs:MyApp .programlisting}
-- file: ch18/UglyStack.hs
newtype MyApp a = MyA {
      runA :: ReaderT AppConfig (StateT AppState IO) a
    } deriving (Monad, MonadIO, MonadReader AppConfig,
                MonadState AppState)

runMyApp :: MyApp a -> Int -> IO (a, AppState)
runMyApp k maxDepth =
    let config = AppConfig maxDepth
        state = AppState 0
    in runStateT (runReaderT (runA k) config) state
~~~~

If we export the MyApp type constructor and the `runMyApp`{.function}
execution function from a module, client code will not be able to tell
that the internals of our monad is a stack of monad transformers.

The large `deriving`{.code} clause requires the
`GeneralizedNewtypeDeriving`{.code} language pragma. It seems somehow
magical that the compiler can derive all of these instances for us. How
does this work?

Earlier, we mentioned that the `mtl`{.code} library provides instances
of a number of typeclasses for each monad transformer. For example, the
IO monad implements MonadIO. If the underlying monad is an instance of
MonadIO, `mtl`{.code} makes StateT an instance, too, and likewise for
ReaderT.

There is thus no magic going on: the top-level monad transformer in the
stack is an instance of all of the type classes that we're rederiving
with our `deriving`{.code} clause. This is a consequence of `mtl`{.code}
providing a carefully coordinated set of typeclasses and instances that
fit together well. There is nothing more going on than the usual
automatic derivation that we can perform with `newtype`{.code}
declarations.

### Exercises

**1.**

Modify the App type synonym to swap the order of ReaderT and WriterT.
What effect does this have on the `runApp`{.function} execution
function?

**2.**

Add the WriterT transformer to the App monad transformer stack. Modify
`runApp`{.function} to work with this new setup.

**3.**

Rewrite the `constrainedCount`{.function} function to record results
using the WriterT transformer in your new App stack.

Moving down the stack
---------------------

So far, our uses of monad transformers have been simple, and the
plumbing of the `mtl`{.code} library has allowed us to avoid the details
of how a stack of monads is constructed. Indeed, we already know enough
about monad transformers to simplify many common programming tasks.

There are a few useful ways in which we can depart from the comfort of
`mtl`{.code}. Most often, a custom monad sits at the bottom of the
stack, or a custom monad transformer lies somewhere within the stack. To
understand the potential difficulty, let's look at an example.

Suppose we have a custom monad transformer, CustomT.

~~~~ {#CustomT.hs:CustomT .programlisting}
-- file: ch18/CustomT.hs
newtype CustomT m a = ...
~~~~

In the framework that `mtl`{.code} provides, each monad transformer in
the stack makes the API of a lower level available by providing
instances of a host of typeclasses. We could follow this pattern, and
write a number of boilerplate instances.

~~~~ {#CustomT.hs:mtl .programlisting}
-- file: ch18/CustomT.hs
instance MonadReader r m => MonadReader r (CustomT m) where
    ...

instance MonadIO m => MonadIO (CustomT m) where
    ...
~~~~

If the underlying monad was an instance of MonadReader, we would write a
MonadReader instance for CustomT in which each function in the API
passes through to the corresponding function in the underlying instance.
This would allow higher level code to only care that the stack as a
whole is an instance of MonadReader, without knowing or caring about
which layer provides the *real* implementation.

Instead of relying on all of these typeclass instances to work for us
behind the scenes, we can be explicit. The MonadTrans typeclass defines
a useful function named `lift`{.function}.

~~~~ {#monadTrans.ghci:MonadTrans .screen}
ghci> :m +Control.Monad.Trans
ghci> :info MonadTrans
class MonadTrans t where lift :: (Monad m) => m a -> t m a
    -- Defined in Control.Monad.Trans
~~~~

This function takes a monadic action from one layer down the stack, and
turns it—in other words, *lifts* it—into an action in the current monad
transformer. Every monad transformer is an instance of MonadTrans.

We use the name `lift`{.function} based on its similarity of purpose to
`fmap`{.function} and `liftM`{.function}. In each case, we hoist
something from a lower level of the type system to the level we're
currently working in.

-   `fmap`{.function} elevates a pure function to the level of functors;

-   `liftM`{.function} takes a pure function to the level of monads;

-   and `lift`{.function} raises a monadic action from one level beneath
    in the transformer stack to the current one.

Let's revisit the App monad stack we defined earlier (before we wrapped
it with a `newtype`{.code}).

~~~~ {#id658428 .programlisting}
-- file: ch18/UglyStack.hs
type App = ReaderT AppConfig (StateT AppState IO)
~~~~

If we want to access the AppState carried by the StateT, we would
usually rely on `mtl`{.code}'s typeclasses and instances to handle the
plumbing for us.

~~~~ {#UglyStack.hs:implicitGet .programlisting}
-- file: ch18/UglyStack.hs
implicitGet :: App AppState
implicitGet = get
~~~~

The `lift`{.function} function lets us achieve the same effect, by
lifting `get`{.function} from StateT into ReaderT.

~~~~ {#UglyStack.hs:explicitGet .programlisting}
-- file: ch18/UglyStack.hs
explicitGet :: App AppState
explicitGet = lift get
~~~~

Obviously, when we can let `mtl`{.code} do this work for us, we end up
with cleaner code, but this is not always possible.

### When explicit lifting is necessary

One case in which we *must* use `lift`{.function} is when we create a
monad transformer stack in which instances of the same typeclass appear
at multiple levels.

~~~~ {#StackStack.hs:Foo .programlisting}
-- file: ch18/StackStack.hs
type Foo = StateT Int (State String)
~~~~

If we try to use the `put`{.function} action of the MonadState
typeclass, the instance we will get is that of StateT Int, because it's
at the top of the stack.

~~~~ {#StackStack.hs:outerPut .programlisting}
-- file: ch18/StackStack.hs
outerPut :: Int -> Foo ()
outerPut = put
~~~~

In this case, the only way we can access the underlying State monad's
`put`{.function} is through use of `lift`{.function}.

~~~~ {#StackStack.hs:innerPut .programlisting}
-- file: ch18/StackStack.hs
innerPut :: String -> Foo ()
innerPut = lift . put
~~~~

Sometimes, we need to access a monad more than one level down the stack,
in which case we must compose calls to `lift`{.function}. Each composed
use of `lift`{.function} gives us access to one deeper level.

~~~~ {#StackStack.hs:Bar .programlisting}
-- file: ch18/StackStack.hs
type Bar = ReaderT Bool Foo

barPut :: String -> Bar ()
barPut = lift . lift . put
~~~~

When we need to use `lift`{.function}, it can be good style to write
wrapper functions that do the lifting for us, as above, and to use
those. The alternative of sprinkling explicit uses of `lift`{.function}
throughout our code tends to look messy. Worse, it hard-wires the
details of the layout of our monad stack into our code, which will
complicate any subsequent modifications.

Understanding monad transformers by building one
------------------------------------------------

To give ourselves some insight into how monad transformers in general
work, we will create one and describe its machinery as we go. Our target
is simple and useful. Surprisingly, though, it is missing from the
`mtl`{.code} library: MaybeT.

This monad transformer modifies the behaviour of an underlying monad m a
by wrapping its type parameter with Maybe, to give m (Maybe a). As with
the Maybe monad, if we call `fail`{.function} in the MaybeT monad
transformer, execution terminates early.

In order to turn m (Maybe a) into a Monad instance, we must make it a
distinct type, via a `newtype`{.code} declaration.

~~~~ {#MaybeT.hs:newtype .programlisting}
-- file: ch18/MaybeT.hs
newtype MaybeT m a = MaybeT {
      runMaybeT :: m (Maybe a)
    }
~~~~

We now need to define the three standard monad functions. The most
complex is `(>>=)`{.function}, and its innards shed the most light on
what we are actually doing. Before we delve into its operation, let us
first take a look at its type.

~~~~ {#MaybeT.hs:bindMT.type .programlisting}
-- file: ch18/MaybeT.hs
bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
~~~~

To understand this type signature, hark back to our discussion of
multi-parameter typeclasses in [the section called “Multi-parameter
typeclasses”](programming-with-monads.html#monadcase.mptc "Multi-parameter typeclasses").
The thing that we intend to make a Monad instance is the *partial type*
MaybeT m: this has the usual single type parameter, `a`{.varname}, that
satisfies the requirements of the Monad typeclass.

The trick to understanding the body of our `(>>=)`{.function}
implementation is that everything inside the `do`{.literal} block
executes in the *underlying* monad m, whatever that is.

~~~~ {#MaybeT.hs:bindMT .programlisting}
-- file: ch18/MaybeT.hs
x `bindMT` f = MaybeT $ do
                 unwrapped <- runMaybeT x
                 case unwrapped of
                   Nothing -> return Nothing
                   Just y -> runMaybeT (f y)
~~~~

Our `runMaybeT`{.function} function unwraps the result contained in
`x`{.varname}. Next, recall that the `<-`{.code} symbol desugars to
`(>>=)`{.function}: a monad transformer's `(>>=)`{.function} must use
the underlying monad's `(>>=)`{.function}. The final bit of case
analysis determines whether we short circuit or chain our computation.
Finally, look back at the top of the body: here, we must wrap the result
with the MaybeT constructor, to once again hide the underlying monad.

The `do`{.literal} notation above might be pleasant to read, but it
hides the fact that we are relying on the underlying monad's
`(>>=)`{.function} implementation. Here is a more idiomatic version of
`(>>=)`{.function} for MaybeT that makes this clearer.

~~~~ {#MaybeT.hs:altBindMT .programlisting}
-- file: ch18/MaybeT.hs
x `altBindMT` f =
    MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)
~~~~

Now that we understand what `(>>=)`{.function} is doing, our
implementations of `return`{.function} and `fail`{.function} need no
explanation, and neither does our Monad instance.

~~~~ {#MaybeT.hs:Monad .programlisting}
-- file: ch18/MaybeT.hs
returnMT :: (Monad m) => a -> MaybeT m a
returnMT a = MaybeT $ return (Just a)

failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing
 
instance (Monad m) => Monad (MaybeT m) where
  return = returnMT
  (>>=) = bindMT
  fail = failMT
~~~~

### Creating a monad transformer

To turn our type into a monad transformer, we must provide an instance
of the MonadTrans class, so that a user can access the underlying monad.

~~~~ {#MaybeT.hs:MonadTrans .programlisting}
-- file: ch18/MaybeT.hs
instance MonadTrans MaybeT where
    lift m = MaybeT (Just `liftM` m)
~~~~

The underlying monad starts out with a type parameter of a: we “inject”
the `Just`{.code} constructor so it will acquire the type that we need,
Maybe a. We then hide the monad with our MaybeT constructor.

### More typeclass instances

Once we have an instance for MonadTrans defined, we can use it to define
instances for the umpteen other `mtl`{.code} typeclasses.

~~~~ {#MaybeT.hs:mtl .programlisting}
-- file: ch18/MaybeT.hs
instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO m = lift (liftIO m)

instance (MonadState s m) => MonadState s (MaybeT m) where
  get = lift get
  put k = lift (put k)

-- ... and so on for MonadReader, MonadWriter, etc ...
~~~~

Because several of the `mtl`{.code} typeclasses use functional
dependencies, some of our instance declarations require us to
considerably relax GHC's usual strict type checking rules. (If we were
to forget any of these directives, the compiler would helpfully advise
us which ones we needed in its error messages.)

~~~~ {#MaybeT.hs:LANGUAGE .programlisting}
-- file: ch18/MaybeT.hs
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances #-}
~~~~

Is it better to use `lift`{.function} explicitly, or to spend time
writing these boilerplate instances? That depends on what we expect to
do with our monad transformer. If we're going to use it in just a few
restricted situations, we can get away with providing an instance for
MonadTrans alone. In this case, a few more instances might still make
sense, such as MonadIO. On the other hand, if our transformer is going
to pop up in diverse situations throughout a body of code, spending a
dull hour to write those instances might be a good investment.

### Replacing the Parse type with a monad stack

Now that we have developed a monad transformer that can exit early, we
can use it to bail if, for example, a parse fails partway through. We
could thus replace the Parse type that we developed in [the section
called “Implicit
state”](code-case-study-parsing-a-binary-data-format.html#binary.implicit "Implicit state")
with a monad customised to our needs.

~~~~ {#MaybeTParse.hs:Parse .programlisting}
-- file: ch18/MaybeTParse.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MaybeTParse
    (
      Parse
    , evalParse
    ) where

import MaybeT
import Control.Monad.State
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as L

data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64
    } deriving (Show)

newtype Parse a = P {
      runP :: MaybeT (State ParseState) a
    } deriving (Monad, MonadState ParseState)

evalParse :: Parse a -> L.ByteString -> Maybe a
evalParse m s = evalState (runMaybeT (runP m)) (ParseState s 0)
~~~~

### Exercises

**1.**

Our Parse monad is not a perfect replacement for its earlier
counterpart. Because we are using Maybe instead of Either to represent a
result, we can't report any useful information if a parse fails.

Create an EitherT sometype monad transformer, and use it to implement a
more capable Parse monad that can report an error message if parsing
fails.

![[Tip]](/support/figs/tip.png)

Tip

If you like to explore the Haskell libraries for fun, you may have run
across an existing Monad instance for the Either type in the
`Control.Monad.Error`{.code} module. We suggest that you do not use that
as a guide. Its design is too restrictive: it turns Either String into a
monad, when you could use a type parameter instead of String.

*Hint*: If you follow this suggestion, you'll probably need to use the
`FlexibleInstances`{.code} language extension in your definition.

Transformer stacking order is important
---------------------------------------

From our early examples using monad transformers like ReaderT and
StateT, it might be easy to conclude that the order in which we stack
monad transformers doesn't matter.

When we stack StateT on top of State, it should be clearer that order
can indeed make a difference. The types StateT Int (State String) and
StateT String (State Int) might carry around the same information, but
we can't use them interchangeably. The ordering determines when we need
to use `lift`{.function} to get at one or the other piece of state.

Here's a case that more dramatically demonstrates the importance of
ordering. Suppose we have a computation that might fail, and we want to
log the circumstances under which it does so.

~~~~ {#MTComposition.hs:problem .programlisting}
-- file: ch18/MTComposition.hs
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Writer
import MaybeT

problem :: MonadWriter [String] m => m ()
problem = do
  tell ["this is where i fail"]
  fail "oops"
~~~~

Which of these monad stacks will give us the information we need?

~~~~ {#MTComposition.hs:types .programlisting}
-- file: ch18/MTComposition.hs
type A = WriterT [String] Maybe

type B = MaybeT (Writer [String])

a :: A ()
a = problem

b :: B ()
b = problem
~~~~

Let's try the alternatives in **ghci**.

~~~~ {#mtComposition.ghci:problem .screen}
ghci> runWriterT a
Loading package mtl-1.1.0.0 ... linking ... done.
Nothing
ghci> runWriter $ runMaybeT b
(Nothing,["this is where i fail"])
~~~~

This difference in results should not come as a surprise: just look at
the signatures of the execution functions.

~~~~ {#mtComposition.ghci:runWriterT .screen}
ghci> :t runWriterT
runWriterT :: WriterT w m a -> m (a, w)
ghci> :t runWriter . runMaybeT
runWriter . runMaybeT :: MaybeT (Writer w) a -> (Maybe a, w)
~~~~

Our WriterT-on-Maybe stack has Maybe as the underlying monad, so
`runWriterT`{.function} must give us back a result of type Maybe. In our
test case, we only get to see the log of what happened if nothing
actually went wrong!

Stacking monad transformers is analogous to composing functions. If we
change the order in which we apply functions, and we then get different
results, we are not surprised. So it is with monad transformers, too.

Putting monads and monad transformers into perspective
------------------------------------------------------

It's useful to step back from details for a few moments, and look at the
weaknesses and strengths of programming with monads and monad
transformers.

### Interference with pure code

Probably the biggest practical irritation of working with monads is that
a monad's type constructor often gets in our way when we'd like to use
pure code. Many useful pure functions need monadic counterparts, simply
to tack on a placeholder parameter `m`{.varname} for some monadic type
constructor.

~~~~ {#monadProblems.ghci:filter .screen}
ghci> :t filter
filter :: (a -> Bool) -> [a] -> [a]
ghci> :i filterM
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
    -- Defined in Control.Monad
~~~~

However, the coverage is incomplete: the standard libraries don't always
provide monadic versions of pure functions.

The reason for this lies in history. Eugenio Moggi introduced the idea
of using monads for programming in 1988, around the time the Haskell 1.0
standard was being developed. Many of the functions in today's
`Prelude`{.code} date back to Haskell 1.0, which was released in 1990.
In 1991, Philip Wadler started writing for a wider functional
programming audience about the potential of monads, at which point they
began to see some use.

Not until 1996, and the release of Haskell 1.3, did the standard acquire
support for monads. By this time, the language designers were already
constrained by backwards compatibility: they couldn't change the
signatures of functions in the `Prelude`{.code}, because it would have
broken existing code.

Since then, the Haskell community has learned a lot about creating
suitable abstractions, so that we can write code that is less affected
by the pure/monadic divide. You can find modern distillations of these
ideas in the `Data.Traversable`{.code} and `Data.Foldable`{.code}
modules. As appealing as those modules are, we do not cover them in this
book. This is in part for want of space, but also because if you're
still following our book at this point, you won't have trouble figuring
them out for yourself.

In an ideal world, would we make a break from the past, and switch over
`Prelude`{.code} to use Traversable and Foldable types? Probably not.
Learning Haskell is already a stimulating enough adventure for
newcomers. The Foldable and Traversable abstractions are easy to pick up
when we already understand functors and monads, but they would put early
learners on too pure a diet of abstraction. For teaching the language,
it's *good* that `map`{.function} operates on lists, not on functors.

### Overdetermined ordering

One of the principal reasons that we use monads is that they let us
specify an ordering for effects. Look again at a small snippet of code
we wrote earlier.

~~~~ {#id659646 .programlisting}
-- file: ch18/MTComposition.hs
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Writer
import MaybeT

problem :: MonadWriter [String] m => m ()
problem = do
  tell ["this is where i fail"]
  fail "oops"
~~~~

Because we are executing in a monad, we are guaranteed that the effect
of the `tell`{.function} will occur before the effect of
`fail`{.function}. The problem is that we get this guarantee of ordering
even when we don't necessarily want it: the compiler is not free to
rearrange monadic code, even if doing so would make it more efficient.

### Runtime overhead

Finally, when we use monads and monad transformers, we can pay an
efficiency tax. For instance, the State monad carries its state around
in a closure. Closures might be cheap in a Haskell implementation, but
they're not free.

A monad transformer adds its own overhead to that of whatever is
underneath. Our MaybeT transformer has to wrap and unwrap Maybe values
every time we use `(>>=)`{.function}. A stack of MaybeT on top of StateT
over ReaderT thus has a lot of book-keeping to do for each
`(>>=)`{.function}.

A sufficiently smart compiler might make some or all of these costs
vanish, but that degree of sophistication is not yet widely available.

There are relatively simple techniques to avoid some of these costs,
though we lack space to do more than mention them by name. For instance,
by using a continuation monad, we can avoid the constant wrapping and
unwrapping in `(>>=)`{.function}, only paying for effects when we use
them. Much of the complexity of this approach has already been packaged
up in libraries. This area of work is still under lively development as
we write. If you want to make your use of monad transformers more
efficient, we recommend looking on Hackage, or asking for directions on
a mailing list or IRC.

### Unwieldy interfaces

If we use the `mtl`{.code} library as a black box, all of its components
mesh quite nicely. However, once we start developing our own monads and
monad transformers, and using them with those provided by `mtl`{.code},
some deficiencies start to show.

For example, if we create a new monad transformer FooT and want to
follow the same pattern as `mtl`{.code}, we'll have it implement a
typeclass MonadFoo. If we really want to integrate it cleanly into the
`mtl`{.code}, we'll have to provide instances for each of the dozen or
so `mtl`{.code} type classes.

On top of that, we'll have to declare instances of MonadFoo for each of
the `mtl`{.code} transformers. Most of those instances will be almost
identical, and quite dull to write. If we want to keep integrating new
monad transformers into the `mtl`{.code} framework, the number of moving
parts we must deal with increases with the *square* of the number of new
transformers!

In fairness, this problem only matters to a tiny number of people. Most
users of `mtl`{.code} don't need to develop new transformers at all, so
they are not affected.

This weakness of `mtl`{.code}'s design lies with the fact that it was
the first library of monad transformers that was developed. Given that
its designers were plunging into the unknown, they did a remarkable job
of producing a powerful library that is easy for most users to
understand and work with.

A newer library of monads and transformers, `monadLib`{.code}, corrects
many of the design flaws in `mtl`{.code}. If at some point you turn into
a hard core hacker of monad transformers, it is well worth looking at.

The quadratic instances definition is actually a problem with the
approach of using monad transformers. There have been many other
approaches put forward for composing monads that don't have this
problem, but none of them seem as convenient to the end user as monad
transformers. Fortunately, there simply aren't that many foundational,
generically useful monad transformers.

### Pulling it all together

Monads are not by any means the end of the road when it comes to working
with effects and types. What they are is the most practical resting
point we have reached so far. Language researchers are always working on
systems that try to provide similar advantages, without the same
compromises.

Although we must make compromises when we use them, monads and monad
transformers still offer a degree of flexibility and control that has no
precedent in an imperative language. With just a few declarations, we
can rewire something as fundamental as the semicolon to give it a new
meaning.

\

* * * * *

^[[37](#id656986)]^The name `mtl`{.code} stands for “monad transformer
library”.

![image](/support/figs/rss.png) Want to stay up to date? Subscribe to
the comment feed for [this chapter](/feeds/comments/), or the [entire
book](/feeds/comments/).

Copyright 2007, 2008 Bryan O'Sullivan, Don Stewart, and John Goerzen.
This work is licensed under a [Creative Commons
Attribution-Noncommercial 3.0
License](http://creativecommons.org/licenses/by-nc/3.0/). Icons by [Paul
Davey](mailto:mattahan@gmail.com) aka
[Mattahan](http://mattahan.deviantart.com/).

  ----------------------------------------- -------------------- -----------------------------
  [Prev](interfacing-with-c-the-ffi.html)                        [Next](error-handling.html)
  Chapter 17. Interfacing with C: the FFI   [Home](index.html)   Chapter 19. Error handling
  ----------------------------------------- -------------------- -----------------------------


