[Real World Haskell](/)by Bryan O'Sullivan, Don Stewart, and John Goerzen
-------------------------------------------------------------------------

Chapter 3. Defining Types, Streamlining Functions

[Prev](types-and-functions.html)

[Next](functional-programming.html)

Chapter 3. Defining Types, Streamlining Functions
-------------------------------------------------

**Table of Contents**

[Defining a new data
type](defining-types-streamlining-functions.html#deftypes.data)

[Naming types and
values](defining-types-streamlining-functions.html#id582783)

[Type
synonyms](defining-types-streamlining-functions.html#deftypes.alias)

[Algebraic data
types](defining-types-streamlining-functions.html#deftypes.adt)

[Tuples, algebraic data types, and when to use
each](defining-types-streamlining-functions.html#id583233)

[Analogues to algebraic data types in other
languages](defining-types-streamlining-functions.html#deftypes.adt.comp)

[The structure](defining-types-streamlining-functions.html#id583523)

[The enumeration](defining-types-streamlining-functions.html#id583611)

[The discriminated
union](defining-types-streamlining-functions.html#id583781)

[Pattern
matching](defining-types-streamlining-functions.html#deftypes.pattern)

[Construction and
deconstruction](defining-types-streamlining-functions.html#id584277)

[Further
adventures](defining-types-streamlining-functions.html#id584453)

[Variable naming in
patterns](defining-types-streamlining-functions.html#id584746)

[The wild card
pattern](defining-types-streamlining-functions.html#deftypes.wildcard)

[Exhaustive patterns and wild
cards](defining-types-streamlining-functions.html#deftypes.patterns.nonexhaustive)

[Record
syntax](defining-types-streamlining-functions.html#deftypes.record)

[Parameterised
types](defining-types-streamlining-functions.html#deftypes.paramtypes)

[Recursive
types](defining-types-streamlining-functions.html#deftypes.recursive)

[Exercises](defining-types-streamlining-functions.html#id585938)

[Reporting
errors](defining-types-streamlining-functions.html#deftypes.error)

[A more controlled
approach](defining-types-streamlining-functions.html#deftypes.morecontrolled)

[Introducing local
variables](defining-types-streamlining-functions.html#deftypes.locals)

[Shadowing](defining-types-streamlining-functions.html#id586544)

[The where clause](defining-types-streamlining-functions.html#id586728)

[Local functions, global
variables](defining-types-streamlining-functions.html#deftypes.locals.functions)

[The offside rule and white space in an
expression](defining-types-streamlining-functions.html#deftypes.offside)

[A note about tabs versus
spaces](defining-types-streamlining-functions.html#deftypes.tabs)

[The offside rule is not
mandatory](defining-types-streamlining-functions.html#deftypes.block)

[The case
expression](defining-types-streamlining-functions.html#deftypes.case)

[Common beginner mistakes with
patterns](defining-types-streamlining-functions.html#deftypes.pattern.limits)

[Incorrectly matching against a
variable](defining-types-streamlining-functions.html#id587485)

[Incorrectly trying to compare for
equality](defining-types-streamlining-functions.html#id587613)

[Conditional evaluation with
guards](defining-types-streamlining-functions.html#deftypes.guard)

[Exercises](defining-types-streamlining-functions.html#id587860)

Defining a new data type
------------------------

Although lists and tuples are useful, we'll often want to construct new
data types of our own. This allows us to add structure to the values in
our programs. Instead of using an anonymous tuple, we can give a
collection of related values a name and a distinct type. Defining our
own types also improves the type safety of our code: Haskell will not
allow us to accidentally mix values of two types that are structurally
similar but have different names.

For motivation, we'll consider a few kinds of data that a small online
bookstore might need to manage. We won't make any attempt at complete or
realistic data definitions, but at least we're tying them to the real
world.

We define a new data type using the `data`{.code} keyword.

~~~~ {#BookStore.hs:BookInfo .programlisting}
-- file: ch03/BookStore.hs
data BookInfo = Book Int String [String]
                deriving (Show)
~~~~

The BookInfo after the `data`{.code} keyword is the name of our new
type. We call BookInfo a *type constructor*. Once we have defined a
type, we will use its type constructor to refer to it. As we've already
mentioned, a type name, and hence a type constructor, must start with a
capital letter.

The `Book`{.code} that follows is the name of the *value constructor*
(sometimes called a data constructor). We use this to create a value of
the BookInfo type. A value constructor's name must also start with a
capital letter.

After `Book`{.code}, the Int, String, and [String] that follow are the
*components* of the type. A component serves the same purpose in Haskell
as a field in a structure or class would in another language: it's a
“slot” where we keep a value. (We'll often refer to components as
fields.)

In this example, the Int represents a book's identifier (e.g. in a stock
database), String its title, and [String] the names of its authors.

To make the link to a concept we've already seen, the BookInfo type
contains the same components as a 3-tuple of type (Int, String,
[String]), but it has a distinct type. We can't accidentally (or
deliberately) use one in a context where the other is expected. For
instance, a bookstore is also likely to carry magazines.

~~~~ {#BookStore.hs:MagazineInfo .programlisting}
-- file: ch03/BookStore.hs
data MagazineInfo = Magazine Int String [String]
                    deriving (Show)
~~~~

Even though this MagazineInfo type has the same structure as our
BookInfo type, Haskell treats the types as distinct because their type
and value constructors have different names.

![[Note]](/support/figs/note.png)

Deriving what?

We'll explain the full meaning of `deriving       (Show)`{.code} later,
in [the section called
“Show”](using-typeclasses.html#typeclasses.wellknown.show "Show"). For
now, it's enough to know that we need to tack this onto a type
declaration so that **ghci** will automatically know how to print a
value of this type.

We can create a new value of type BookInfo by treating `Book`{.function}
as a function, and applying it with arguments of types Int, String, and
[String].

~~~~ {#BookStore.hs:myInfo .programlisting}
-- file: ch03/BookStore.hs
myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]
~~~~

Once we have defined a type, we can experiment with it in **ghci**. We
begin by using the **:load** command to load our source file.

~~~~ {#bookstore.ghci:load .screen}
ghci> :load BookStore
[1 of 1] Compiling Main             ( BookStore.hs, interpreted )
Ok, modules loaded: Main.
~~~~

Remember the `myInfo`{.code} variable we defined in our source file?
Here it is.

~~~~ {#bookstore.ghci:myInfo .screen}
ghci> myInfo
Book 9780135072455 "Algebra of Programming" ["Richard Bird","Oege de Moor"]
ghci> :type myInfo
myInfo :: BookInfo
~~~~

We can construct new values interactively in **ghci**, too.

~~~~ {#bookstore.ghci:newValue .screen}
ghci> Book 0 "The Book of Imaginary Beings" ["Jorge Luis Borges"]
Book 0 "The Book of Imaginary Beings" ["Jorge Luis Borges"]
~~~~

The **ghci** command **:type** lets us see what the type of an
expression is.

~~~~ {#bookstore.ghci:valueType .screen}
ghci> :type Book 1 "Cosmicomics" ["Italo Calvino"]
Book 1 "Cosmicomics" ["Italo Calvino"] :: BookInfo
~~~~

Remember that if we want to define a new variable inside **ghci**, the
syntax is slightly different from that of a Haskell source file: we need
to put a `let`{.literal} in front.

~~~~ {#bookstore.ghci:newVar .screen}
ghci> let cities = Book 173 "Use of Weapons" ["Iain M. Banks"]
~~~~

To find out more about a type, we can use some of **ghci**'s browsing
capabilities. The **:info** command gets **ghci** to tell us everything
it knows about a name.

~~~~ {#bookstore.ghci:info .screen}
ghci> :info BookInfo
data BookInfo = Book Int String [String]
    -- Defined at BookStore.hs:4:5-12
instance Show BookInfo -- Defined at BookStore.hs:4:5-12
~~~~

We can also find out why we use `Book`{.function} to construct a new
value of type BookStore.

~~~~ {#bookstore.ghci:type .screen}
ghci> :type Book
Book :: Int -> String -> [String] -> BookInfo
~~~~

We can treat a value constructor as just another function, one that
happens to create and return a new value of the type we desire.

### Naming types and values

When we introduced the type BookStore, we deliberately chose to give the
type constructor BookStore a different name from the value constructor
`Book`{.code}, purely to make it obvious which was which.

However, in Haskell, the names of types and values are independent of
each other. We only use a type constructor (i.e. the type's name) in a
type declaration or a type signature. We only use a value constructor in
actual code. Because these uses are distinct, there is no ambiguity if
we give a type constructor and a value constructor the same name. If we
are writing a type signature, we must be referring to a type
constructor. If we are writing an expression, we must be using the value
constructor.

~~~~ {#BookStore.hs:BookReview .programlisting}
-- file: ch03/BookStore.hs
-- We will introduce the CustomerID type shortly.

data BookReview = BookReview BookInfo CustomerID String
~~~~

This definition says that the type named BookReview has a value
constructor that is also named `BookReview`{.code}.

Not only is it *legal* for a value constructor to have the same name as
its type constructor, it's *normal*: you'll see this all the time in
regular Haskell code.

Type synonyms
-------------

We can introduce a *synonym* for an existing type at any time, to give a
type a more descriptive name. For example, the String in our BookReview
type doesn't tell us what the string is for, but we can clarify this.

~~~~ {#BookStore.hs:BetterReview .programlisting}
-- file: ch03/BookStore.hs
type CustomerID = Int
type ReviewBody = String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody
~~~~

The `type`{.literal} keyword introduces a type synonym. The new name is
on the left of the `=`{.code}, with the existing name on the right. The
two names identify the same type, so type synonyms are *purely* for
making code more readable.

We can also use a type synonym to create a shorter name for a verbose
type.

~~~~ {#BookStore.hs:BookRecord .programlisting}
-- file: ch03/BookStore.hs
type BookRecord = (BookInfo, BookReview)
~~~~

This states that we can use BookRecord as a synonym for the tuple
(BookInfo, BookReview). A type synonym only creates a new name that
refers to an existing type^[[7](#ftn.id582956)]^. We still use the same
value constructors to create a value of the type.

Algebraic data types
--------------------

The familiar Bool is the simplest common example of a category of type
called an *algebraic data type*. An algebraic data type can have more
than one value constructor.

~~~~ {#Bool.hs:Bool .programlisting}
-- file: ch03/Bool.hs
data Bool = False | True
~~~~

The Bool type has two value constructors, `True`{.code} and
`False`{.code}. Each value constructor is separated in the definition by
a `|`{.literal} character, which we can read as “or”: we can construct a
Bool that has the value `True`{.code}, or the value `False`{.code}. When
a type has more than one value constructor, they are usually referred to
as *alternatives* or *cases*. We can use any one of the alternatives to
create a value of that type.

![[Note]](/support/figs/note.png)

A note about naming

Although the phrase “algebraic data type” is long, we're being careful
to avoid using the acronym “ADT”. That acronym is already widely
understood to stand for “*abstract* data type”. Since Haskell supports
both algebraic and abstract data types, we'll be explicit and avoid the
acronym entirely.

Each of an algebraic data type's value constructors can take zero or
more arguments. As an example, here's one way we might represent billing
information.

~~~~ {#BookStore.hs:BillingInfo .programlisting}
-- file: ch03/BookStore.hs
type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)
~~~~

Here, we're saying that we support three ways to bill our customers. If
they want to pay by credit card, they must supply a card number, the
holder's name, and the holder's billing address as arguments to the
`CreditCard`{.code} value constructor. Alternatively, they can pay the
person who delivers their shipment. Since we don't need to store any
extra information about this, we specify no arguments for the
`CashOnDelivery`{.code} constructor. Finally, we can send an invoice to
the specified customer, in which case we need their CustomerID as an
argument to the `Invoice`{.code} constructor.

When we use a value constructor to create a value of type BillingInfo,
we must supply the arguments that it requires.

~~~~ {#bookstore.ghci:billingInfo .screen}
ghci> :type CreditCard
CreditCard :: CardNumber -> CardHolder -> Address -> BillingInfo
ghci> CreditCard "2901650221064486" "Thomas Gradgrind" ["Dickens", "England"]
CreditCard "2901650221064486" "Thomas Gradgrind" ["Dickens","England"]
ghci> :type it
it :: BillingInfo
ghci> Invoice

<interactive>:1:0:
    No instance for (Show (CustomerID -> BillingInfo))
      arising from a use of `print' at <interactive>:1:0-6
    Possible fix:
      add an instance declaration for (Show (CustomerID -> BillingInfo))
    In the expression: print it
    In a 'do' expression: print it
ghci> :type it
it :: BillingInfo
~~~~

The `No instance`{.code} error message arose because we did not supply
an argument to the `Invoice`{.code} constructor. As a result, we were
trying to print the `Invoice`{.code} constructor itself. That
constructor requires an argument and returns a value, so it is a
function. We cannot print functions in Haskell, which is ultimately why
the interpreter complained.

### Tuples, algebraic data types, and when to use each

There is some overlap between tuples and user-defined algebraic data
types. If we wanted to, we could represent our BookInfo type from
earlier as an (Int, String, [String]) tuple.

~~~~ {#bookstore.ghci:tuple .screen}
ghci> Book 2 "The Wealth of Networks" ["Yochai Benkler"]
Book 2 "The Wealth of Networks" ["Yochai Benkler"]
ghci> (2, "The Wealth of Networks", ["Yochai Benkler"])
(2,"The Wealth of Networks",["Yochai Benkler"])
~~~~

Algebraic data types allow us to distinguish between otherwise identical
pieces of information. Two tuples with elements of the same type are
structurally identical, so they have the same type.

~~~~ {#Distinction.hs:tuples .programlisting}
-- file: ch03/Distinction.hs
a = ("Porpoise", "Grey")
b = ("Table", "Oak")
~~~~

Since they have different names, two algebraic data types have distinct
types, even if they are otherwise structurally equivalent.

~~~~ {#Distinction.hs:data .programlisting}
-- file: ch03/Distinction.hs
data Cetacean = Cetacean String String
data Furniture = Furniture String String

c = Cetacean "Porpoise" "Grey"
d = Furniture "Table" "Oak"
~~~~

This lets us bring the type system to bear in writing programs with
fewer bugs. With the tuples we defined above, we could conveivably pass
a description of a whale to a function expecting a chair, and the type
system could not help us. With the algebraic data types, there is no
such possibility of confusion.

Here is a more subtle example. Consider the following representations of
a two-dimensional vector.

~~~~ {#AlgebraicVector.hs:types .programlisting}
-- file: ch03/AlgebraicVector.hs
-- x and y coordinates or lengths.
data Cartesian2D = Cartesian2D Double Double
                   deriving (Eq, Show)

-- Angle and distance (magnitude).
data Polar2D = Polar2D Double Double
               deriving (Eq, Show)
~~~~

The Cartesian and polar forms use the same types for their two elements.
However, the *meanings* of the elements are different. Because
Cartesian2D and Polar2D are distinct types, the type system will not let
us accidentally use a Cartesian2D value where a Polar2D is expected, or
vice versa.

~~~~ {#algebraicvector.ghci:typed .screen}
ghci> Cartesian2D (sqrt 2) (sqrt 2) == Polar2D (pi / 4) 2

<interactive>:1:33:
    Couldn't match expected type `Cartesian2D'
           against inferred type `Polar2D'
    In the second argument of `(==)', namely `Polar2D (pi / 4) 2'
    In the expression:
          Cartesian2D (sqrt 2) (sqrt 2) == Polar2D (pi / 4) 2
    In the definition of `it':
        it = Cartesian2D (sqrt 2) (sqrt 2) == Polar2D (pi / 4) 2
~~~~

The `(==)`{.function} operator requires its arguments to have the same
type.

![[Tip]](/support/figs/tip.png)

Comparing for equality

Notice that in the `deriving`{.code} clause for our vector types, we
added another word, `Eq`{.code}. This causes the Haskell implementation
to generate code that lets us compare the values for equality.

If we used tuples to represent these values, we could quickly land
ourselves in hot water by mixing the two representations
inappropriately.

~~~~ {#algebraicvector.ghci:tupled .screen}
ghci> (1, 2) == (1, 2)
True
~~~~

The type system can't rescue us here: as far as it's concerned, we're
comparing two (Double, Double) pairs, which is a perfectly valid thing
to do. Indeed, we cannot tell by inspection which of these values is
supposed to be polar or Cartesian, but `(1,2)`{.code} has a different
meaning in each representation.

There is no hard and fast rule for deciding when it's better to use a
tuple or a distinct data type, but here's a rule of thumb to follow. If
you're using compound values widely in your code (as almost all
non-trivial programs do), adding `data`{.literal} declarations will
benefit you in both type safety and readability. For smaller, localised
uses, a tuple is usually fine.

### Analogues to algebraic data types in other languages

Algebraic data types provide a single powerful way to describe data
types. Other languages often need several different features to achieve
the same degree of expressiveness. Here are some analogues from C and
C++, which might make it clearer what we can do with algebraic data
types, and how they relate to concepts that might be more familiar.

#### The structure

With just one constructor, an algebraic data type is similar to a tuple:
it groups related values together into a compound value. It corresponds
to a `struct`{.code} in C or C++, and its components correspond to the
fields of a `struct`{.code}. Here's a C equivalent of the BookInfo type
that we defined earlier.

~~~~ {#types.c:book_info .programlisting}
struct book_info {
    int id;
    char *name;
    char **authors;
};
~~~~

The main difference between the two is that the fields in the Haskell
type are anonymous and positional.

~~~~ {#id583567 .programlisting}
-- file: ch03/BookStore.hs
data BookInfo = Book Int String [String]
                deriving (Show)
~~~~

By *positional*, we mean that the section number is in the first field
of the Haskell type, and the title is in the second. We refer to them by
location, not by name.

In [the section called “Pattern
matching”](defining-types-streamlining-functions.html#deftypes.pattern "Pattern matching"),
we'll see how to access the fields of a BookStore value. In [the section
called “Record
syntax”](defining-types-streamlining-functions.html#deftypes.record "Record syntax"),
we'll introduce an alternate syntax for defining data types that looks a
little more C-like.

#### The enumeration

Algebraic data types also serve where we'd use an `enum`{.code} in C or
C++, to represent a range of symbolic values. Such algebraic data types
are sometimes referred to as enumeration types. Here's an example from
C.

~~~~ {#types.c:roygbiv .programlisting}
enum roygbiv {
    red,
    orange,
    yellow,
    green,
    blue,
    indigo,
    violet,
};
~~~~

And here's a Haskell equivalent.

~~~~ {#Roygbiv.hs:Roygbiv .programlisting}
-- file: ch03/Roygbiv.hs

data Roygbiv = Red
             | Orange
             | Yellow
             | Green
             | Blue
             | Indigo
             | Violet
               deriving (Eq, Show)
~~~~

We can try these out in **ghci**.

~~~~ {#roygbiv.ghci:using .screen}
ghci> :type Yellow
Yellow :: Roygbiv
ghci> :type Red
Red :: Roygbiv
ghci> Red == Yellow
False
ghci> Green == Green
True
~~~~

In C, the elements of an `enum`{.code} are integers. We can use an
integer in a context where an `enum`{.code} is expected, and vice versa:
a C compiler will automatically convert values between the two types.
This can be a source of nasty bugs. In Haskell, this kind of problem
does not occur. For example, we cannot use a Roygbiv value where an
`Int`{.code} is expected.

~~~~ {#roygbiv.ghci:types .screen}
ghci> take 3 "foobar"
"foo"
ghci> take Red "foobar"

<interactive>:1:5:
    Couldn't match expected type `Int' against inferred type `Roygbiv'
    In the first argument of `take', namely `Red'
    In the expression: take Red "foobar"
    In the definition of `it': it = take Red "foobar"
~~~~

#### The discriminated union

If an algebraic data type has multiple alternatives, we can think of it
as similar to a `union`{.code} in C or C++. A big difference between the
two is that a union doesn't tell us which alternative is actually
present; we have to explicitly and manually track which alternative
we're using, usually in another field of an enclosing struct. This means
that unions can be sources of nasty bugs, where our notion of which
alternative we should be using is incorrect.

~~~~ {#types.c:shape .programlisting}
enum shape_type {
    shape_circle,
    shape_poly,
};

struct circle {
    struct vector centre;
    float radius;
};

struct poly {
    size_t num_vertices;
    struct vector *vertices;
};

struct shape 
{
    enum shape_type type;
    union {
    struct circle circle;
    struct poly poly;
    } shape;
};
~~~~

In the example above, the `union`{.code} can contain valid data for
either a `struct circle`{.code} or a `struct poly`{.code}. We have to
use the `enum         shape_type`{.code} by hand to indicate which kind
of value is currently stored in the `union`{.code}.

The Haskell version of this code is both dramatically shorter and safer
than the C equivalent.

~~~~ {#ShapeUnion.hs:Shape .programlisting}
-- file: ch03/ShapeUnion.hs
type Vector = (Double, Double)

data Shape = Circle Vector Double
           | Poly [Vector]
~~~~

If we create a Shape value using the `Circle`{.code} constructor, the
fact that we created a `Circle`{.code} is stored. When we later use a
`Circle`{.code}, we can't accidentally treat it as a `Square`{.code}. We
will see why in [the section called “Pattern
matching”](defining-types-streamlining-functions.html#deftypes.pattern "Pattern matching")

![[Tip]](/support/figs/tip.png)

A few notes

From reading the preceding sections, it should now be clear that *all*
of the data types that we define with the `data`{.code} keyword are
algebraic data types. Some may have just one alternative, while others
have several, but they're all using the same machinery.

Pattern matching
----------------

Now that we've seen how to construct values with algebraic data types,
let's discuss how we work with these values. If we have a value of some
type, there are two things we would like to be able to do.

-   If the type has more than one value constructor, we need to be able
    to tell which value constructor was used to create the value.

-   If the value constructor has data components, we need to be able to
    extract those values.

Haskell has a simple, but tremendously useful, *pattern matching*
facility that lets us do both of these things.

A pattern lets us look inside a value and bind variables to the data it
contains. Here's an example of pattern matching in action on a Bool
value: we're going to reproduce the `not`{.function} function.

~~~~ {#add.hs:myNot .programlisting}
-- file: ch03/add.hs
myNot True  = False
myNot False = True
~~~~

It might seem that we have two functions named `myNot`{.function} here,
but Haskell lets us define a function as a *series of equations*: these
two clauses are defining the behavior of the same function for different
patterns of input. On each line, the patterns are the items following
the function name, up until the `=`{.code} sign.

To understand how pattern matching works, let's step through an example,
say `myNot False`{.code}.

When we apply `myNot`{.function}, the Haskell runtime checks the value
we supply against the value constructor in the first pattern. This does
not match, so it tries against the second pattern. That match succeeds,
so it uses the right hand side of that equation as the result of the
function application.

Here is a slightly more extended example. This function adds together
the elements of a list.

~~~~ {#add.hs:sumList .programlisting}
-- file: ch03/add.hs
sumList (x:xs) = x + sumList xs
sumList []     = 0
~~~~

Let us step through the evaluation of `sumList     [1,2]`{.code}. The
list notation `[1,2]`{.code} is shorthand for the expression
`(1:(2:[]))`{.code}. We begin by trying to match the pattern in the
first equation of the definition of `sumList`{.code}. In the
`(x:xs)`{.code} pattern, the “`:`{.code}” is the familiar list
constructor, `(:)`{.function}. We are now using it to match against a
value, not to construct one. The value `(1:(2:[]))`{.code} was
constructed with `(:)`{.code}, so the constructor in the value matches
the constructor in the pattern. We say that the pattern *matches*, or
that the match *succeeds*.

The variables `x`{.varname} and `xs`{.varname} are now “bound to” the
constructor's arguments, so `x`{.varname} is given the value `1`{.code},
and `xs`{.code} the value `2:[]`{.code}.

The expression we are now evaluating is `1 +     sumList (2:[])`{.code}.
We must now recursively apply `sumList`{.function} to the value
`2:[]`{.code}. Once again, this was constructed using `(:)`{.code}, so
the match succeeds. In our recursive application of
`sumList`{.function}, `x`{.varname} is now bound to `2`{.code}, and
`xs`{.varname} to `[]`{.code}.

We are now evaluating `1 + (2 + sumList     [])`{.code}. In this
recursive application of `sumList`{.function}, the value we are matching
against is `[]`{.code}. The value's constructor does not match the
constructor in the first pattern, so we skip this equation. Instead, we
“fall through” to the next pattern, which matches. The right hand side
of this equation is thus chosen as the result of this application.

The result of `sumList [1,2]`{.code} is thus `1 + (2 + (0))`{.code}, or
`3`{.code}.

![[Note]](/support/figs/note.png)

Ordering is important

As we have already mentioned, a Haskell implementation checks patterns
for matches in the order in which we specify them in our equations.
Matching proceeds from top to bottom, and stops at the first success.
Equations below a successful match have no effect.

As a final note, there already exists a standard function,
`sum`{.function}, that performs this sum-of-a-list for us. Our
`sumList`{.function} is purely for illustration.

### Construction and deconstruction

Let's step back and take a look at the relationship between constructing
a value and pattern matching on it.

We apply a value constructor to build a value. The expression
`Book 9 "Close Calls" ["John Long"]`{.code} applies the
`Book`{.function} constructor to the values `9`{.code},
`"Close Calls"`{.code}, and `["John Long"]`{.code} to produce a new
value of type BookInfo.

When we pattern match against the `Book`{.function} constructor, we
*reverse* the construction process. First of all, we check to see if the
value was created using that constructor. If it was, we inspect it to
obtain the individual values that we originally supplied to the
constructor when we created the value.

Let's consider what happens if we match the pattern
`(Book id name authors)`{.code} against our example expression.

-   The match will succeed, because the constructor in the value matches
    the one in our pattern.

-   The variable `id`{.varname} will be bound to `9`{.code}.

-   The variable `name`{.varname} will be bound to
    `"Close Calls"`{.code}.

-   The variable `authors`{.varname} will be bound to
    `["John Long"]`{.code}.

Because pattern matching acts as the inverse of construction, it's
sometimes referred to as *de*construction.

![[Note]](/support/figs/note.png)

Deconstruction doesn't destroy anything

If you're steeped in object oriented programming jargon, don't confuse
deconstruction with destruction! Matching a pattern has no effect on the
value we're examining: it just lets us “look inside” it.

### Further adventures

The syntax for pattern matching on a tuple is similar to the syntax for
constructing a tuple. Here's a function that returns the last element of
a 3-tuple.

~~~~ {#Tuple.hs:third .programlisting}
-- file: ch03/Tuple.hs
third (a, b, c) = c
~~~~

There's no limit on how “deep” within a value a pattern can look. This
definition looks both inside a tuple and inside a list within that
tuple.

~~~~ {#Tuple.hs:complicated .programlisting}
-- file: ch03/Tuple.hs
complicated (True, a, x:xs, 5) = (a, xs)
~~~~

We can try this out interactively.

~~~~ {#tuple.ghci:complicated .screen}
ghci> :load Tuple.hs
[1 of 1] Compiling Main             ( Tuple.hs, interpreted )
Ok, modules loaded: Main.
ghci> complicated (True, 1, [1,2,3], 5)
(1,[2,3])
~~~~

Wherever a literal value is present in a pattern (`True`{.literal} and
`5`{.literal} in the tuple pattern above), that value must match exactly
for the pattern match to succeed. If every pattern within a series of
equations fails to match, we get a runtime error.

~~~~ {#tuple.ghci:nomatch .screen}
ghci> complicated (False, 1, [1,2,3], 5)
*** Exception: Tuple.hs:10:0-39: Non-exhaustive patterns in function complicated
~~~~

For an explanation of this error message, skip forward a little, to [the
section called “Exhaustive patterns and wild
cards”](defining-types-streamlining-functions.html#deftypes.patterns.nonexhaustive "Exhaustive patterns and wild cards").

We can pattern match on an algebraic data type using its value
constructors. Recall the BookInfo type we defined earlier: we can
extract the values from a BookInfo as follows.

~~~~ {#BookStore.hs:accessors .programlisting}
-- file: ch03/BookStore.hs
bookID      (Book id title authors) = id
bookTitle   (Book id title authors) = title
bookAuthors (Book id title authors) = authors
~~~~

Let's see it in action.

~~~~ {#bookstore.ghci:unwrap .screen}
ghci> bookID (Book 3 "Probability Theory" ["E.T.H. Jaynes"])
3
ghci> bookTitle (Book 3 "Probability Theory" ["E.T.H. Jaynes"])
"Probability Theory"
ghci> bookAuthors (Book 3 "Probability Theory" ["E.T.H. Jaynes"])
["E.T.H. Jaynes"]
~~~~

The compiler can infer the types of the accessor functions based on the
constructor we're using in our pattern.

~~~~ {#bookstore.ghci:unwrap.types .screen}
ghci> :type bookID
bookID :: BookInfo -> Int
ghci> :type bookTitle
bookTitle :: BookInfo -> String
ghci> :type bookAuthors
bookAuthors :: BookInfo -> [String]
~~~~

If we use a literal value in a pattern, the corresponding part of the
value we're matching against must contain an identical value. For
instance, the pattern `(3:xs)`{.code} first of all checks that a value
is a non-empty list, by matching against the `(:)`{.function}
constructor. It also ensures that the head of the list has the exact
value `3`{.literal}. If both of these conditions hold, the tail of the
list will be bound to the variable `xs`{.varname}.

### Variable naming in patterns

As you read functions that match on lists, you'll frequently find that
the names of the variables inside a pattern resemble `(x:xs)`{.code} or
`(d:ds)`{.code}. This is a popular naming convention. The idea is that
the name `xs`{.varname} has an “`s`{.code}” on the end of its name as if
it's the “plural” of `x`{.varname}, because `x`{.varname} contains the
head of the list, and `xs`{.varname} the remaining elements.

### The wild card pattern

We can indicate that we don't care what is present in part of a pattern.
The notation for this is the underscore character “`_`{.code}”, which we
call a *wild card*. We use it as follows.

~~~~ {#BookStore.hs:niceAccessors .programlisting}
-- file: ch03/BookStore.hs
nicerID      (Book id _     _      ) = id
nicerTitle   (Book _  title _      ) = title
nicerAuthors (Book _  _     authors) = authors
~~~~

Here, we have tidier versions of the accessor functions we introduced
earlier. Now, there's no question about which element we're using in
each function.

In a pattern, a wild card acts similarly to a variable, but it doesn't
bind a new variable. As the examples above indicate, we can use more
than one wild card in a single pattern.

Another advantage of wild cards is that a Haskell compiler can warn us
if we introduce a variable name in a pattern, but do not use it in a
function's body. Defining a variable, but forgetting to use it, can
often indicate the presence of a bug, so this is a helpful feature. If
we use a wild card instead of a variable that we do not intend to use,
the compiler won't complain.

### Exhaustive patterns and wild cards

When writing a series of patterns, it's important to cover all of a
type's constructors. For example, if we're inspecting a list, we should
have one equation that matches the non-empty constructor
`(:)`{.function}, and one that matches the empty-list constructor
`[]`{.function}.

Let's see what happens if we fail to cover all the cases. Here, we
deliberately omit a check for the `[]`{.function} constructor.

~~~~ {#BadPattern.hs:badExample .programlisting}
-- file: ch03/BadPattern.hs
badExample (x:xs) = x + badExample xs
~~~~

If we apply this to a value that it cannot match, we'll get an error at
runtime: our software has a bug!

~~~~ {#badpattern.ghci:error .screen}
ghci> badExample []
*** Exception: BadPattern.hs:4:0-36: Non-exhaustive patterns in function badExample
~~~~

In this example, no equation in the function's definition matches the
value `[]`{.code}.

![[Tip]](/support/figs/tip.png)

Warning about incomplete patterns

GHC provides a helpful compilation option,
`-fwarn-incomplete-patterns`{.option}, that will cause it to print a
warning during compilation if a sequence of patterns don't match all of
a type's value constructors.

If we need to provide a default behavior in cases where we don't care
about specific constructors, we can use a wild card pattern.

~~~~ {#BadPattern.hs:goodExample .programlisting}
-- file: ch03/BadPattern.hs
goodExample (x:xs) = x + goodExample xs
goodExample _      = 0
~~~~

The wild card above will match the `[]`{.code} constructor, so applying
this function does not lead to a crash.

~~~~ {#badpattern.ghci:ok .screen}
ghci> goodExample []
0
ghci> goodExample [1,2]
3
~~~~

Record syntax
-------------

Writing accessor functions for each of a data type's components can be
repetitive and tedious.

~~~~ {#id585051 .programlisting}
-- file: ch03/BookStore.hs
nicerID      (Book id _     _      ) = id
nicerTitle   (Book _  title _      ) = title
nicerAuthors (Book _  _     authors) = authors
~~~~

We call this kind of code *boilerplate*: necessary, but bulky and
irksome. Haskell programmers don't like boilerplate. Fortunately, the
language addresses this particular boilerplate problem: we can define a
data type, and accessors for each of its components, simultaneously.
(The positions of the commas here is a matter of preference. If you
like, put them at the end of a line instead of the beginning.)

~~~~ {#BookStore.hs:Customer .programlisting}
-- file: ch03/BookStore.hs
data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)
~~~~

This is almost exactly identical in meaning to the following, more
familiar form.

~~~~ {#AltCustomer.hs:Customer .programlisting}
-- file: ch03/AltCustomer.hs
data Customer = Customer Int String [String]
                deriving (Show)

customerID :: Customer -> Int
customerID (Customer id _ _) = id

customerName :: Customer -> String
customerName (Customer _ name _) = name

customerAddress :: Customer -> [String]
customerAddress (Customer _ _ address) = address
~~~~

For each of the fields that we name in our type definition, Haskell
creates an accessor function of that name.

~~~~ {#bookstore.ghci:accessor.type .screen}
ghci> :type customerID
customerID :: Customer -> CustomerID
~~~~

We can still use the usual application syntax to create a value of this
type.

~~~~ {#BookStore.hs:customer1 .programlisting}
-- file: ch03/BookStore.hs
customer1 = Customer 271828 "J.R. Hacker"
            ["255 Syntax Ct",
             "Milpitas, CA 95134",
             "USA"]
~~~~

Record syntax adds a more verbose notation for creating a value. This
can sometimes make code more readable.

~~~~ {#BookStore.hs:customer2 .programlisting}
-- file: ch03/BookStore.hs
customer2 = Customer {
              customerID = 271828
            , customerAddress = ["1048576 Disk Drive",
                                 "Milpitas, CA 95134",
                                 "USA"]
            , customerName = "Jane Q. Citizen"
            }
~~~~

If we use this form, we can vary the order in which we list fields.
Here, we have moved the name and address fields from their positions in
the declaration of the type.

When we define a type using record syntax, it also changes the way the
type's values are printed.

~~~~ {#bookstore.ghci:customer1 .screen}
ghci> customer1
Customer {customerID = 271828, customerName = "J.R. Hacker", customerAddress = ["255 Syntax Ct","Milpitas, CA 95134","USA"]}
~~~~

For comparison, let's look at a BookInfo value; we defined this type
without record syntax.

~~~~ {#bookstore.ghci:cities .screen}
ghci> cities
Book 173 "Use of Weapons" ["Iain M. Banks"]
~~~~

The accessor functions that we get “for free” when we use record syntax
really are normal Haskell functions.

~~~~ {#bookstore.ghci:accessor .screen}
ghci> :type customerName
customerName :: Customer -> String
ghci> customerName customer1
"J.R. Hacker"
~~~~

The standard `System.Time`{.literal} module makes good use of record
syntax. Here's a type defined in that module:

~~~~ {#id585295 .programlisting}
data CalendarTime = CalendarTime {
  ctYear                      :: Int,
  ctMonth                     :: Month,
  ctDay, ctHour, ctMin, ctSec :: Int,
  ctPicosec                   :: Integer,
  ctWDay                      :: Day,
  ctYDay                      :: Int,
  ctTZName                    :: String,
  ctTZ                        :: Int,
  ctIsDST                     :: Bool
}
    
~~~~

In the absence of record syntax, it would be painful to extract specific
fields from a type like this. The notation makes it easier to work with
large structures.

Parameterised types
-------------------

We've repeatedly mentioned that the list type is polymorphic: the
elements of a list can be of any type. We can also add polymorphism to
our own types. To do this, we introduce type variables into a type
declaration. The Prelude defines a type named Maybe: we can use this to
represent a value that could be either present or missing, e.g. a field
in a database row that could be null.

~~~~ {#Nullable.hs:Nullable .programlisting}
-- file: ch03/Nullable.hs
data Maybe a = Just a
             | Nothing
~~~~

Here, the variable `a`{.varname} is not a regular variable: it's a type
variable. It indicates that the Maybe type takes another type as its
parameter. This lets us use Maybe on values of any type.

~~~~ {#Nullable.hs:wrappedTypes .programlisting}
-- file: ch03/Nullable.hs
someBool = Just True

someString = Just "something"
~~~~

As usual, we can experiment with this type in **ghci**.

~~~~ {#nullable.ghci:experiment .screen}
ghci> Just 1.5
Just 1.5
ghci> Nothing
Nothing
ghci> :type Just "invisible bike"
Just "invisible bike" :: Maybe [Char]
~~~~

Maybe is a polymorphic, or generic, type. We give the Maybe type
constructor a parameter to create a specific type, such as Maybe Int or
Maybe [Bool]. As we might expect, these types are distinct.

We can nest uses of parameterised types inside each other, but when we
do, we may need to use parentheses to tell the Haskell compiler how to
parse our expression.

~~~~ {#Nullable.hs:parens .programlisting}
-- file: ch03/Nullable.hs
wrapped = Just (Just "wrapped")
~~~~

To once again extend an analogy to more familiar languages,
parameterised types bear some resemblance to templates in C++, and to
generics in Java. Just be aware that this is a shallow analogy.
Templates and generics were added to their respective languages long
after the languages were initially defined, and have an awkward feel.
Haskell's parameterised types are simpler and easier to use, as the
language was designed with them from the beginning.

Recursive types
---------------

The familiar list type is *recursive*: it's defined in terms of itself.
To understand this, let's create our own list-like type. We'll use
`Cons`{.code} in place of the `(:)`{.function} constructor, and
`Nil`{.code} in place of `[]`{.code}.

~~~~ {#ListADT.hs:List .programlisting}
-- file: ch03/ListADT.hs
data List a = Cons a (List a)
            | Nil
              deriving (Show)
~~~~

Because List a appears on both the left and the right of the `=`{.code}
sign, the type's definition refers to itself. If we want to use the
`Cons`{.code} constructor to create a new value, we must supply one
value of type `a`{.varname}, and another of type `List a`{.varname}.
Let's see where this leads us in practice.

The simplest value of type List a that we can create is `Nil`{.code}.
Save the type definition in a file, then load it into **ghci**.

~~~~ {#listadt.ghci:empty .screen}
ghci> Nil
Nil
~~~~

Because `Nil`{.code} has a List type, we can use it as a parameter to
`Cons`{.code}.

~~~~ {#listadt.ghci:tiny .screen}
ghci> Cons 0 Nil
Cons 0 Nil
~~~~

And because `Cons 0 Nil`{.code} has the type List a, we can use this as
a parameter to `Cons`{.code}.

~~~~ {#listadt.ghci:two .screen}
ghci> Cons 1 it
Cons 1 (Cons 0 Nil)
ghci> Cons 2 it
Cons 2 (Cons 1 (Cons 0 Nil))
ghci> Cons 3 it
Cons 3 (Cons 2 (Cons 1 (Cons 0 Nil)))
~~~~

We could continue in this fashion indefinitely, creating ever longer
`Cons`{.code} chains, each with a single `Nil`{.code} at the end.

![[Tip]](/support/figs/tip.png)

Is List an acceptable list?

We can easily prove to ourselves that our List a type has the same shape
as the built-in list type [a]. To do this, we write a function that
takes any value of type [a], and produces a value of type List a.

~~~~ {#ListADT.hs:fromList .programlisting}
-- file: ch03/ListADT.hs
fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil
~~~~

By inspection, this clearly substitutes a `Cons`{.code} for every
`(:)`{.function}, and a `Nil`{.code} for each `[]`{.code}. This covers
both of the built-in list type's constructors. The two types are
*isomorphic*; they have the same shape.

~~~~ {#listadt.ghci:fromList .screen}
ghci> fromList "durian"
Cons 'd' (Cons 'u' (Cons 'r' (Cons 'i' (Cons 'a' (Cons 'n' Nil)))))
ghci> fromList [Just True, Nothing, Just False]
Cons (Just True) (Cons Nothing (Cons (Just False) Nil))
~~~~

For a third example of what a recursive type is, here is a definition of
a binary tree type.

~~~~ {#Tree.hs:Tree .programlisting}
-- file: ch03/Tree.hs
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)
~~~~

A binary tree is either a node with two children, which are themselves
binary trees, or an empty value.

This time, let's search for insight by comparing our definition with one
from a more familiar language. Here's a similar class definition in
Java.

~~~~ {#Tree.java:Tree .programlisting}
class Tree<A>
{
    A value;
    Tree<A> left;
    Tree<A> right;

    public Tree(A v, Tree<A> l, Tree<A> r)
    {
    value = v;
    left = l;
    right = r;
    }
}
~~~~

The one significant difference is that Java lets us use the special
value `null`{.code} anywhere to indicate “nothing”, so we can use
`null`{.code} to indicate that a node is missing a left or right child.
Here's a small function that constructs a tree with two leaves (a leaf,
by convention, has no children).

~~~~ {#Tree.java:Example .programlisting}
class Example 
{
    static Tree<String> simpleTree()
    {
    return new Tree<String>(
            "parent",
        new Tree<String>("left leaf", null, null),
        new Tree<String>("right leaf", null, null));
    }
}
~~~~

In Haskell, we don't have an equivalent of `null`{.code}. We could use
the Maybe type to provide a similar effect, but that bloats the pattern
matching. Instead, we've decided to use a no-argument `Empty`{.code}
constructor. Where the Java example provides `null`{.code} to the Tree
constructor, we supply `Empty`{.code} in Haskell.

~~~~ {#Tree.hs:simpleTree .programlisting}
-- file: ch03/Tree.hs
simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)
~~~~

### Exercises

**1.**

Write the converse of `fromList`{.function} for the List type: a
function that takes a List a and generates a [a].

**2.**

Define a tree type that has only one constructor, like our Java example.
Instead of the `Empty`{.code} constructor, use the Maybe type to refer
to a node's children.

Reporting errors
----------------

Haskell provides a standard function, `error :: String -> a`{.function},
that we can call when something has gone terribly wrong in our code. We
give it a string parameter, which is the error message to display. Its
type signature looks peculiar: how can it produce a value of any type
`a`{.varname} given only a string?

It has a result type of `a`{.varname} so that we can call it anywhere
and it will always have the right type. However, it does not return a
value like a normal function: instead, it *immediately aborts
evaluation*, and prints the error message we give it.

The `mySecond`{.function} function returns the second element of its
input list, but fails if its input list isn't long enough.

~~~~ {#MySecond.hs:mySecond .programlisting}
-- file: ch03/MySecond.hs
mySecond :: [a] -> a

mySecond xs = if null (tail xs)
              then error "list too short"
              else head (tail xs)
~~~~

As usual, we can see how this works in practice in **ghci**.

~~~~ {#error.ghci:mySecond .screen}
ghci> mySecond "xi"
'i'
ghci> mySecond [2]
*** Exception: list too short
ghci> head (mySecond [[9]])
*** Exception: list too short
~~~~

Notice the third case above, where we try to use the result of the call
to `mySecond`{.function} as the argument to another function. Evaluation
still terminates and drops us back to the **ghci** prompt. This is the
major weakness of using `error`{.function}: it doesn't let our caller
distinguish between a recoverable error and a problem so severe that it
really should terminate our program.

As we have already seen, a pattern matching failure causes a similar
unrecoverable error.

~~~~ {#error.ghci:myError .screen}
ghci> mySecond []
*** Exception: Prelude.tail: empty list
~~~~

### A more controlled approach

We can use the Maybe type to represent the possibility of an error.

If we want to indicate that an operation has failed, we can use the
`Nothing`{.function} constructor. Otherwise, we wrap our value with the
`Just`{.function} constructor.

Let's see how our `mySecond`{.function} function changes if we return a
Maybe value instead of calling `error`{.function}.

~~~~ {#MySecond.hs:safeSecond .programlisting}
-- file: ch03/MySecond.hs
safeSecond :: [a] -> Maybe a

safeSecond [] = Nothing
safeSecond xs = if null (tail xs)
                then Nothing
                else Just (head (tail xs))
~~~~

If the list we're passed is too short, we return `Nothing`{.code} to our
caller. This lets them decide what to do, where a call to
`error`{.function} would force a crash.

~~~~ {#error.ghci:safeSecond .screen}
ghci> safeSecond []
Nothing
ghci> safeSecond [1]
Nothing
ghci> safeSecond [1,2]
Just 2
ghci> safeSecond [1,2,3]
Just 2
~~~~

To return to an earlier topic, we can further improve the readability of
this function with pattern matching.

~~~~ {#MySecond.hs:tidySecond .programlisting}
-- file: ch03/MySecond.hs
tidySecond :: [a] -> Maybe a

tidySecond (_:x:_) = Just x
tidySecond _       = Nothing
~~~~

The first pattern only matches if the list is at least two elements long
(it contains two list constructors), and it binds the variable
`x`{.varname} to the list's second element. The second pattern is
matched if the first fails.

Introducing local variables
---------------------------

Within the body of a function, we can introduce new local variables
whenever we need them, using a `let`{.literal} expression. Here is a
simple function that determines whether we should lend some money to a
customer. We meet a money reserve of at least 100, we return our new
balance after subtracting the amount we have loaned.

~~~~ {#Lending.hs:lend .programlisting}
-- file: ch03/Lending.hs
lend amount balance = let reserve    = 100
                          newBalance = balance - amount
                      in if balance < reserve
                         then Nothing
                         else Just newBalance
~~~~

The keywords to look out for here are `let`{.literal}, which starts a
block of variable declarations, and `in`{.code}, which ends it. Each
line introduces a new variable. The name is on the left of the
`=`{.literal}, and the expression to which it is bound is on the right.

![[Note]](/support/figs/note.png)

Special notes

Let us re-emphasise our wording: a name in a `let`{.literal} block is
bound to an *expression*, not to a *value*. Because Haskell is a lazy
language, the expression associated with a name won't actually be
evaluated until it's needed. In the above example, we will not compute
the value of `newBalance`{.varname} if we do not meet our reserve.

When we define a variable in a `let`{.literal} block, we refer to it as
a *`let`{.literal}-bound* variable. This simply means what it says: we
have bound the variable in a `let`{.literal} block.

Also, our use of white space here is important. We'll talk in more
detail about the layout rules in [the section called “The offside rule
and white space in an
expression”](defining-types-streamlining-functions.html#deftypes.offside "The offside rule and white space in an expression").

We can use the names of a variable in a `let`{.literal} block both
within the block of declarations and in the expression that follows the
`in`{.code} keyword.

In general, we'll refer to the places within our code where we can use a
name as the name's *scope*. If we can use a name, it's *in scope*,
otherwise it's *out of scope*. If a name is visible throughout a source
file, we say it's at the *top level*.

### Shadowing

We can “nest” multiple `let`{.literal} blocks inside each other in an
expression.

~~~~ {#NestedLets.hs:foo .programlisting}
-- file: ch03/NestedLets.hs
foo = let a = 1
      in let b = 2
         in a + b
~~~~

It's perfectly legal, but not exactly wise, to repeat a variable name in
a nested `let`{.literal} expression.

~~~~ {#NestedLets.hs:bar .programlisting}
-- file: ch03/NestedLets.hs
bar = let x = 1
      in ((let x = "foo" in x), x)
~~~~

Here, the inner `x`{.varname} is hiding, or *shadowing*, the outer
`x`{.varname}. It has the same name, but a different type and value.

~~~~ {#nestedlets.ghci:bar .screen}
ghci> bar
("foo",1)
~~~~

We can also shadow a function's parameters, leading to even stranger
results. What is the type of this function?

~~~~ {#NestedLets.hs:quux .programlisting}
-- file: ch03/NestedLets.hs
quux a = let a = "foo"
         in a ++ "eek!"
~~~~

Because the function's argument `a`{.varname} is never used in the body
of the function, due to being shadowed by the `let`{.literal}-bound
`a`{.varname}, the argument can have any type at all.

~~~~ {#nestedlets.ghci:quux .screen}
ghci> :type quux
quux :: t -> [Char]
~~~~

![[Tip]](/support/figs/tip.png)

Compiler warnings are your friends

Shadowing can obviously lead to confusion and nasty bugs, so GHC has a
helpful `-fwarn-name-shadowing`{.option} option. When enabled, GHC will
print a warning message any time we shadow a name.

### The where clause

We can use another mechanism to introduce local variables: the
`where`{.literal} clause. The definitions in a `where`{.literal} clause
apply to the code that *precedes* it. Here's a similar function to
`lend`{.function}, using `where`{.literal} instead of `let`{.literal}.

~~~~ {#Lending.hs:lend2 .programlisting}
-- file: ch03/Lending.hs
lend2 amount balance = if amount < reserve * 0.5
                       then Just newBalance
                       else Nothing
    where reserve    = 100
          newBalance = balance - amount
~~~~

While a `where`{.literal} clause may initially seem weird, it offers a
wonderful aid to readability. It lets us direct our reader's focus to
the important details of an expression, with the supporting definitions
following afterwards. After a while, you may find yourself missing
`where`{.literal} clauses in languages that lack them.

As with `let`{.literal} expressions, white space is significant in
`where`{.literal} clauses. We will talk more about the layout rules
shortly, in [the section called “The offside rule and white space in an
expression”](defining-types-streamlining-functions.html#deftypes.offside "The offside rule and white space in an expression").

### Local functions, global variables

You'll have noticed that Haskell's syntax for defining a variable looks
very similar to its syntax for defining a function. This symmetry is
preserved in `let`{.literal} and `where`{.literal} blocks: we can define
local *functions* just as easily as local *variables*.

~~~~ {#LocalFunction.hs:pluralise .programlisting}
-- file: ch03/LocalFunction.hs
pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
    where plural 0 = "no " ++ word ++ "s"
          plural 1 = "one " ++ word
          plural n = show n ++ " " ++ word ++ "s"
~~~~

We have defined a local function, `plural`{.function}, that consists of
several equations. Local functions can freely use variables from the
scopes that enclose them: here, we use `word`{.varname} from the
definition of the outer function `pluralise`{.function}. In the
definition of `pluralise`{.function}, the `map`{.function} function
(which we'll be revisiting in the next chapter) applies the local
function `plural`{.function} to every element of the `counts`{.varname}
list.

We can also define variables, as well as functions, at the top level of
a source file.

~~~~ {#GlobalVariable.hs:itemName .programlisting}
-- file: ch03/GlobalVariable.hs
itemName = "Weighted Companion Cube"
~~~~

The offside rule and white space in an expression
-------------------------------------------------

In our definitions of `lend`{.function} and `lend2`{.function}, the left
margin of our text wandered around quite a bit. This was not an
accident: in Haskell, white space has meaning.

Haskell uses indentation as a cue to parse sections of code. This use of
layout to convey structure is sometimes called the *offside rule*. At
the beginning of a source file, the first top level declaration or
definition can start in any column, and the Haskell compiler or
interpreter remembers that indentation level. Every subsequent top level
declaration must have the same indentation.

Here's an illustration of the top level indentation rule. Our first
file, `GoodIndent.hs`{.filename}, is well behaved.

~~~~ {#GoodIndent.hs:good .programlisting}
-- file: ch03/GoodIndent.hs
-- This is the leftmost column.

  -- It's fine for top-level declarations to start in any column...
  firstGoodIndentation = 1

  -- ...provided all subsequent declarations do, too!
  secondGoodIndentation = 2
~~~~

Our second, `BadIndent.hs`{.filename}, doesn't play by the rules.

~~~~ {#BadIndent.hs:bad .programlisting}
-- file: ch03/BadIndent.hs
-- This is the leftmost column.

    -- Our first declaration is in column 4.
    firstBadIndentation = 1

  -- Our second is left of the first, which is illegal!
  secondBadIndentation = 2
~~~~

Here's what happens when we try to load the two files into **ghci**.

~~~~ {#indent.ghci:load .screen}
ghci> :load GoodIndent.hs
[1 of 1] Compiling Main             ( GoodIndent.hs, interpreted )
Ok, modules loaded: Main.
ghci> :load BadIndent.hs
[1 of 1] Compiling Main             ( BadIndent.hs, interpreted )

BadIndent.hs:8:2: parse error on input `secondBadIndentation'
Failed, modules loaded: none.
~~~~

An empty following line is treated as a continuation of the current
item, as is a following line indented further to the right.

The rules for `let`{.literal} expressions and `where`{.literal} clauses
are similar. After a `let`{.literal} or `where`{.literal} keyword, the
Haskell compiler or interpreter remembers the indentation of the next
token it sees. If the line that follows is empty, or its indentation is
further to the right, it is considered to continue the previous line. If
the indentation is the same as the start of the preceding item, this is
treated as beginning a new item in the same block.

~~~~ {#Indentation.hs:foo .programlisting}
-- file: ch03/Indentation.hs
foo = let firstDefinition = blah blah
          -- a comment-only line is treated as empty
                              continuation blah

          -- we reduce the indentation, so this is a new definition
          secondDefinition = yada yada

                             continuation yada
      in whatever
~~~~

Here are nested uses of `let`{.literal} and `where`{.literal}.

~~~~ {#letwhere.hs:let .programlisting}
-- file: ch03/letwhere.hs
bar = let b = 2
          c = True
      in let a = b
         in (a, c)
~~~~

The name `a`{.varname} is only visible within the inner `let`{.literal}
expression. It's not visible in the outer `let`{.literal}. If we try to
use the name `a`{.varname} there, we'll get a compilation error. The
indentation gives both us and the compiler a visual cue as to what is
currently in scope.

~~~~ {#letwhere.hs:where .programlisting}
-- file: ch03/letwhere.hs
foo = x
    where x = y
              where y = 2
~~~~

Similarly, the scope of the first `where`{.literal} clause is the
definition of `foo`{.varname}, but the scope of the second is just the
first `where`{.literal} clause.

The indentation we use for the `let`{.literal} and `where`{.literal}
clauses makes our intentions easy to figure out.

### A note about tabs versus spaces

If you use a Haskell-aware text editor (e.g. Emacs), it is probably
already configured to use space characters for all white space when you
edit Haskell source files. If your editor is *not* Haskell-aware, you
should configure it to only use space characters.

The reason for this is portability. In an editor that uses a fixed-width
font, tab stops are by convention placed at different intervals on
Unix-like systems (every eight characters) than on Windows (every four
characters). This means that no matter what your personal beliefs are
about where tabs belong, you can't rely on someone else's editor
honouring your preferences. Any indentation that uses tabs is going to
look broken under *someone's* configuration. In fact, this could lead to
compilation problems, as the Haskell language standard requires
implementations to use the Unix tab width convention. Using space
characters avoids this problem entirely.

### The offside rule is not mandatory

We can use explicit structuring instead of layout to indicate what we
mean. To do so, we start a block of equations with an opening curly
brace; separate each item with a semicolon; and finish the block with a
closing curly brace. The following two uses of `let`{.literal} have the
same meanings.

~~~~ {#Braces.hs:braces .programlisting}
-- file: ch03/Braces.hs
bar = let a = 1
          b = 2
          c = 3
      in a + b + c

foo = let { a = 1;  b = 2;
        c = 3 }
      in a + b + c
~~~~

When we use explicit structuring, the normal layout rules don't apply,
which is why we can get away with unusual indentation in the second
`let`{.literal} expression.

We can use explicit structuring anywhere that we'd normally use layout.
It's valid for `where`{.literal} clauses, and even top-level
declarations. Just remember that although the facility exists, explicit
structuring is hardly ever actually *used* in Haskell programs.

The case expression
-------------------

Function definitions are not the only place where we can use pattern
matching. The `case`{.literal} construct lets us match patterns within
an expression. Here's what it looks like. This function (defined for us
in `Data.Maybe`{.code}) unwraps a Maybe value, using a default if the
value is `Nothing`{.code}.

~~~~ {#Guard.hs:fromMaybe .programlisting}
-- file: ch03/Guard.hs
fromMaybe defval wrapped =
    case wrapped of
      Nothing     -> defval
      Just value  -> value
~~~~

The `case`{.code} keyword is followed by an arbitrary expression: the
pattern match is performed against the result of this expression. The
`of`{.code} keyword signifies the end of the expression and the
beginning of the block of patterns and expressions.

Each item in the block consists of a pattern, followed by an arrow
`->`{.code}, followed by an expression to evaluate if that pattern
matches. These expressions must all have the same type. The result of
the `case`{.code} expression is the result of the expression associated
with the first pattern to match. Matches are attempted from top to
bottom.

To express “here's the expression to evaluate if none of the other
patterns match”, we just use the wild card pattern `_`{.code} as the
last in our list of patterns. If a pattern match fails, we will get the
same kind of runtime error as we saw earlier.

Common beginner mistakes with patterns
--------------------------------------

There are a few ways in which new Haskell programmers can misunderstand
or misuse patterns. Here are some attempts at pattern matching gone
awry. Depending on what you expect one of these examples to do, it might
contain a surprise.

### Incorrectly matching against a variable

~~~~ {#BogusPattern.hs:whichFruit .programlisting}
-- file: ch03/BogusPattern.hs
data Fruit = Apple | Orange

apple = "apple"

orange = "orange"        

whichFruit :: String -> Fruit

whichFruit f = case f of
                 apple  -> Apple
                 orange -> Orange
~~~~

A naive glance suggests that this code is trying to check the value
`f`{.function} to see whether it matches the value `apple`{.varname} or
`orange`{.varname}.

It is easier to spot the mistake if we rewrite the code in an equational
style.

~~~~ {#BogusPattern.hs:equational .programlisting}
-- file: ch03/BogusPattern.hs
equational apple = Apple
equational orange = Orange
~~~~

Now can you see the problem? Here, it is more obvious `apple`{.varname}
does not refer to the top level value named `apple`{.varname}: it is a
local pattern variable.

![[Note]](/support/figs/note.png)

Irrefutable patterns

We refer to a pattern that always succeeds as *irrefutable*. Plain
variable names and the wild card `_`{.code} are examples of irrefutable
patterns.

Here's a corrected version of this function.

~~~~ {#BogusPattern.hs:betterFruit .programlisting}
-- file: ch03/BogusPattern.hs
betterFruit f = case f of
                  "apple"  -> Apple
                  "orange" -> Orange
~~~~

We fixed the problem by matching against the literal values
`"apple"`{.code} and `"orange"`{.code}.

### Incorrectly trying to compare for equality

What if we want to compare the values stored in two nodes of type Tree,
and return one of them if they're equal? Here's an attempt.

~~~~ {#BadTree.hs:bad_nodesAreSame .programlisting}
-- file: ch03/BadTree.hs
bad_nodesAreSame (Node a _ _) (Node a _ _) = Just a
bad_nodesAreSame _            _            = Nothing
~~~~

A name can only appear once in a set of pattern bindings. We cannot
place a variable in multiple positions to express the notion “this value
and that should be identical”. Instead, we'll solve this problem using
*guards*, another invaluable Haskell feature.

Conditional evaluation with guards
----------------------------------

Pattern matching limites us to performing fixed tests of a value's
shape. Although this is useful, we will often want to make a more
expressive check before evaluating a function's body. Haskell provides a
feature, *guards*, that give us this ability. We'll introduce the idea
with a modification of the function we wrote to compare two nodes of a
tree.

~~~~ {#BadTree.hs:nodesAreSame .programlisting}
-- file: ch03/BadTree.hs
nodesAreSame (Node a _ _) (Node b _ _)
    | a == b     = Just a
nodesAreSame _ _ = Nothing
~~~~

In this example, we use pattern matching to ensure that we are looking
at values of the right shape, and a guard to compare pieces of them.

A pattern can be followed by zero or more guards, each an expression of
type Bool. A guard is introduced by a `|`{.code} symbol. This is
followed by the guard expression, then an `=`{.code} symbol (or
`->`{.code} if we're in a `case`{.literal} expression), then the body to
use if the guard expression evaluates to `True`{.code}. If a pattern
matches, each guard associated with that pattern is evaluated, in the
order in which they are written. If a guard succeeds, the body
affiliated with it is used as the result of the function. If no guard
succeeds, pattern matching moves on to the next pattern.

When a guard expression is evaluated, all of the variables mentioned in
the pattern with which it is associated are bound and can be used.

Here is a reworked version of our `lend`{.function} function that uses
guards.

~~~~ {#Lending.hs:lend3 .programlisting}
-- file: ch03/Lending.hs
lend3 amount balance
     | amount <= 0            = Nothing
     | amount > reserve * 0.5 = Nothing
     | otherwise              = Just newBalance
    where reserve    = 100
          newBalance = balance - amount
~~~~

The special-looking guard expression `otherwise`{.varname} is simply a
variable bound to the value `True`{.code}, to aid readability.

We can use guards anywhere that we can use patterns. Writing a function
as a series of equations using pattern matching and guards can make it
much clearer. Remember the `myDrop`{.function} function we defined in
[the section called “Conditional
evaluation”](types-and-functions.html#funcstypes.if "Conditional evaluation")?

~~~~ {#myDrop.hs:myDrop1 .programlisting}
-- file: ch02/myDrop.hs
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)
~~~~

Here is a reformulation that uses patterns and guards.

~~~~ {#myDrop.hs:niceDrop .programlisting}
-- file: ch02/myDrop.hs
niceDrop n xs | n <= 0 = xs
niceDrop _ []          = []
niceDrop n (_:xs)      = niceDrop (n - 1) xs
~~~~

This change in style lets us enumerate *up front* the cases in which we
expect a function to behave differently. If we bury the decisions inside
a function as `if`{.literal} expressions, the code becomes harder to
read.

Exercises
---------

**1.**

Write a function that computes the number of elements in a list. To test
it, ensure that it gives the same answers as the standard
`length`{.function} function.

**2.**

Add a type signature for your function to your source file. To test it,
load the source file into **ghci** again.

**3.**

Write a function that computes the mean of a list, i.e. the sum of all
elements in the list divided by its length. (You may need to use the
`fromIntegral`{.function} function to convert the length of the list
from an integer into a floating point number.)

**4.**

Turn a list into a palindrome, i.e. it should read the same both
backwards and forwards. For example, given the list `[1,2,3]`{.code},
your function should return `[1,2,3,3,2,1]`{.code}.

**5.**

Write a function that determines whether its input list is a palindrome.

**6.**

Create a function that sorts a list of lists based on the length of each
sublist. (You may want to look at the `sortBy`{.function} function from
the `Data.List`{.code} module.)

**7.**

Define a function that joins a list of lists together using a separator
value.

~~~~ {#Intersperse.hs:intersperse .programlisting}
-- file: ch03/Intersperse.hs
intersperse :: a -> [[a]] -> [a]
~~~~

The separator should appear between elements of the list, but should not
follow the last element. Your function should behave as follows.

~~~~ {#intersperse.ghci:intersperse .screen}
ghci> :load Intersperse
[1 of 1] Compiling Main             ( Intersperse.hs, interpreted )
Ok, modules loaded: Main.
ghci> intersperse ',' []
""
ghci> intersperse ',' ["foo"]
"foo"
ghci> intersperse ',' ["foo","bar","baz","quux"]
"foo,bar,baz,quux"
~~~~

**8.**

Using the binary tree type that we defined earlier in this chapter,
write a function that will determine the height of the tree. The height
is the largest number of hops from the root to an `Empty`{.code}. For
example, the tree `Empty`{.code} has height zero;
`Node           "x" Empty Empty`{.code} has height one;
`Node "x"           Empty (Node "y" Empty Empty)`{.code} has height two;
and so on.

**9.**

Consider three two-dimensional points *a*, *b*, and *c*. If we look at
the angle formed by the line segment from *a* to *b* and the line
segment from *b* to *c*, it either turns left, turns right, or forms a
straight line. Define a Direction data type that lets you represent
these possibilities.

**10.**

Write a function that calculates the turn made by three 2D points and
returns a Direction.

**11.**

Define a function that takes a list of 2D points and computes the
direction of each successive triple. Given a list of points
`[a,b,c,d,e]`{.code}, it should begin by computing the turn made by
`[a,b,c]`{.code}, then the turn made by `[b,c,d]`{.code}, then
`[c,d,e]`{.code}. Your function should return a list of Direction.

**12.**

Using the code from the preceding three exercises, implement Graham's
scan algorithm for the convex hull of a set of 2D points. You can find
good description of what a [convex
hull](http://en.wikipedia.org/wiki/Convex_hull). is, and how the [Graham
scan algorithm](http://en.wikipedia.org/wiki/Graham_scan) should work,
on [Wikipedia](http://en.wikipedia.org/).

\

* * * * *

^[[7](#id582956)]^If you are familiar with C or C++, it is analogous to
a `typedef`{.code}.

![image](/support/figs/rss.png) Want to stay up to date? Subscribe to
the comment feed for [this chapter](/feeds/comments/), or the [entire
book](/feeds/comments/).

Copyright 2007, 2008 Bryan O'Sullivan, Don Stewart, and John Goerzen.
This work is licensed under a [Creative Commons
Attribution-Noncommercial 3.0
License](http://creativecommons.org/licenses/by-nc/3.0/). Icons by [Paul
Davey](mailto:mattahan@gmail.com) aka
[Mattahan](http://mattahan.deviantart.com/).

  ---------------------------------- -------------------- -------------------------------------
  [Prev](types-and-functions.html)                        [Next](functional-programming.html)
  Chapter 2. Types and Functions     [Home](index.html)   Chapter 4. Functional programming
  ---------------------------------- -------------------- -------------------------------------


