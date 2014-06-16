[Real World Haskell](/)by Bryan O'Sullivan, Don Stewart, and John Goerzen
-------------------------------------------------------------------------

Appendix B. Characters, strings, and escaping rules

[Prev](installing-ghc-and-haskell-libraries.html)

[Next](web-site-and-comment-system-usage-and-policies.html)

Appendix B. Characters, strings, and escaping rules
---------------------------------------------------

**Table of Contents**

[Writing character and string
literals](characters-strings-and-escaping-rules.html#escapes.char)

[International language
support](characters-strings-and-escaping-rules.html#escapes.intl)

[Escaping
text](characters-strings-and-escaping-rules.html#escapes.escape)

[Single-character escape
codes](characters-strings-and-escaping-rules.html#id689632)

[Multiline string
literals](characters-strings-and-escaping-rules.html#id689881)

[ASCII control
codes](characters-strings-and-escaping-rules.html#id689903)

[Control-with-character
escapes](characters-strings-and-escaping-rules.html#id690525)

[Numeric escapes](characters-strings-and-escaping-rules.html#id690711)

[The zero-width escape
sequence](characters-strings-and-escaping-rules.html#id690781)

This appendix covers the escaping rules used to represent non-ASCII
characters in Haskell character and string literals. Haskell's escaping
rules follow the pattern established by the C programming language, but
expand considerably upon them.

Writing character and string literals
-------------------------------------

A single character is surrounded by ASCII single quotes, `'`{.literal},
and has type Char.

~~~~ {#text.ghci:char .screen}
ghci> 'c'
'c'
ghci> :type 'c'
'c' :: Char
~~~~

A string literal is surrounded by double quotes, `"`{.literal}, and has
type [Char] (more often written as String).

~~~~ {#text.ghci:string .screen}
ghci> "a string literal"
"a string literal"
ghci> :type "a string literal"
"a string literal" :: [Char]
~~~~

The double-quoted form of a string literal is just syntactic sugar for
list notation.

~~~~ {#text.ghci:stringlist .screen}
ghci> ['a', ' ', 's', 't', 'r', 'i', 'n', 'g'] == "a string"
True
~~~~

International language support
------------------------------

Haskell uses Unicode internally for its Char data type. Since String is
just an alias for [Char], a list of Chars, Unicode is also used to
represent strings.

Different Haskell implementations place limitations on the character
sets they can accept in source files. GHC allows source files to be
written in the UTF-8 encoding of Unicode, so in a source file, you can
use UTF-8 literals inside a character or string constant. Do be aware
that if you use UTF-8, other Haskell implementations may not be able to
parse your source files.

When you run the **ghci** interpreter interactively, it may not be able
to deal with international characters in character or string literals
that you enter at the keyboard.

![[Note]](/support/figs/note.png)

Note

Although Haskell represents characters and strings internally using
Unicode, there is no standardised way to do I/O on files that contain
Unicode data. Haskell's standard text I/O functions treat text as a
sequence of 8-bit characters, and do not perform any character set
conversion.

There exist third-party libraries that will convert between the many
different encodings used in files and Haskell's internal Unicode
representation.

Escaping text
-------------

Some characters must be escaped to be represented inside a character or
string literal. For example, a double quote character inside a string
literal must be escaped, or else it will be treated as the end of the
string.

### Single-character escape codes

Haskell uses essentially the same single-character escapes as the C
language and many other popular languages.

**Table B.1. Single-character escape codes**

Escape

Unicode

Character

`\0`{.literal}

U+0000

null character

`\a`{.literal}

U+0007

alert

`\b`{.literal}

U+0008

backspace

`\f`{.literal}

U+000C

form feed

`\n`{.literal}

U+000A

newline (line feed)

`\r`{.literal}

U+000D

carriage return

`\t`{.literal}

U+0009

horizontal tab

`\v`{.literal}

U+000B

vertical tab

`\"`{.literal}

U+0022

double quote

`\&`{.literal}

*n/a*

empty string

`\'`{.literal}

U+0027

single quote

`\\`{.literal}

U+005C

backslash

\

### Multiline string literals

To write a string literal that spans multiple lines, terminate one line
with a backslash, and resume the string with another backslash. An
arbitrary amount of whitespace (of any kind) can fill the gap between
the two backslashes.

~~~~ {#id689896 .programlisting}
"this is a \
    \long string,\
    \ spanning multiple lines"
~~~~

### ASCII control codes

Haskell recognises the escaped use of the standard two- and three-letter
abbreviations of ASCII control codes.

**Table B.2. ASCII control code abbreviations**

Escape

Unicode

Meaning

`\NUL`{.literal}

U+0000

null character

`\SOH`{.literal}

U+0001

start of heading

`\STX`{.literal}

U+0002

start of text

`\ETX`{.literal}

U+0003

end of text

`\EOT`{.literal}

U+0004

end of transmission

`\ENQ`{.literal}

U+0005

enquiry

`\ACK`{.literal}

U+0006

acknowledge

`\BEL`{.literal}

U+0007

bell

`\BS`{.literal}

U+0008

backspace

`\HT`{.literal}

U+0009

horizontal tab

`\LF`{.literal}

U+000A

line feed (newline)

`\VT`{.literal}

U+000B

vertical tab

`\FF`{.literal}

U+000C

form feed

`\CR`{.literal}

U+000D

carriage return

`\SO`{.literal}

U+000E

shift out

`\SI`{.literal}

U+000F

shift in

`\DLE`{.literal}

U+0010

data link escape

`\DC1`{.literal}

U+0011

device control 1

`\DC2`{.literal}

U+0012

device control 2

`\DC3`{.literal}

U+0013

device control 3

`\DC4`{.literal}

U+0014

device control 4

`\NAK`{.literal}

U+0015

negative acknowledge

`\SYN`{.literal}

U+0016

synchronous idle

`\ETB`{.literal}

U+0017

end of transmission block

`\CAN`{.literal}

U+0018

cancel

`\EM`{.literal}

U+0019

end of medium

`\SUB`{.literal}

U+001A

substitute

`\ESC`{.literal}

U+001B

escape

`\FS`{.literal}

U+001C

file separator

`\GS`{.literal}

U+001D

group separator

`\RS`{.literal}

U+001E

record separator

`\US`{.literal}

U+001F

unit separator

`\SP`{.literal}

U+0020

space

`\DEL`{.literal}

U+007F

delete

\

### Control-with-character escapes

Haskell recognises an alternate notation for control characters, which
represents the archaic effect of pressing the **control** key on a
keyboard and chording it with another key. These sequences begin with
the characters `\^`{.literal}, followed by a symbol or uppercase letter.

**Table B.3. Control-with-character escapes**

Escape

Unicode

Meaning

`\^@`{.literal}

U+0000

null character

`\^A`{.literal} through `\^Z`{.literal}

U+0001 through U+001A

control codes

`\^[`{.literal}

U+001B

escape

`\^\`{.literal}

U+001C

file separator

`\^]`{.literal}

U+001D

group separator

`\^^`{.literal}

U+001E

record separator

`\^_`{.literal}

U+001F

unit separator

\

### Numeric escapes

Haskell allows Unicode characters to be written using numeric escapes. A
decimal character begins with a digit, e.g. `\1234`{.literal}. A
hexadecimal character begins with an `x`{.literal}, e.g.
`\xbeef`{.literal}. An octal character begins with an `o`{.literal},
e.g. `\o1234`{.literal}.

The maximum value of a numeric literal is `\1114111`{.literal}, which
may also be written `\x10ffff`{.literal} or `\o4177777`{.literal}.

### The zero-width escape sequence

String literals can contain a zero-width escape sequence, written
`\&`{.literal}. This is not a real character, as it represents the empty
string.

~~~~ {#text.ghci:empty .screen}
ghci> "\&"
""
ghci> "foo\&bar"
"foobar"
~~~~

The purpose of this escape sequence is to make it possible to write a
numeric escape followed immediately by a regular ASCII digit.

~~~~ {#text.ghci:empty.example .screen}
ghci> "\130\&11"
"\130\&11"
~~~~

Because the empty escape sequence represents an empty string, it is not
legal in a character literal.

![image](/support/figs/rss.png) Want to stay up to date? Subscribe to
the comment feed for [this chapter](/feeds/comments/), or the [entire
book](/feeds/comments/).

Copyright 2007, 2008 Bryan O'Sullivan, Don Stewart, and John Goerzen.
This work is licensed under a [Creative Commons
Attribution-Noncommercial 3.0
License](http://creativecommons.org/licenses/by-nc/3.0/). Icons by [Paul
Davey](mailto:mattahan@gmail.com) aka
[Mattahan](http://mattahan.deviantart.com/).

  --------------------------------------------------- -------------------- -------------------------------------------------------------
  [Prev](installing-ghc-and-haskell-libraries.html)                        [Next](web-site-and-comment-system-usage-and-policies.html)
  Appendix A. Installing GHC and Haskell libraries    [Home](index.html)   Appendix C. Web site and comment system usage and policies
  --------------------------------------------------- -------------------- -------------------------------------------------------------


