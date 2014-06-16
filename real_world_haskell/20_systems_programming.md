[Real World Haskell](/)by Bryan O'Sullivan, Don Stewart, and John Goerzen
-------------------------------------------------------------------------

Chapter 20. Systems Programming in Haskell

[Prev](error-handling.html)

[Next](using-databases.html)

Chapter 20. Systems Programming in Haskell
------------------------------------------

**Table of Contents**

[Running External
Programs](systems-programming-in-haskell.html#systems.running)

[Directory and File
Information](systems-programming-in-haskell.html#systems.directories)

[Program
Termination](systems-programming-in-haskell.html#system.termination)

[Dates and Times](systems-programming-in-haskell.html#systems.datetime)

[ClockTime and
CalendarTime](systems-programming-in-haskell.html#systems.datetime.types)

[Using ClockTime](systems-programming-in-haskell.html#systems.clocktime)

[Using
CalendarTime](systems-programming-in-haskell.html#system.calendartime)

[TimeDiff for
ClockTime](systems-programming-in-haskell.html#system.timediff)

[File Modification
Times](systems-programming-in-haskell.html#systems.modtime)

[Extended Example:
Piping](systems-programming-in-haskell.html#systems.piping)

[Using Pipes for
Redirection](systems-programming-in-haskell.html#systems.piping.theory)

[Better Piping](systems-programming-in-haskell.html#piping.extended)

[Final Words on
Pipes](systems-programming-in-haskell.html#systems.piping.finalwords)

So far, we've been talking mostly about high-level concepts. Haskell can
also be used for lower-level systems programming. It is quite possible
to write programs that interface with the operating system at a low
level using Haskell.

In this chapter, we are going to attempt something ambitious: a
Perl-like "language" that is valid Haskell, implemented in pure Haskell,
that makes shell scripting easy. We are going to implement piping, easy
command invocation, and some simple tools to handle tasks that might
otherwise be performed with `grep`{.literal} or `sed`{.literal}.

Specialized modules exist for different operating systems. In this
chapter, we will use generic OS-independent modules as much as possible.
However, we will be focusing on the POSIX environment for much of the
chapter. POSIX is a standard for Unix-like operating systems such as
Linux, FreeBSD, MacOS X, or Solaris. Windows does not support POSIX by
default, but the Cygwin environment provides a POSIX compatibility layer
for Windows.

Running External Programs
-------------------------

It is possible to invoke external commands from Haskell. To do that, we
suggest using `rawSystem`{.literal} from the `System.Cmd`{.literal}
module. This will invoke a specified program, with the specified
arguments, and return the exit code from that program. You can play with
it in **ghci**:

~~~~ {#rawSystem.ghci:s1 .screen}
ghci> :module System.Cmd
ghci> rawSystem "ls" ["-l", "/usr"]
Loading package old-locale-1.0.0.0 ... linking ... done.
Loading package old-time-1.0.0.0 ... linking ... done.
Loading package filepath-1.1.0.0 ... linking ... done.
Loading package directory-1.0.0.0 ... linking ... done.
Loading package unix-2.3.0.0 ... linking ... done.
Loading package process-1.0.0.0 ... linking ... done.
total 124
drwxr-xr-x   2 root root  49152 2008-08-18 11:04 bin
drwxr-xr-x   2 root root   4096 2008-03-09 05:53 games
drwxr-sr-x  10 jimb guile  4096 2006-02-04 09:13 guile
drwxr-xr-x  47 root root   8192 2008-08-08 08:18 include
drwxr-xr-x 107 root root  32768 2008-08-18 11:04 lib
lrwxrwxrwx   1 root root      3 2007-09-24 16:55 lib64 -> lib
drwxrwsr-x  17 root staff  4096 2008-06-24 17:35 local
drwxr-xr-x   2 root root   8192 2008-08-18 11:03 sbin
drwxr-xr-x 181 root root   8192 2008-08-12 10:11 share
drwxrwsr-x   2 root src    4096 2007-04-10 16:28 src
drwxr-xr-x   3 root root   4096 2008-07-04 19:03 X11R6
ExitSuccess
~~~~

Here, we run the equivalent of the shell command `ls -l /usr`{.literal}.
`rawSystem`{.literal} does not parse arguments from a string or expand
wildcards.^[[43](#ftn.id664371)]^ Instead, it expects every argument to
be contained in a list. If you don't want to pass any arguments, you can
simply pass an empty list like this:

~~~~ {#rawSystem.ghci:s2 .screen}
ghci> rawSystem "ls" []
calendartime.ghci  modtime.ghci    rp.ghci    RunProcessSimple.hs
cmd.ghci       posixtime.hs    rps.ghci   timediff.ghci
dir.ghci       rawSystem.ghci  RunProcess.hs  time.ghci
ExitSuccess
~~~~

Directory and File Information
------------------------------

The `System.Directory`{.literal} module contains quite a few functions
that can be used to obtain information from the filesystem. You can get
a list of files in a directory, rename or delete files, copy files,
change the current working directory, or create new directories.
`System.Directory`{.literal} is portable and works on any platform where
GHC itself works.

The [library reference for
`System.Directory`{.literal}](http://www.haskell.org/ghc/docs/latest/html/libraries/base/System-Directory.html)
provides a comprehensive list of the functions available. Let's use
**ghci** to demonstrate a few of them. Most of these functions are
straightforward equivalents to C library calls or shell commands.

~~~~ {#dir.ghci:setdir .screen}
ghci> :module System.Directory
ghci> setCurrentDirectory "/etc"
Loading package old-locale-1.0.0.0 ... linking ... done.
Loading package old-time-1.0.0.0 ... linking ... done.
Loading package filepath-1.1.0.0 ... linking ... done.
Loading package directory-1.0.0.0 ... linking ... done.
ghci> getCurrentDirectory
"/etc"
ghci> setCurrentDirectory ".."
ghci> getCurrentDirectory
"/"
~~~~

Here we saw commands to change the current working directory and obtain
the current working directory from the system. These are similar to the
`cd`{.literal} and `pwd`{.literal} commands in the POSIX shell.

~~~~ {#dir.ghci:contents .screen}
ghci> getDirectoryContents "/"
[".","..","lost+found","boot","etc","media","initrd.img","var","usr","bin","dev","home","lib","mnt","proc","root","sbin","tmp","sys","lib64","srv","opt","initrd","vmlinuz",".rnd","www","ultra60","emul",".fonts.cache-1","selinux","razor-agent.log",".svn","initrd.img.old","vmlinuz.old","ugid-survey.bulkdata","ugid-survey.brief"]
~~~~

`getDirectoryContents`{.literal} returns a list for every item in a
given directory. Note that on POSIX systems, this list normally includes
the special values `"."`{.literal} and `".."`{.literal}. You will
usually want to filter these out when processing the content of the
directory, perhaps like this:

~~~~ {#dir.ghci:contents2 .screen}
ghci> getDirectoryContents "/" >>= return . filter (`notElem` [".", ".."])
["lost+found","boot","etc","media","initrd.img","var","usr","bin","dev","home","lib","mnt","proc","root","sbin","tmp","sys","lib64","srv","opt","initrd","vmlinuz",".rnd","www","ultra60","emul",".fonts.cache-1","selinux","razor-agent.log",".svn","initrd.img.old","vmlinuz.old","ugid-survey.bulkdata","ugid-survey.brief"]
~~~~

![[Tip]](/support/figs/tip.png)

Tip

For a more detailed discussion of filtering the results of
`getDirectoryContents`{.literal}, refer to [Chapter 8, *Efficient file
processing, regular expressions, and file name
matching*](efficient-file-processing-regular-expressions-and-file-name-matching.html "Chapter 8. Efficient file processing, regular expressions, and file name matching").

Is the `` filter (`notElem` [".", ".."]) ``{.literal} part confusing?
That could got also be written as
`filter         (\c -> not $ elem c [".", ".."])`{.literal}. The
backticks in this case effectively let us pass the second argument to
`notElem`{.literal}; see [the section called “Infix
functions”](functional-programming.html#fp.infix "Infix functions") for
more information on backticks.

You can also query the system about the location of certain directories.
This query will ask the underlying operating system for the information.

~~~~ {#dir.ghci:query .screen}
ghci> getHomeDirectory
"/home/bos"
ghci> getAppUserDataDirectory "myApp"
"/home/bos/.myApp"
ghci> getUserDocumentsDirectory
"/home/bos"
~~~~

Program Termination
-------------------

Developers often write individual programs to accomplish particular
tasks. These individual parts may be combined to accomplish larger
tasks. A shell script or another program may execute them. The calling
script often needs a way to discover whether the program was able to
complete its task successfully. Haskell automatically indicates a
non-successful exit whenever a program is aborted by an exception.

However, you may need more fine-grained control over the exit code than
that. Perhaps you need to return different codes for different types of
errors. The `System.Exit`{.literal} module provides a way to exit the
program and return a specific exit status code to the caller. You can
call `exitWith ExitSuccess`{.literal} to return a code indicating a
successful termination (0 on POSIX systems). Or, you can call something
like `exitWith (ExitFailure 5)`{.literal}, which will return code 5 to
the calling program.

Dates and Times
---------------

Everything from file timestamps to business transactions involve dates
and times. Haskell provides ways for manipulating dates and times, as
well as features for obtaining date and time information from the
system.

### ClockTime and CalendarTime

In Haskell, the `System.Time`{.literal} module is primarily responsible
for date and time handling. It defines two types: `ClockTime`{.literal}
and `CalendarTime`{.literal}.

`ClockTime`{.literal} is the Haskell version of the traditional POSIX
epoch. A `ClockTime`{.literal} represents a time relative to midnight
the morning of January 1, 1970, UTC. A negative `ClockTime`{.literal}
represents a number of seconds prior to that date, while a positive
number represents a count of seconds after it.

`ClockTime`{.literal} is convenient for computations. Since it tracks
Coordinated Universal Time (UTC), it doesn't have to adjust for local
timezones, daylight saving time, or other special cases in time
handling. Every day is exactly (60 \* 60 \* 24) or 86,400
seconds^[[44](#ftn.id664881)]^, which makes time interval calculations
simple. You can, for instance, check the `ClockTime`{.literal} at the
start of a long task, again at the end, and simply subtract the start
time from the end time to determine how much time elapsed. You can then
divide by 3600 and display the elapsed time as a count of hours if you
wish.

`ClockTime`{.literal} is ideal for answering questions such as these:

-   How much time has elapsed?

-   What will be the `ClockTime`{.literal} 14 days ahead of this precise
    instant?

-   When was the file last modified?

-   What is the precise time right now?

These are good uses of `ClockTime`{.literal} because they refer to
precise, unambiguous moments in time. However, `ClockTime`{.literal} is
not as easily used for questions such as:

-   Is today Monday?

-   What day of the week will May 1 fall on next year?

-   What is the current time in my local timezone, taking the potential
    presence of Daylight Saving Time (DST) into account?

`CalendarTime`{.literal} stores a time the way humans do: with a year,
month, day, hour, minute, second, timezone, and DST information. It's
easy to convert this into a conveniently-displayable string, or to
answer questions about the local time.

You can convert between `ClockTime`{.literal} and
`CalendarTime`{.literal} at will. Haskell includes functions to convert
a `ClockTime`{.literal} to a `CalendarTime`{.literal} in the local
timezone, or to a `CalendarTime`{.literal} representing UTC.

#### Using ClockTime

`ClockTime`{.literal} is defined in `System.Time`{.literal} like this:

~~~~ {#id665093 .programlisting}
data ClockTime = TOD Integer Integer
        
~~~~

The first `Integer`{.literal} represents the number of seconds since the
epoch. The second `Integer`{.literal} represents an additional number of
picoseconds. Because `ClockTime`{.literal} in Haskell uses the unbounded
`Integer`{.literal} type, it can effectively represent a date range
limited only by computational resources.

Let's look at some ways to use `ClockTime`{.literal}. First, there is
the `getClockTime`{.literal} function that returns the current time
according to the system's clock.

~~~~ {#time.ghci:getClockTime .screen}
ghci> :module System.Time
ghci> getClockTime
Loading package old-locale-1.0.0.0 ... linking ... done.
Loading package old-time-1.0.0.0 ... linking ... done.
Mon Aug 18 12:10:38 CDT 2008
~~~~

If you wait a second and run `getClockTime`{.literal} again, you'll see
it returning an updated time. Notice that the output from this command
was a nice-looking string, complete with day-of-week information. That's
due to the `Show`{.literal} instance for `ClockTime`{.literal}. Let's
look at the `ClockTime`{.literal} at a lower level:

~~~~ {#time.ghci:ClockTime2 .screen}
ghci> TOD 1000 0
Wed Dec 31 18:16:40 CST 1969
ghci> getClockTime >>= (\(TOD sec _) -> return sec)
1219079438
~~~~

Here we first construct a `ClockTime`{.literal} representing the point
in time 1000 seconds after midnight on January 1, 1970, UTC. That moment
in time is known as the *epoch*. Depending on your timezone, this moment
in time may correspond to the evening of December 31, 1969, in your
local timezone.

The second example shows us pulling the number of seconds out of the
value returned by `getClockTime`{.literal}. We can now manipulate it,
like so:

~~~~ {#time.ghci:ClockTime3 .screen}
ghci> getClockTime >>= (\(TOD sec _) -> return (TOD (sec + 86400) 0))
Tue Aug 19 12:10:38 CDT 2008
~~~~

This will display what the time will be exactly 24 hours from now in
your local timezone, since there are 86,400 seconds in 24 hours.

#### Using CalendarTime

As its name implies, `CalendarTime`{.literal} represents time like we
would on a calendar. It has fields for information such as year, month,
and day. `CalendarTime`{.literal} and its associated types are defined
like this:

~~~~ {#id665339 .programlisting}
data CalendarTime = CalendarTime
   {ctYear :: Int,         -- Year (post-Gregorian)
    ctMonth :: Month, 
    ctDay :: Int,          -- Day of the month (1 to 31)
    ctHour :: Int,         -- Hour of the day (0 to 23)
    ctMin :: Int,          -- Minutes (0 to 59)
    ctSec :: Int,          -- Seconds (0 to 61, allowing for leap seconds)
    ctPicosec :: Integer,  -- Picoseconds
    ctWDay :: Day,         -- Day of the week
    ctYDay :: Int,         -- Day of the year (0 to 364 or 365)
    ctTZName :: String,    -- Name of timezone
    ctTZ :: Int,           -- Variation from UTC in seconds
    ctIsDST :: Bool        -- True if Daylight Saving Time in effect
   }

data Month = January | February | March | April | May | June 
             | July | August | September | October | November | December

data Day = Sunday | Monday | Tuesday | Wednesday
           | Thursday | Friday | Saturday
        
~~~~

There are a few things about these structures that should be
highlighted:

-   `ctWDay`{.literal}, `ctYDay`{.literal}, and `ctTZName`{.literal} are
    generated by the library functions that create a
    `CalendarTime`{.literal}, but are not used in calculations. If you
    are creating a `CalendarTime`{.literal} by hand, it is not necessary
    to put accurate values into these fields, unless your later
    calculations will depend upon them.

-   All of these three types are members of the `Eq`{.literal},
    `Ord`{.literal}, `Read`{.literal}, and `Show`{.literal} typeclasses.
    In addition, `Month`{.literal} and `Day`{.literal} are declared as
    members of the `Enum`{.literal} and `Bounded`{.literal} typeclasses.
    For more information on these typeclasses, refer to [the section
    called “Important Built-In
    Typeclasses”](using-typeclasses.html#typeclasses.wellknown "Important Built-In Typeclasses").

    You can generate `CalendarTime`{.literal} values several ways. You
    could start by converting a `ClockTime`{.literal} to a
    `CalendarTime`{.literal} such as this:

~~~~ {#calendartime.ghci:conv .screen}
ghci> :module System.Time
ghci> now <- getClockTime
Loading package old-locale-1.0.0.0 ... linking ... done.
Loading package old-time-1.0.0.0 ... linking ... done.
Mon Aug 18 12:10:35 CDT 2008
ghci> nowCal <- toCalendarTime now
CalendarTime {ctYear = 2008, ctMonth = August, ctDay = 18, ctHour = 12, ctMin = 10, ctSec = 35, ctPicosec = 804267000000, ctWDay = Monday, ctYDay = 230, ctTZName = "CDT", ctTZ = -18000, ctIsDST = True}
ghci> let nowUTC = toUTCTime now
ghci> nowCal
CalendarTime {ctYear = 2008, ctMonth = August, ctDay = 18, ctHour = 12, ctMin = 10, ctSec = 35, ctPicosec = 804267000000, ctWDay = Monday, ctYDay = 230, ctTZName = "CDT", ctTZ = -18000, ctIsDST = True}
ghci> nowUTC
CalendarTime {ctYear = 2008, ctMonth = August, ctDay = 18, ctHour = 17, ctMin = 10, ctSec = 35, ctPicosec = 804267000000, ctWDay = Monday, ctYDay = 230, ctTZName = "UTC", ctTZ = 0, ctIsDST = False}
~~~~

    We used `getClockTime`{.literal} to obtain the current
    `ClockTime`{.literal} from the system's clock. Next,
    `toCalendarTime`{.literal} converts the `ClockTime`{.literal} to a
    `CalendarTime`{.literal} representing the time in the local
    timezone. `toUTCtime`{.literal} performs a similar conversion, but
    its result is in the UTC timezone instead of the local timezone.

    Notice that `toCalendarTime`{.literal} is an IO function, but
    `toUTCTime`{.literal} is not. The reason is that
    `toCalendarTime`{.literal} returns a different result depending upon
    the locally-configured timezone, but `toUTCTime`{.literal} will
    return the exact same result whenever it is passed the same source
    `ClockTime`{.literal}.

    It's easy to modify a `CalendarTime`{.literal} value:

~~~~ {#calendartime.ghci:mod .screen}
ghci> nowCal {ctYear = 1960}
CalendarTime {ctYear = 1960, ctMonth = August, ctDay = 18, ctHour = 12, ctMin = 10, ctSec = 35, ctPicosec = 804267000000, ctWDay = Monday, ctYDay = 230, ctTZName = "CDT", ctTZ = -18000, ctIsDST = True}
ghci> (\(TOD sec _) -> sec) (toClockTime nowCal)
1219079435
ghci> (\(TOD sec _) -> sec) (toClockTime (nowCal {ctYear = 1960}))
-295685365
~~~~

    In this example, we first took the `CalendarTime`{.literal} value
    from earlier and simply switched its year to 1960. Then, we used
    `toClockTime`{.literal} to convert the unmodified value to a
    `ClockTime`{.literal}, and then the modified value, so you can see
    the difference. Notice that the modified value shows a negative
    number of seconds once converted to `ClockTime`{.literal}. That's to
    be expected, since a `ClockTime`{.literal} is an offset from
    midnight on January 1, 1970, UTC, and this value is in 1960.

    You can also create `CalendarTime`{.literal} values manually:

~~~~ {#calendartime.ghci:create .screen}
ghci> let newCT = CalendarTime 2010 January 15 12 30 0 0 Sunday 0 "UTC" 0 False
ghci> newCT
CalendarTime {ctYear = 2010, ctMonth = January, ctDay = 15, ctHour = 12, ctMin = 30, ctSec = 0, ctPicosec = 0, ctWDay = Sunday, ctYDay = 0, ctTZName = "UTC", ctTZ = 0, ctIsDST = False}
ghci> (\(TOD sec _) -> sec) (toClockTime newCT)
1263558600
~~~~

    Note that even though January 15, 2010, isn't a Sunday -- and isn't
    day 0 in the year -- the system was able to process this just fine.
    In fact, if we convert the value to a `ClockTime`{.literal} and then
    back to a `CalendarTime`{.literal}, you'll find those fields
    properly filled in:

~~~~ {#calendartime.ghci:norm .screen}
ghci> toUTCTime . toClockTime $ newCT
CalendarTime {ctYear = 2010, ctMonth = January, ctDay = 15, ctHour = 12, ctMin = 30, ctSec = 0, ctPicosec = 0, ctWDay = Friday, ctYDay = 14, ctTZName = "UTC", ctTZ = 0, ctIsDST = False}
~~~~

#### TimeDiff for ClockTime

Because it can be difficult to manage differences between
`ClockTime`{.literal} values in a human-friendly way, the
`System.Time`{.literal} module includes a `TimeDiff`{.literal} type.
`TimeDiff`{.literal} can be used, where convenient, to handle these
differences. It is defined like this:

~~~~ {#id665901 .programlisting}
data TimeDiff = TimeDiff
   {tdYear :: Int,
    tdMonth :: Int,
    tdDay :: Int,
    tdHour :: Int,
    tdMin :: Int,
    tdSec :: Int,
    tdPicosec :: Integer}
        
~~~~

Functions such as `diffClockTimes`{.literal} and
`addToClockTime`{.literal} take a `ClockTime`{.literal} and a
`TimeDiff`{.literal} and handle the calculations internally by
converting to a `CalendarTime`{.literal} in UTC, applying the
differences, and converting back to a `ClockTime`{.literal}.

Let's see how it works:

~~~~ {#timediff.ghci:all .screen}
ghci> :module System.Time
ghci> let feb5 = toClockTime $ CalendarTime 2008 February 5 0 0 0 0 Sunday 0 "UTC" 0 False
Loading package old-locale-1.0.0.0 ... linking ... done.
Loading package old-time-1.0.0.0 ... linking ... done.
ghci> feb5
Mon Feb  4 18:00:00 CST 2008
ghci> addToClockTime (TimeDiff 0 1 0 0 0 0 0) feb5
Tue Mar  4 18:00:00 CST 2008
ghci> toUTCTime $ addToClockTime (TimeDiff 0 1 0 0 0 0 0) feb5
CalendarTime {ctYear = 2008, ctMonth = March, ctDay = 5, ctHour = 0, ctMin = 0, ctSec = 0, ctPicosec = 0, ctWDay = Wednesday, ctYDay = 64, ctTZName = "UTC", ctTZ = 0, ctIsDST = False}
ghci> let jan30 = toClockTime $ CalendarTime 2009 January 30 0 0 0 0 Sunday 0 "UTC" 0 False
ghci> jan30
Thu Jan 29 18:00:00 CST 2009
ghci> addToClockTime (TimeDiff 0 1 0 0 0 0 0) jan30
Sun Mar  1 18:00:00 CST 2009
ghci> toUTCTime $ addToClockTime (TimeDiff 0 1 0 0 0 0 0) jan30
CalendarTime {ctYear = 2009, ctMonth = March, ctDay = 2, ctHour = 0, ctMin = 0, ctSec = 0, ctPicosec = 0, ctWDay = Monday, ctYDay = 60, ctTZName = "UTC", ctTZ = 0, ctIsDST = False}
ghci> diffClockTimes jan30 feb5
TimeDiff {tdYear = 0, tdMonth = 0, tdDay = 0, tdHour = 0, tdMin = 0, tdSec = 31104000, tdPicosec = 0}
ghci> normalizeTimeDiff $ diffClockTimes jan30 feb5
TimeDiff {tdYear = 0, tdMonth = 12, tdDay = 0, tdHour = 0, tdMin = 0, tdSec = 0, tdPicosec = 0}
~~~~

We started by generating a `ClockTime`{.literal} representing midnight
February 5, 2008 in UTC. Note that, unless your timezone is the same as
UTC, when this time is printed out on the display, it may show up as the
evening of February 4 because it is formatted for your local timezone.

Next, we add one month to to it by calling `addToClockTime`{.literal}.
2008 is a leap year, but the system handled that properly and we get a
result that has the same date and time in March. By using
`toUTCTime`{.literal}, we can see the effect on this in the original UTC
timezone.

For a second experiment, we set up a time representing midnight on
January 30, 2009 in UTC. 2009 is not a leap year, so we might wonder
what will happen when trying to add one month to it. We can see that,
since neither February 29 or 30 exist in 2009, we wind up with March 2.

Finally, we can see how `diffClockTimes`{.literal} turns two
`ClockTime`{.literal} values into a `TimeDiff`{.literal}, though only
the seconds and picoseconds are filled in. The
`normalizeTimeDiff`{.literal} function takes such a `TimeDiff`{.literal}
and reformats it as a human might expect to see it.

### File Modification Times

Many programs need to find out when particular files were last modified.
Programs such as `ls`{.literal} or graphical file managers typically
display the modification time of files. The `System.Directory`{.literal}
module contains a cross-platform `getModificationTime`{.literal}
function. It takes a filename and returns a `ClockTime`{.literal}
representing the time the file was last modified. For instance:

~~~~ {#modtime.ghci:all .screen}
ghci> :module System.Directory
ghci> getModificationTime "/etc/passwd"
Loading package old-locale-1.0.0.0 ... linking ... done.
Loading package old-time-1.0.0.0 ... linking ... done.
Loading package filepath-1.1.0.0 ... linking ... done.
Loading package directory-1.0.0.0 ... linking ... done.
Fri Aug 15 08:29:48 CDT 2008
~~~~

POSIX platforms maintain not just a modification time (known as mtime),
but also the time of last read or write access (atime) and the time of
last status change (ctime). Since this information is POSIX-specific,
the cross-platform `System.Directory`{.literal} module does not provide
access to it. Instead, you will need to use functions in
`System.Posix.Files`{.literal}. Here is an example function to do that:

~~~~ {#posixtime.hs:all .programlisting}
-- file: ch20/posixtime.hs
-- posixtime.hs

import System.Posix.Files
import System.Time
import System.Posix.Types

-- | Given a path, returns (atime, mtime, ctime)
getTimes :: FilePath -> IO (ClockTime, ClockTime, ClockTime)
getTimes fp =
    do stat <- getFileStatus fp
       return (toct (accessTime stat),
               toct (modificationTime stat),
               toct (statusChangeTime stat))

-- | Convert an EpochTime to a ClockTime
toct :: EpochTime -> ClockTime
toct et = 
    TOD (truncate (toRational et)) 0
~~~~

Notice that call to `getFileStatus`{.literal}. That call maps directly
to the C function `stat()`{.literal}. Its return value stores a vast
assortment of information, including file type, permissions, owner,
group, and the three time values we're interested in.
`System.Posix.Files`{.literal} provides various functions, such as
`accessTime`{.literal}, that extract the information we're interested
out of the opaque `FileStatus`{.literal} type returned by
`getFileStatus`{.literal}.

The functions such as `accessTime`{.literal} return data in a
POSIX-specific type called `EpochTime`{.literal}, which se convert to a
`ClockTime`{.literal} using the `toct`{.literal} function.
`System.Posix.Files`{.literal} also provides a `setFileTimes`{.literal}
function to set the atime and mtime for a file.^[[45](#ftn.id666386)]^

Extended Example: Piping
------------------------

We've just seen how to invoke external programs. Sometimes we need more
control that that. Perhaps we need to obtain the output from those
programs, provide input, or even chain together multiple external
programs. Piping can help with all of these needs. Piping is often used
in shell scripts. When you set up a pipe in the shell, you run multiple
programs. The output of the first program is sent to the input of the
second. Its output is sent to the third as input, and so on. The last
program's output normally goes to the terminal, or it could go to a
file. Here's an example session with the POSIX shell to illustrate
piping:

~~~~ {#id666421 .screen}
$ ls /etc | grep 'm.*ap' | tr a-z A-Z
IDMAPD.CONF
MAILCAP
MAILCAP.ORDER
MEDIAPRM
TERMCAP
    
~~~~

This command runs three programs, piping data between them. It starts
with `ls /etc`{.literal}, which outputs a list of all files or
directories in `/etc`{.literal}. The output of `ls`{.literal} is sent as
input to `grep`{.literal}. We gave `grep`{.literal} a regular expression
that will cause it to output only the lines that start with
`'m'`{.literal} and then contain `"ap"`{.literal} somewhere in the line.
Finally, the result of that is sent to `tr`{.literal}. We gave
`tr`{.literal} options to convert everything to uppercase. The output of
`tr`{.literal} isn't set anywhere in particular, so it is displayed on
the screen.

In this situation, the shell handles setting up all the pipelines
between programs. By using some of the POSIX tools in Haskell, we can
accomplish the same thing.

Before describing how to do this, we should first warn you that the
`System.Posix`{.literal} modules expose a very low-level interface to
Unix systems. The interfaces can be complex and their interactions can
be complex as well, regardless of the programming language you use to
access them. The full nature of these low-level interfaces has been the
topic of entire books themselves, so in this chapter we will just
scratch the surface.

### Using Pipes for Redirection

POSIX defines a function that creates a pipe. This function returns two
file descriptors (FDs), which are similar in concept to a Haskell
`Handle`{.literal}. One FD is the reading end of the pipe, and the other
is the writing end. Anything that is written to the writing end can be
read by the reading end. The data is "shoved through a pipe". In
Haskell, you call `createPipe`{.literal} to access this interface.

Having a pipe is the first step to being able to pipe data between
external programs. We must also be able to redirect the output of a
program to a pipe, and the input of another program from a pipe. The
Haskell function `dupTo`{.literal} accomplishes this. It takes a FD and
makes a copy of it at another FD number. POSIX FDs for standard input,
standard output, and standard error have the predefined FD numbers of 0,
1, and 2, respectively. By renumbering an endpoint of a pipe to one of
those numbers, we effectively can cause programs to have their input or
output redirected.

There is another piece of the puzzle, however. We can't just use
`dupTo`{.literal} before a call such as `rawSystem`{.literal} because
this would mess up the standard input or output of our main Haskell
process. Moreover, `rawSystem`{.literal} blocks until the invoked
program executes, leaving us no way to start multiple processes running
in parallel. To make this happen, we must use `forkProcess`{.literal}.
This is a very special function. It actually makes a copy of the
currently-running program and you wind up with two copies of the program
running at the same time. Haskell's `forkProcess`{.literal} function
takes a function to execute in the new process (known as the child). We
have that function call `dupTo`{.literal}. After it has done that, it
calls `executeFile`{.literal} to actually invoke the command. This is
also a special function: if all goes well, it *never returns*. That's
because `executeFile`{.literal} replaces the running process with a
different program. Eventually, the original Haskell process will call
`getProcessStatus`{.literal} to wait for the child processes to
terminate and learn of their exit codes.

Whenever you run a command on POSIX systems, whether you've just typed
`ls`{.literal} on the command line or used `rawSystem`{.literal} in
Haskell, under the hood, `forkProcess`{.literal},
`executeFile`{.literal}, and `getProcessStatus`{.literal} (or their C
equivalents) are always being used. To set up pipes, we are duplicating
the process that the system uses to start up programs, and adding a few
steps involving piping and redirection along the way.

There are a few other housekeeping things we must be careful about. When
you call `forkProcess`{.literal}, just about everything about your
program is cloned^[[46](#ftn.id666704)]^ That includes the set of open
file descriptors (handles). Programs detect when they're done receiving
input from a pipe by checking the end-of-file indicator. When the
process at the writing end of a pipe closes the pipe, the process at the
reading end will receive an end-of-file indication. However, if the
writing file descriptor exists in more than one process, the end-of-file
indicator won't be sent until all processes have closed that particular
FD. Therefore, we must keep track of which FDs are opened so we can
close them all in the child processes. We must also close the child ends
of the pipes in the parent process as soon as possible.

Here is an initial implementation of a system of piping in Haskell.

~~~~ {#RunProcessSimple.hs:all .programlisting}
-- file: ch20/RunProcessSimple.hs
{-# OPTIONS_GHC -fglasgow-exts #-}
-- RunProcessSimple.hs

module RunProcessSimple where

import System.Process
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import System.Exit
import Text.Regex
import System.Posix.Process
import System.Posix.IO
import System.Posix.Types

{- | The type for running external commands.  The first part
of the tuple is the program name.  The list represents the
command-line parameters to pass to the command. -}
type SysCommand = (String, [String])

{- | The result of running any command -}
data CommandResult = CommandResult {
    cmdOutput :: IO String,              -- ^ IO action that yields the output
    getExitStatus :: IO ProcessStatus    -- ^ IO action that yields exit result
    }

{- | The type for handling global lists of FDs to always close in the clients
-}
type CloseFDs = MVar [Fd]

{- | Class representing anything that is a runnable command -}
class CommandLike a where
    {- | Given the command and a String representing input,
         invokes the command.  Returns a String
         representing the output of the command. -}
    invoke :: a -> CloseFDs -> String -> IO CommandResult

-- Support for running system commands
instance CommandLike SysCommand where
    invoke (cmd, args) closefds input =
        do -- Create two pipes: one to handle stdin and the other
           -- to handle stdout.  We do not redirect stderr in this program.
           (stdinread, stdinwrite) <- createPipe
           (stdoutread, stdoutwrite) <- createPipe

           -- We add the parent FDs to this list because we always need
           -- to close them in the clients.
           addCloseFDs closefds [stdinwrite, stdoutread]

           -- Now, grab the closed FDs list and fork the child.
           childPID <- withMVar closefds (\fds ->
                          forkProcess (child fds stdinread stdoutwrite))

           -- Now, on the parent, close the client-side FDs.
           closeFd stdinread
           closeFd stdoutwrite

           -- Write the input to the command.
           stdinhdl <- fdToHandle stdinwrite
           forkIO $ do hPutStr stdinhdl input
                       hClose stdinhdl

           -- Prepare to receive output from the command
           stdouthdl <- fdToHandle stdoutread

           -- Set up the function to call when ready to wait for the
           -- child to exit.
           let waitfunc = 
                do status <- getProcessStatus True False childPID
                   case status of
                       Nothing -> fail $ "Error: Nothing from getProcessStatus"
                       Just ps -> do removeCloseFDs closefds 
                                          [stdinwrite, stdoutread]
                                     return ps
           return $ CommandResult {cmdOutput = hGetContents stdouthdl,
                                   getExitStatus = waitfunc}

        -- Define what happens in the child process
        where child closefds stdinread stdoutwrite = 
                do -- Copy our pipes over the regular stdin/stdout FDs
                   dupTo stdinread stdInput
                   dupTo stdoutwrite stdOutput

                   -- Now close the original pipe FDs
                   closeFd stdinread
                   closeFd stdoutwrite

                   -- Close all the open FDs we inherited from the parent
                   mapM_ (\fd -> catch (closeFd fd) (\_ -> return ())) closefds

                   -- Start the program
                   executeFile cmd True args Nothing

-- Add FDs to the list of FDs that must be closed post-fork in a child
addCloseFDs :: CloseFDs -> [Fd] -> IO ()
addCloseFDs closefds newfds =
    modifyMVar_ closefds (\oldfds -> return $ oldfds ++ newfds)

-- Remove FDs from the list
removeCloseFDs :: CloseFDs -> [Fd] -> IO ()
removeCloseFDs closefds removethem =
    modifyMVar_ closefds (\fdlist -> return $ procfdlist fdlist removethem)

    where
    procfdlist fdlist [] = fdlist
    procfdlist fdlist (x:xs) = procfdlist (removefd fdlist x) xs

    -- We want to remove only the first occurance ot any given fd
    removefd [] _ = []
    removefd (x:xs) fd 
        | fd == x = xs
        | otherwise = x : removefd xs fd

{- | Type representing a pipe.  A 'PipeCommand' consists of a source
and destination part, both of which must be instances of
'CommandLike'. -}
data (CommandLike src, CommandLike dest) => 
     PipeCommand src dest = PipeCommand src dest 

{- | A convenient function for creating a 'PipeCommand'. -}
(-|-) :: (CommandLike a, CommandLike b) => a -> b -> PipeCommand a b
(-|-) = PipeCommand

{- | Make 'PipeCommand' runnable as a command -}
instance (CommandLike a, CommandLike b) =>
         CommandLike (PipeCommand a b) where
    invoke (PipeCommand src dest) closefds input =
        do res1 <- invoke src closefds input
           output1 <- cmdOutput res1
           res2 <- invoke dest closefds output1
           return $ CommandResult (cmdOutput res2) (getEC res1 res2)

{- | Given two 'CommandResult' items, evaluate the exit codes for
both and then return a "combined" exit code.  This will be ExitSuccess
if both exited successfully.  Otherwise, it will reflect the first
error encountered. -}
getEC :: CommandResult -> CommandResult -> IO ProcessStatus 
getEC src dest =
    do sec <- getExitStatus src
       dec <- getExitStatus dest
       case sec of
            Exited ExitSuccess -> return dec
            x -> return x

{- | Execute a 'CommandLike'. -}
runIO :: CommandLike a => a -> IO ()
runIO cmd =
    do -- Initialize our closefds list
       closefds <- newMVar []

       -- Invoke the command
       res <- invoke cmd closefds []

       -- Process its output
       output <- cmdOutput res
       putStr output

       -- Wait for termination and get exit status
       ec <- getExitStatus res
       case ec of
            Exited ExitSuccess -> return ()
            x -> fail $ "Exited: " ++ show x
~~~~

Let's experiment with this in **ghci** a bit before looking at how it
works.

~~~~ {#rps.ghci:all .screen}
ghci> :load RunProcessSimple.hs

RunProcessSimple.hs:12:7:
    Could not find module `Text.Regex':
      Use -v to see a list of the files searched for.
Failed, modules loaded: none.
ghci> runIO $ ("pwd", []::[String])

<interactive>:1:0: Not in scope: `runIO'
ghci> runIO $ ("ls", ["/usr"])

<interactive>:1:0: Not in scope: `runIO'
ghci> runIO $ ("ls", ["/usr"]) -|- ("grep", ["^l"])

<interactive>:1:0: Not in scope: `runIO'

<interactive>:1:25: Not in scope: `-|-'
ghci> runIO $ ("ls", ["/etc"]) -|- ("grep", ["m.*ap"]) -|- ("tr", ["a-z", "A-Z"])

<interactive>:1:0: Not in scope: `runIO'

<interactive>:1:25: Not in scope: `-|-'

<interactive>:1:49: Not in scope: `-|-'
~~~~

We start by running a simple command, `pwd`{.literal}, which just prints
the name of the current working directory. We pass `[]`{.literal} for
the list of arguments, because `pwd`{.literal} doesn't need any
arguments. Due to the typeclasses used, Haskell can't infer the type of
`[]`{.literal}, so we specifically mention that it's a
`String`{.literal}.

Then we get into more complex commands. We run `ls`{.literal}, sending
it through `grep`{.literal}. At the end, we set up a pipe to run the
exact same command that we ran via a shell-built pipe at the start of
this section. It's not yet as pleasant as it was in the shell, but then
again our program is still relatively simple when compared to the shell.

Let's look at the program. The very first line has a special
`OPTIONS_GHC`{.literal} clause. This is the same as passing
`-fglasgow-exts`{.literal} to **ghc** or **ghci**. We are using a GHC
extension that permits us to use a
`(String,           [String])`{.literal} type as an instance of a
typeclass.^[[47](#ftn.id667005)]^ By putting it in the source file, we
don't have to remember to specify it every time we use this module.

After the `import`{.literal} lines, we define a few types. First, we
define `type SysCommand = (String,           [String])`{.literal} as an
alias. This is the type a command to be executed by the system will
take. We used data of this type for each command in the example
execution above. The `CommandResult`{.literal} type represents the
result from executing a given command, and the `CloseFDs`{.literal} type
represents the list of FDs that we must close upon forking a new child
process.

Next, we define a class named `CommandLike`{.literal}. This class will
be used to run "things", where a "thing" might be a standalone program,
a pipe set up between two or more programs, or in the future, even pure
Haskell functions. To be a member of this class, only one function --
`invoke`{.literal} -- needs to be present for a given type. This will
let us use `runIO`{.literal} to start either a standalone command or a
pipeline. It will also be useful for defining a pipeline, since we may
have a whole stack of commands on one or both sides of a given command.

Our piping infrastructure is going to use strings as the way of sending
data from one process to another. We can take advantage of Haskell's
support for lazy reading via `hGetContents`{.literal} while reading
data, and use `forkIO`{.literal} to let writing occur in the background.
This will work well, although not as fast as connecting the endpoints of
two processes directly together.^[[48](#ftn.id667108)]^ It makes
implementation quite simple, however. We need only take care to do
nothing that would require the entire `String`{.literal} to be buffered,
and let Haskell's laziness do the rest.

Next, we define an instance of `CommandLike`{.literal} for
`SysCommand`{.literal}. We create two pipes: one to use for the new
process's standard input, and the other for its standard output. This
creates four endpoints, and thus four file descriptors. We add the
parent file descriptors to the list of those that must be closed in all
children. These would be the write end of the child's standard input,
and the read end of the child's standard output. Next, we fork the child
process. In the parent, we can then close the file descriptors that
correspond to the child. We can't do that before the fork, because then
they wouldn't be available to the child. We obtain a handle for the
`stdinwrite`{.literal} file descriptor, and start a thread via
`forkIO`{.literal} to write the input data to it. We then define
`waitfunc`{.literal}, which is the action that the caller will invoke
when it is ready to wait for the called process to terminate. Meanwhile,
the child uses `dupTo`{.literal}, closes the file descriptors it doesn't
need, and executes the command.

Next, we define some utility functions to manage the list of file
descriptors. After that, we define the tools that help set up pipelines.
First, we define a new type `PipeCommand`{.literal} that has a source
and destination. Both the source and destination must be members of
`CommandLike`{.literal}. We also define the `-|-`{.literal} convenience
operator. Then, we make `PipeCommand`{.literal} an instance of
`CommandLike`{.literal}. Its `invoke`{.literal} implementation starts
the first command with the given input, obtains its output, and passes
that output to the invocation of the second command. It then returns the
output of the second command, and causes the `getExitStatus`{.literal}
function to wait for and check the exit statuses from both commands.

We finish by defining `runIO`{.literal}. This function establishes the
list of FDs that must be closed in the client, starts the command,
displays its output, and checks its exit status.

### Better Piping

Our previous example solved the basic need of letting us set up
shell-like pipes. There are some other features that it would be nice to
have though:

-   Supporting more shell-like syntax

-   Letting people pipe data into external programs or regular Haskell
    functions, freely mixing and matching the two

-   Returning the final output and exit code in a way that Haskell
    programs can readily use

Fortunately, we already have most of the pieces to support this in
place. We need only add a few more instances of `CommandLike`{.literal}
to support this, and a few more functions similar to `runIO`{.literal}.
Here is a revised example that implements all of these features:

~~~~ {#RunProcess.hs:all .programlisting}
-- file: ch20/RunProcess.hs
{-# OPTIONS_GHC -fglasgow-exts #-}

module RunProcess where

import System.Process
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception(evaluate)
import System.Posix.Directory
import System.Directory(setCurrentDirectory)
import System.IO
import System.Exit
import Text.Regex
import System.Posix.Process
import System.Posix.IO
import System.Posix.Types
import Data.List
import System.Posix.Env(getEnv)

{- | The type for running external commands.  The first part
of the tuple is the program name.  The list represents the
command-line parameters to pass to the command. -}
type SysCommand = (String, [String])

{- | The result of running any command -}
data CommandResult = CommandResult {
    cmdOutput :: IO String,              -- ^ IO action that yields the output
    getExitStatus :: IO ProcessStatus    -- ^ IO action that yields exit result
    }

{- | The type for handling global lists of FDs to always close in the clients
-}
type CloseFDs = MVar [Fd]

{- | Class representing anything that is a runnable command -}
class CommandLike a where
    {- | Given the command and a String representing input,
         invokes the command.  Returns a String
         representing the output of the command. -}
    invoke :: a -> CloseFDs -> String -> IO CommandResult

-- Support for running system commands
instance CommandLike SysCommand where
    invoke (cmd, args) closefds input =
        do -- Create two pipes: one to handle stdin and the other
           -- to handle stdout.  We do not redirect stderr in this program.
           (stdinread, stdinwrite) <- createPipe
           (stdoutread, stdoutwrite) <- createPipe

           -- We add the parent FDs to this list because we always need
           -- to close them in the clients.
           addCloseFDs closefds [stdinwrite, stdoutread]

           -- Now, grab the closed FDs list and fork the child.
           childPID <- withMVar closefds (\fds ->
                          forkProcess (child fds stdinread stdoutwrite))

           -- Now, on the parent, close the client-side FDs.
           closeFd stdinread
           closeFd stdoutwrite

           -- Write the input to the command.
           stdinhdl <- fdToHandle stdinwrite
           forkIO $ do hPutStr stdinhdl input
                       hClose stdinhdl

           -- Prepare to receive output from the command
           stdouthdl <- fdToHandle stdoutread

           -- Set up the function to call when ready to wait for the
           -- child to exit.
           let waitfunc = 
                do status <- getProcessStatus True False childPID
                   case status of
                       Nothing -> fail $ "Error: Nothing from getProcessStatus"
                       Just ps -> do removeCloseFDs closefds 
                                          [stdinwrite, stdoutread]
                                     return ps
           return $ CommandResult {cmdOutput = hGetContents stdouthdl,
                                   getExitStatus = waitfunc}

        -- Define what happens in the child process
        where child closefds stdinread stdoutwrite = 
                do -- Copy our pipes over the regular stdin/stdout FDs
                   dupTo stdinread stdInput
                   dupTo stdoutwrite stdOutput

                   -- Now close the original pipe FDs
                   closeFd stdinread
                   closeFd stdoutwrite

                   -- Close all the open FDs we inherited from the parent
                   mapM_ (\fd -> catch (closeFd fd) (\_ -> return ())) closefds

                   -- Start the program
                   executeFile cmd True args Nothing

{- | An instance of 'CommandLike' for an external command.  The String is
passed to a shell for evaluation and invocation. -}
instance CommandLike String where
    invoke cmd closefds input =
        do -- Use the shell given by the environment variable SHELL,
           -- if any.  Otherwise, use /bin/sh
           esh <- getEnv "SHELL"
           let sh = case esh of
                       Nothing -> "/bin/sh"
                       Just x -> x
           invoke (sh, ["-c", cmd]) closefds input

-- Add FDs to the list of FDs that must be closed post-fork in a child
addCloseFDs :: CloseFDs -> [Fd] -> IO ()
addCloseFDs closefds newfds =
    modifyMVar_ closefds (\oldfds -> return $ oldfds ++ newfds)

-- Remove FDs from the list
removeCloseFDs :: CloseFDs -> [Fd] -> IO ()
removeCloseFDs closefds removethem =
    modifyMVar_ closefds (\fdlist -> return $ procfdlist fdlist removethem)

    where
    procfdlist fdlist [] = fdlist
    procfdlist fdlist (x:xs) = procfdlist (removefd fdlist x) xs

    -- We want to remove only the first occurance ot any given fd
    removefd [] _ = []
    removefd (x:xs) fd 
        | fd == x = xs
        | otherwise = x : removefd xs fd

-- Support for running Haskell commands
instance CommandLike (String -> IO String) where
    invoke func _ input =
       return $ CommandResult (func input) (return (Exited ExitSuccess))

-- Support pure Haskell functions by wrapping them in IO
instance CommandLike (String -> String) where
    invoke func = invoke iofunc
        where iofunc :: String -> IO String
              iofunc = return . func

-- It's also useful to operate on lines.  Define support for line-based
-- functions both within and without the IO monad.

instance CommandLike ([String] -> IO [String]) where
    invoke func _ input =
           return $ CommandResult linedfunc (return (Exited ExitSuccess))
       where linedfunc = func (lines input) >>= (return . unlines)

instance CommandLike ([String] -> [String]) where
    invoke func = invoke (unlines . func . lines)

{- | Type representing a pipe.  A 'PipeCommand' consists of a source
and destination part, both of which must be instances of
'CommandLike'. -}
data (CommandLike src, CommandLike dest) => 
     PipeCommand src dest = PipeCommand src dest 

{- | A convenient function for creating a 'PipeCommand'. -}
(-|-) :: (CommandLike a, CommandLike b) => a -> b -> PipeCommand a b
(-|-) = PipeCommand

{- | Make 'PipeCommand' runnable as a command -}
instance (CommandLike a, CommandLike b) =>
         CommandLike (PipeCommand a b) where
    invoke (PipeCommand src dest) closefds input =
        do res1 <- invoke src closefds input
           output1 <- cmdOutput res1
           res2 <- invoke dest closefds output1
           return $ CommandResult (cmdOutput res2) (getEC res1 res2)

{- | Given two 'CommandResult' items, evaluate the exit codes for
both and then return a "combined" exit code.  This will be ExitSuccess
if both exited successfully.  Otherwise, it will reflect the first
error encountered. -}
getEC :: CommandResult -> CommandResult -> IO ProcessStatus 
getEC src dest =
    do sec <- getExitStatus src
       dec <- getExitStatus dest
       case sec of
            Exited ExitSuccess -> return dec
            x -> return x

{- | Different ways to get data from 'run'.

 * IO () runs, throws an exception on error, and sends stdout to stdout

 * IO String runs, throws an exception on error, reads stdout into
   a buffer, and returns it as a string.

 * IO [String] is same as IO String, but returns the results as lines

 * IO ProcessStatus runs and returns a ProcessStatus with the exit
   information.  stdout is sent to stdout.  Exceptions are not thrown.

 * IO (String, ProcessStatus) is like IO ProcessStatus, but also
   includes a description of the last command in the pipe to have
   an error (or the last command, if there was no error)

 * IO Int returns the exit code from a program directly.  If a signal
   caused the command to be reaped, returns 128 + SIGNUM.

 * IO Bool returns True if the program exited normally (exit code 0,
   not stopped by a signal) and False otherwise.

-}
class RunResult a where
    {- | Runs a command (or pipe of commands), with results presented
       in any number of different ways. -}
    run :: (CommandLike b) => b -> a

-- | Utility function for use by 'RunResult' instances
setUpCommand :: CommandLike a => a -> IO CommandResult
setUpCommand cmd = 
    do -- Initialize our closefds list
       closefds <- newMVar []

       -- Invoke the command
       invoke cmd closefds []

instance RunResult (IO ()) where
    run cmd = run cmd >>= checkResult

instance RunResult (IO ProcessStatus) where
    run cmd = 
        do res <- setUpCommand cmd

           -- Process its output
           output <- cmdOutput res
           putStr output

           getExitStatus res
           
instance RunResult (IO Int) where
    run cmd = do rc <- run cmd
                 case rc of
                   Exited (ExitSuccess) -> return 0
                   Exited (ExitFailure x) -> return x
                   Terminated x -> return (128 + (fromIntegral x))
                   Stopped x -> return (128 + (fromIntegral x))

instance RunResult (IO Bool) where
    run cmd = do rc <- run cmd
                 return ((rc::Int) == 0)

instance RunResult (IO [String]) where
    run cmd = do r <- run cmd
                 return (lines r)

instance RunResult (IO String) where
    run cmd =
        do res <- setUpCommand cmd

           output <- cmdOutput res

           -- Force output to be buffered
           evaluate (length output)

           ec <- getExitStatus res
           checkResult ec
           return output

checkResult :: ProcessStatus -> IO ()
checkResult ps =
    case ps of
         Exited (ExitSuccess) -> return ()
         x -> fail (show x)

{- | A convenience function.  Refers only to the version of 'run'
that returns @IO ()@.  This prevents you from having to cast to it
all the time when you do not care about the result of 'run'.
-}
runIO :: CommandLike a => a -> IO ()
runIO = run

------------------------------------------------------------
-- Utility Functions
------------------------------------------------------------
cd :: FilePath -> IO ()
cd = setCurrentDirectory
 
{- | Takes a string and sends it on as standard output.
The input to this function is never read. -}
echo :: String -> String -> String
echo inp _ = inp

-- | Search for the regexp in the lines.  Return those that match.
grep :: String -> [String] -> [String]
grep pat = filter (ismatch regex)
    where regex = mkRegex pat
          ismatch r inp = case matchRegex r inp of
                            Nothing -> False
                            Just _ -> True

{- | Creates the given directory.  A value of 0o755 for mode would be typical.
An alias for System.Posix.Directory.createDirectory. -}
mkdir :: FilePath -> FileMode -> IO ()
mkdir = createDirectory

{- | Remove duplicate lines from a file (like Unix uniq).
Takes a String representing a file or output and plugs it through 
lines and then nub to uniqify on a line basis. -}
uniq :: String -> String
uniq = unlines . nub . lines

-- | Count number of lines.  wc -l
wcL, wcW :: [String] -> [String]
wcL inp = [show (genericLength inp :: Integer)]

-- | Count number of words in a file (like wc -w)
wcW inp = [show ((genericLength $ words $ unlines inp) :: Integer)]

sortLines :: [String] -> [String]
sortLines = sort

-- | Count the lines in the input
countLines :: String -> IO String
countLines = return . (++) "\n" . show . length . lines
~~~~

Here's what has changed:

-   A new `CommandLike`{.literal} instance for `String`{.literal} that
    uses the shell to evaluate and invoke the string.

-   New `CommandLike`{.literal} instances for
    `String -> IO String`{.literal} and various other types that are
    implemented in terms of this one. These process Haskell functions as
    commands.

-   A new `RunResult`{.literal} typeclass that defines a function
    `run`{.literal} that returns information about the command in many
    different ways. See the comments in the source for more information.
    `runIO`{.literal} is now just an alias for one particular
    `RunResult`{.literal} instance.

-   A few utility functions providing Haskell implementations of
    familiar Unix shell commands.

Let's try out the new shell features. First, let's make sure that the
command we used in the previous example still works. Then, let's try it
using a more shell-like syntax.

~~~~ {#rp.ghci:shell .screen}
ghci> :load RunProcess.hs

RunProcess.hs:14:7:
    Could not find module `Text.Regex':
      Use -v to see a list of the files searched for.
Failed, modules loaded: none.
ghci> runIO $ ("ls", ["/etc"]) -|- ("grep", ["m.*ap"]) -|- ("tr", ["a-z", "A-Z"])

<interactive>:1:0: Not in scope: `runIO'

<interactive>:1:25: Not in scope: `-|-'

<interactive>:1:49: Not in scope: `-|-'
ghci> runIO $ "ls /etc" -|- "grep 'm.*ap'" -|- "tr a-z A-Z"

<interactive>:1:0: Not in scope: `runIO'

<interactive>:1:18: Not in scope: `-|-'

<interactive>:1:37: Not in scope: `-|-'
~~~~

That was a lot easier to type. Let's try substituting our native Haskell
implementation of `grep`{.literal} and try out some other new features
as well:

~~~~ {#rp.ghci:hask .screen}
ghci> runIO $ "ls /etc" -|- grep "m.*ap" -|- "tr a-z A-Z"

<interactive>:1:0: Not in scope: `runIO'

<interactive>:1:18: Not in scope: `-|-'

<interactive>:1:22: Not in scope: `grep'

<interactive>:1:35: Not in scope: `-|-'
ghci> run $ "ls /etc" -|- grep "m.*ap" -|- "tr a-z A-Z" :: IO String

<interactive>:1:0: Not in scope: `run'

<interactive>:1:16: Not in scope: `-|-'

<interactive>:1:20: Not in scope: `grep'

<interactive>:1:33: Not in scope: `-|-'
ghci> run $ "ls /etc" -|- grep "m.*ap" -|- "tr a-z A-Z" :: IO [String]

<interactive>:1:0: Not in scope: `run'

<interactive>:1:16: Not in scope: `-|-'

<interactive>:1:20: Not in scope: `grep'

<interactive>:1:33: Not in scope: `-|-'
ghci> run $ "ls /nonexistant" :: IO String

<interactive>:1:0: Not in scope: `run'
ghci> run $ "ls /nonexistant" :: IO ProcessStatus

<interactive>:1:0: Not in scope: `run'

<interactive>:1:30:
    Not in scope: type constructor or class `ProcessStatus'
ghci> run $ "ls /nonexistant" :: IO Int

<interactive>:1:0: Not in scope: `run'
ghci> runIO $ echo "Line1\nHi, test\n" -|- "tr a-z A-Z" -|- sortLines

<interactive>:1:0: Not in scope: `runIO'

<interactive>:1:8: Not in scope: `echo'

<interactive>:1:33: Not in scope: `-|-'

<interactive>:1:50: Not in scope: `-|-'

<interactive>:1:54: Not in scope: `sortLines'
~~~~

### Final Words on Pipes

We have developed a sophisticated system here. We warned you earlier
that POSIX can be complex. One other thing we need to highlight: you
must always make sure to evaluate the `String`{.literal} returned by
these functions before you attempt to evaluate the exit code of the
child process. The child process will often not exit until it can write
all of its data, and if you do this in the wrong order, your program
will hang.

In this chapter, we have developed, from the ground up, a simplified
version of HSH. If you wish to use these shell-like capabilities in your
own programs, we recommend HSH instead of the example developed here due
to optimizations present in HSH. HSH also comes with a larger set of
utility functions and more capabilities, but the source code behind the
library is much more complex and large. Some of the utility functions
presented here, in fact, were copied verbatim from HSH. HSH is available
from
[http://software.complete.org/hsh](http://software.complete.org/hsh).

\

* * * * *

^[[43](#id664371)]^There is also a function `system`{.literal} that
takes only a single string and passes it through the shell to parse. We
recommend using `rawSystem`{.literal} instead, because the shell
attaches special meaning to certain characters, which could lead to
security issues or unexpected behavior.

^[[44](#id664881)]^Some will note that UTC defines leap seconds at
irregular intervals. The POSIX standard, which Haskell follows, states
that every day is exactly 86,400 seconds in length in its
representation, so you need not be concerned about leap seconds when
performing routine calculations. The exact manner of handling leap
seconds is system-dependent and complex, though usually it can be
explained as having a "long second". This nuance is generally only of
interest when performing precise subsecond calculations.

^[[45](#id666386)]^It is not normally possible to set the ctime on POSIX
systems.

^[[46](#id666704)]^The main exception is threads, which are not cloned.

^[[47](#id667005)]^This extension is well-supported in the Haskell
community; Hugs users can access the same thing with
`hugs -98 +o`{.literal}.

^[[48](#id667108)]^The Haskell library HSH provides a similar API to
that presented here, but uses a more efficient (and much more complex)
mechanism of connecting pipes directly between external processes
without the data needing to pass through Haskell. This is the same
approach that the shell takes, and reduces the CPU load of handling
piping.

![image](/support/figs/rss.png) Want to stay up to date? Subscribe to
the comment feed for [this chapter](/feeds/comments/), or the [entire
book](/feeds/comments/).

Copyright 2007, 2008 Bryan O'Sullivan, Don Stewart, and John Goerzen.
This work is licensed under a [Creative Commons
Attribution-Noncommercial 3.0
License](http://creativecommons.org/licenses/by-nc/3.0/). Icons by [Paul
Davey](mailto:mattahan@gmail.com) aka
[Mattahan](http://mattahan.deviantart.com/).

  ----------------------------- -------------------- ------------------------------
  [Prev](error-handling.html)                        [Next](using-databases.html)
  Chapter 19. Error handling    [Home](index.html)   Chapter 21. Using Databases
  ----------------------------- -------------------- ------------------------------


