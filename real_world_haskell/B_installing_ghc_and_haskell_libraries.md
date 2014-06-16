[Real World Haskell](/)by Bryan O'Sullivan, Don Stewart, and John Goerzen
-------------------------------------------------------------------------

Appendix A. Installing GHC and Haskell libraries

[Prev](software-transactional-memory.html)

[Next](characters-strings-and-escaping-rules.html)

Appendix A. Installing GHC and Haskell libraries
------------------------------------------------

**Table of Contents**

[Installing GHC](installing-ghc-and-haskell-libraries.html#id688268)

[Windows](installing-ghc-and-haskell-libraries.html#install.win)

[Mac OS X](installing-ghc-and-haskell-libraries.html#install.mac)

[Alternatives](installing-ghc-and-haskell-libraries.html#id688691)

[Ubuntu and Debian
Linux](installing-ghc-and-haskell-libraries.html#install.debian)

[Fedora Linux](installing-ghc-and-haskell-libraries.html#install.fedora)

[FreeBSD](installing-ghc-and-haskell-libraries.html#id688876)

[Installing Haskell
software](installing-ghc-and-haskell-libraries.html#installing.haskell.software)

[Automated download and installation with
cabal](installing-ghc-and-haskell-libraries.html#id688956)

[Installing cabal](installing-ghc-and-haskell-libraries.html#id689017)

[Updating cabal's package
list](installing-ghc-and-haskell-libraries.html#id689131)

[Installing a library or
program](installing-ghc-and-haskell-libraries.html#id689166)

[Building packages by
hand](installing-ghc-and-haskell-libraries.html#install.pkg.manual)

We are providing the instructions below based on our experience
installing GHC and other software in late 2008. Installation
instructions inevitably become dated quickly; please bear this in mind
as you read.

Installing GHC
--------------

Because GHC runs on a large number of platforms, we focus on a handful
of the most popular.

### Windows

The prebuilt binary packages of GHC should work on Windows Vista and XP
(even Windows 2000). We have installed GHC 6.8.3 under Windows XP
Service Pack 2; here are the steps we followed.

![[Note]](/support/figs/note.png)

How much room does GHC need?

On Windows, GHC requires about 400MB of disk space. The exact amount
will vary from release to release.

Our first step is to visit the GHC [download
page](http://www.haskell.org/ghc/download.html), and follow the link to
the current stable release. Scroll down to the section entitled “Binary
packages”, and then again to the subsection for Windows. Download the
installer; in our case, it's named
`ghc-6.8.3-i386-windows.exe`{.filename}.

![Screenshot of Firefox, displaying the GHC download
page.](figs/win32-download.png)

After the installer has downloaded, double-clicking on it starts the
installation process. This involves stepping through a normal Windows
installer wizard.

![Screenshot of the GHC installation wizard on
Windows.](figs/win32-wizard1.png)

Once the installer has finished, the Start Menu's “All Programs” submenu
should have a GHC folder, inside of which you'll find an icon that you
can use to run **ghci**.

![Screenshot of the Windows XP start menu, showing the GHC
submenu.](figs/win32-start-menu.png)

Clicking on the **ghci** icon brings up a normal Windows console window,
running **ghci**.

![Screenshot of the ghci interpreter running on
Windows.](figs/win32-ghci.png)

![[Note]](/support/figs/note.png)

Updating your search path

The GHC installer automatically modifies your user account's
`PATH`{.envar} environment variable so that commands like **ghc** will
be present in the command shell's search path (i.e. you can type a GHC
command name without typing its complete path). This change will take
effect the next time you open a command shell.

### Mac OS X

We have installed GHC 6.8.3 under Mac OS X 10.5 (Leopard), on an
Intel-based MacBook. Before installing GHC, the Xcode development system
must already be installed.

The Xcode software installer may have come bundled on a DVD with your
Mac. If not (or you can't find it), you should be able to download it
from Apple. Once you've finished installing Xcode, continue on to
download GHC itself.

Visit the GHC [download page](http://www.haskell.org/ghc/download.html),
and follow the link to the current stable release. Scroll down to the
section entitled “Binary packages”, and then again to the subsection for
Mac OS X. There is a single installer package available. Download and
run it.

![[Tip]](/support/figs/tip.png)

Terminal at your fingertips yet?

Since most of your interactions with GHC will be through a Terminal
window, this might be a good time to add the Terminal application to
your dock, if you haven't already done so. You can find it in the
system's `/Applications/Utilities`{.filename} folder.

The installation process should take a minute or two. Finally, you
should be able to successfully run the **ghci** command from your shell
prompt.

![Screenshot of the ghci interpreter running in a Terminal window on Mac
OS X.](figs/osx-ghci.png)

#### Alternatives

Both the MacPorts and Fink projects provide builds of GHC.

### Ubuntu and Debian Linux

Under both Ubuntu and Debian, you can install a minimal working version
of GHC by running **sudo aptitude install ghc6** from a shell prompt.

These distros maintain a small core GHC package, which is insufficient
for much practical development. However, they make a number of
additional prebuilt packages available. To see a complete list of these
prebuilt packages, run **apt-cache search libghc6**. We recommend that
you install at least the `mtl`{.code} package, using **sudo aptitude
install libghc6-mtl-dev**.

Since you will probably want to profile the performance of your Haskell
programs at some point, you should also install the `ghc6-prof`{.code}
package.

### Fedora Linux

GHC is available as a standard Fedora binary package. From a shell, all
you need to do is run the following command:

~~~~ {#id688806 .screen}
sudo yum -y install ghc ghc-doc ghc683-prof
~~~~

The base package, containing the **ghc** and **ghci** commands and
libraries, is `ghc`{.filename}. The `ghc-doc`{.filename} package
contains the GHC user guide, and command and library documentation. The
`ghc683-prof`{.filename} package contains profiling-capable versions of
the standard libraries (note: its version number may have changed by the
time you read this).

Once installation has finished, you should be able to run **ghci** from
the shell immediately. You won't need to change your shell's search
path, or set any environment variables.

### FreeBSD

Under FreeBSD, run the following commands:

~~~~ {#id688889 .screen}
$ cd /usr/ports/lang/ghc
$ sudo make install clean
~~~~

This will download and build GHC from source. You should expect the
process to take several hours.

Installing Haskell software
---------------------------

Almost all Haskell libraries are distributed using a standard packaging
system named Cabal. You can find hundreds of Haskell open source
libraries and programs, all of which use Cabal, at
[http://hackage.haskell.org/](http://hackage.haskell.org/), the home of
the Hackage code repository.

### Automated download and installation with cabal

A command named **cabal** automates the job of downloading, building,
and installing a Haskell package. It also figures out what dependencies
a particular library needs, and either makes sure that they are
installed already, or downloads and builds those first. You can install
any Haskell package with a single **cabal install *`mypackage`***
command.

The **cabal** command is not bundled with GHC, so at least as of version
6.8.3 of GHC, you will have to download and build **cabal** yourself.

#### Installing cabal

To build the **cabal** command, download the sources for the following
four packages from
[http://hackage.haskell.org/](http://hackage.haskell.org/)

-   `Cabal`{.code}

-   `HTTP`{.code}

-   `zlib`{.code}

-   `cabal-install`{.code}

Follow the instructions in [the section called “Building packages by
hand”](installing-ghc-and-haskell-libraries.html#install.pkg.manual "Building packages by hand")
below to manually build each of the four packages above, making sure
that you leave `cabal-install`{.code} until last.

After you install the `cabal-install`{.code} package, the
`$HOME/.cabal/bin`{.code} directory will contain the **cabal** command.
You can either move it somewhere more convenient or add that directory
to your shell's search path.

#### Updating cabal's package list

After installing **cabal**, and periodically thereafter, you should
download a fresh list of packages from Hackage. You can do so as
follows:

~~~~ {#id689151 .screen}
$ cabal update
~~~~

#### Installing a library or program

To install some executable or library, just run the following command:

~~~~ {#id689180 .screen}
$ cabal install -p mypackage
~~~~

### Building packages by hand

If you download a tarball from Hackage, it will arrive in source form.
Unpack the tarball, and go into the newly created directory in a command
shell. The process to build and install it is simple, consisting of
three commmands:

1.  Configure for system-wide installation (i.e. available to all
    users):

~~~~ {#id689234 .screen}
$ runghc Setup configure -p
~~~~

    Alternatively, configure to install only for yourself:

~~~~ {#id689257 .screen}
$ runghc Setup configure --user --prefix=$HOME -p
~~~~

2.  Build (this will build each source file twice, with and without
    profiling support):

~~~~ {#id689283 .screen}
$ runghc Setup build
~~~~

3.  Install if you chose system-wide configuration above:

~~~~ {#id689308 .screen}
$ sudo runghc Setup install
~~~~

    Alternatively, if you chose configuration for yourself only:

~~~~ {#id689331 .screen}
$ runghc Setup install
~~~~

If you build by hand, you will frequently find that the configuration
step fails because some other library must be installed first. You may
find yourself needing to download and build several packages before you
can make progress on the one you really want. This is why we recommend
using the **cabal** command instead.

![image](/support/figs/rss.png) Want to stay up to date? Subscribe to
the comment feed for [this chapter](/feeds/comments/), or the [entire
book](/feeds/comments/).

Copyright 2007, 2008 Bryan O'Sullivan, Don Stewart, and John Goerzen.
This work is licensed under a [Creative Commons
Attribution-Noncommercial 3.0
License](http://creativecommons.org/licenses/by-nc/3.0/). Icons by [Paul
Davey](mailto:mattahan@gmail.com) aka
[Mattahan](http://mattahan.deviantart.com/).

  -------------------------------------------- -------------------- -----------------------------------------------------
  [Prev](software-transactional-memory.html)                        [Next](characters-strings-and-escaping-rules.html)
  Chapter 28. Software transactional memory    [Home](index.html)   Appendix B. Characters, strings, and escaping rules
  -------------------------------------------- -------------------- -----------------------------------------------------


