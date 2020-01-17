# Working with projects

If you're working on a very small script or program that fits into one
module and has no external dependencies (i.e. you aren't importing
libraries that aren't part of the Haskell base language), you can get
along very well using the interpreter `ghci` to run and experiment
with the code and the compiler `ghc` to build an executable. For
anything bigger you need a *build tool*. This will automate
downloading of the dependencies, make sure everything is compiled in
the right order, make it easy to run a suite of tests and to package
your application for other users. There are two main ways of building
Haskell projects --
[`stack`](https://docs.haskellstack.org/en/stable/README/) and
[`cabal`](https://www.haskell.org/cabal/). We will be using `cabal`.

`cabal` is installed on Linux in the labs but it will be an older
version that needs to be upgraded. Start by entering these commands
into a terminal:

```bash
$ cabal update

$ cabal install cabal-install
```

This will download the latest list of packages from the "Hackage"
repository (which is where haskell developers upload libraries and
other software for distribution) and install the latest version of
`cabal`. The default location for packages is
`~/.cabal/bin`, where `~` is your home directory,
e.g. `/home/ab123`. So to run programs installed by `cabal` (such as the
new version of it you just installed), you need to
add this location to your `PATH` variable. This is a Linux environment
variable containing a list of locations in the file system that Bash
will look in when you type a command in to a
terminal. You can check the current state of `PATH` like this:

```bash
$ echo $PATH
```

You can add the `cabal` bin (short for "binary") directory to PATH by
editing the file `~/.bashrc`.  Add these lines to the bottom of the
file:

```bash
PATH=~/.cabal/bin:$PATH
export PATH
```

Then apply the changes:

```bash
$ source .bashrc
```

Check that the changes were applied by echoing `PATH` again. The new
version of `cabal` should now be the first one that the system
encounters when you enter the command. Check this using the `which`
command, which reports the location of executables in your `PATH`. You
should see something like this:

```bash
$ which cabal
/home/ab123/.cabal/bin/cabal
```

Now you can start building haskell projects. The
basic idea is that each project lives in its own directory and if the
project is called `myproject` there should be a text file called `myproject.cabal`
at the top level. You can generate it like this:

```bash
$ mkdir myproject
$ cd myproject
$ cabal init
$ ls
```

To get an idea of the structure of a Haskell project we will use a simple one of
mine as an example. `MkPasswd` is a program that generates new passowrds of varying
strength. Clone the repository from github, `cd` into the directory and list its 
contents:

```
$ git clone https://github.com/jimburton/MkPasswd
Cloning into 'MkPasswd'...
remote: Enumerating objects: 4, done.
remote: Counting objects: 100% (4/4), done.
remote: Compressing objects: 100% (4/4), done.
remote: Total 117 (delta 0), reused 2 (delta 0), pack-reused 113
Receiving objects: 100% (117/117), 18.72 KiB | 912.00 KiB/s, done.
Resolving deltas: 100% (53/53), done.
$ cd MkPasswd
MkPasswd$ ls
LICENSE  mkPasswd.cabal  README.md  Setup.hs  src  tests
```
As you can see, there is a `cabal` file, a `README` file describing the project,
and two directories for code, `src/` and `tests`. Use `cabal` to run the program
and then run the tests:

```
MkPasswd$ cabal run mkPasswd
...
MkPasswd$ cabal run test-mkPasswd
```

This program takes a variety of flags on the command line that govern
the kind of passwords that are generated. If you want to pass flags to
a program that is being run by `cabal` you have to do so after two
dashes (`--`) so that `cabal` can distinguish between the arguments
intended for it and those intended for the program it is running. Pass
the ``--help` command to `MkPasswd` to lists all its options then
experiment with producing a few different types of password:

```
MkPasswd$ cabal run mkPasswd -- --help
```

This is how you would run the program during development. `cabal` can also install it
permanently into `~/.cabal/bin`. Install the program and run it:

```
MkPasswd$ cabal install
MkPasswd$ which mkPasswd
/home/jb259/.cabal/bin/mkPasswd
MkPasswd$ mkPasswd 
5ICk3D
```

One more very useful thing `cabal` can do for you is to start the interpreter, `ghci`,
loading all the modules and dependencies that it needs to run. This is done with the `repl`
command.

Every project consists of one or more *executables* (a runnable program), 
*libraries* (code that other developers will import to their own projects)
or both. An executable needs to have an entry point. This is a top-level module
called `Main.hs` which includes a function called `main` with the right
type. 

```haskell
-- In src/Main.hs
module Main where

-- ...
{-| Entry point. -}
main :: IO ()
main = do xs <- getArgs
-- ...
```

All the `Main` module does is act as an entry point and handle reading
the command line flags passed in by the user.

You need to tell `cabal` about the structure of your project, where
the `Main` module is and so on. Open the cabal file in your editor.

The cabal file contains a "stanza" (a named
block of config details) for the executable and one for the 
test-suite. 

See the
[docs](https://www.haskell.org/cabal/users-guide/developing-packages.html)
for more information.
