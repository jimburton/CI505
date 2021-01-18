# Haskell development environment

## Writing Haskell with style
 The way that you format and organise your code is important. It has an enormous effect on 
 the readability and ease of maintaining the code. If you get used to using standard conventions 
 then others will find it much easier to understand what you wrote, and you'll find it easier to 
 read code written by others too. I recommend that everyone read and follow 
 [Johan Tibell's Haskell Style Guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md). 

You can also get suggestions on improving your code from the very useful `hlint` tool. You can 
install it using `cabal`, read the hints then fix them: 
```
$ cabal install hlint
$ hlint src/
src/Main.hs:16:11-31: Warning: Use print
Found:
  putStrLn $ show coins
Perhaps:
  print coins

1 hint
```

## Editors for Haskell development

There are quite a few editors that support Haskell development,
ranging from ones which aim to be a full-on IDE to ones which just
provide a few helpful features such as syntax highlighting. You can do
everything you need to do with a basic editor and the terminal, and
it's important to know how to do that, but as your programs become
longer and solve more complex problems features like autocompletion,
refactoring, integration with **build tools** and **version control**
start to make a big difference to productivity.

There is a list of options which is kept up to date on the Haskell
wiki [here](https://wiki.haskell.org/IDEs).

I think the best option is to learn one of the "poweruser" editors,
`vim` or `emacs` - I use `emacs`. Once you get used to the quirky
keybindings and terminology it is a *very* powerful editor that has
modes for every programming language under the sun. Start with the
tutorial. Open emacs and type `C-h t`. That is, hold down `Ctrl` and press
`h`, let go of both and press `t`). 

There is some guidance on getting set up for developing haskell in
emacs [here](https://wiki.haskell.org/Emacs). In short, the `emacs`
"major mode" called `haskell-mode` provides the basic syntax
highlighting, ability to launch `ghci` and so on. You can install a
"minor mode" alongside that to provide an interface to the
interpreter, allowing you to query the type of expressions and so
on. Currently (2020), I like [`dante`](https://github.com/jyp/dante). 
You can give this combination a try by adding the following to your
`emacs` config file, `~/.emacs`:

```
(require 'package)
(add-to-list 'package-archives
	 '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package haskell-mode
  :ensure t)
(add-hook 'haskell-mode-hook #'hindent-mode)
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
(setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
(add-to-list 'exec-path my-cabal-path))
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'dante-mode))
```


## Working with projects

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
project is called `myproject` there should be a config file called `myproject.cabal`
at the top level. You can generate a new project like this:

```bash
$ mkdir myproject
$ cd myproject
$ cabal init
$ ls
CHANGELOG.md  Main.hs  myproject.cabal  Setup.hs
```

Display the contents of the config file with `cat`:

```bash
myproject$ cat myproject.cabal 
cabal-version:       >=1.10
-- Initial package description 'myproject.cabal' generated by 'cabal init'.
--   For further documentation, see http://haskell.org/cabal/users-guide/

name:                myproject
version:             0.1.0.0
-- synopsis:
-- description:
-- bug-reports:
-- license:
license-file:        LICENSE
author:              Jim Burton
maintainer:          j.burton@brighton.ac.uk
-- copyright:
-- category:
build-type:          Simple
extra-source-files:  CHANGELOG.md

executable myproject
  main-is:             Main.hs
  -- other-modules:
  -- other-extensions:
  build-depends:       base >=4.10 && <4.11
  -- hs-source-dirs:
  default-language:    Haskell2010

```

The upper block of text contains general information about the
project. The lower block, beginning with `executable ...`, tells
`cabal` to build an executable (i.e. a program) called `myproject`,
and that it can find the code in a file called `Main.hs`.

To get a better idea of the structure of a real Haskell project we
will look at two very simple projects as examples. The first is called
`change` -- it reads in a number from the user and calculates the
coins needed to make that amount. Clone the repository from github and
`cd` into the directory:

```
$ git clone https://github.com/jimburton/change
Cloning into 'change'...
...
$ cd change
```

Now you can use `cabal` to run the program:

```
change$ cabal run change
...
Enter a number and I'll count out the change
44
2 twenty pence pieces, 2 two pence pieces
Enter a number and I'll count out the change

```

Enter an empty line to stop using the program. List the contents of the
project folder:

```
change$ ls
cabal.project.local  ChangeLog.md   LICENSE    Setup.hs  TAGS
change.cabal         dist-newstyle  README.md  src       test

```
The main things to notice are the `src` folder, which is where the code
lives, and the config file `change.cabal`. Open the config file and read the contents. 

From the `cabal` file, you can see that the entry point for the
application is the file `src/Main.hs`. Open this file and read the
code. Don't worry if you don't understand all of it at this stage, but
look out for the `main` method -- this is the first function to run.
Note that most of the code is actually in the module called `Change`,
which is imported in the `Main` module. Open `Change.hs` and read this
file too.

The `test` folder contains tests for the project. Haskell testing will be 
discussed later in the module. For now, use `cabal` to run the tests,
noting the names of the two test-suites given in `change.cabal`.


Next, we will look at a project which is still very simple but
slightly more realistic because it uses some libraries. `mkpasswd` is
a program that generates new passwords of varying strength. Clone the
repository from github and `cd` into the top-level directory:

```
$ git clone https://github.com/jimburton/mkpasswd
Cloning into 'mkpasswd'...
...
$ cd mkpasswd
```

Take a look at the `cabal` config file for the new project,
`mkpasswd.cabal`. This is the block (or "stanza") that defines the program to be built:

```
executable mkpasswd
  main-is: Main.hs             
  other-modules:       MkPasswd.MkPasswd
                     , MkPasswd.Types
  build-depends:       base, random >=1.0 
  hs-source-dirs:      src
  default-language:    Haskell2010

```

`other-modules` is a list of all the modules in our program that are
used when it runs. `build-depends` is a list of all the external
libraries that it uses. In this case that is just `base` (i.e. the
Prelude functions) and `random`, used for making random
passwords. 

Below this stanza is a second one that defines a test suite. Because
there is more than one stanza, you need to tell `cabal` which you want
to run. Now run the program, followed by its tests:


```
$ cabal run mkpasswd
...
$ cabal run test-mkpasswd
```

This program takes a variety of flags (or options) on the command line that govern
the kind of passwords that are generated. If you want to pass flags to
a program that is being run by `cabal` you have to do so after two
dashes (`--`) so that `cabal` can distinguish between the arguments
intended for *it* and those intended for *the program it is running*. Pass
the `--help` command to `MkPasswd` to lists all its options then
experiment with producing a few different types of password:

```
$ cabal run mkpasswd -- --help
```

This is how you would run the program during development. `cabal` can
also install your programs permanently into `~/.cabal/bin`. Install
the program and run it:

```
$ cabal install
$ which mkpasswd
/home/jb259/.cabal/bin/mkpasswd
$ mkpasswd  -l 5 -e
vIt@15 [vitals] 
```

One more very useful thing `cabal` can do for you is to start the interpreter, `ghci`,
loading all the modules and dependencies that it needs to run. This is done with the `repl`
command. Run the program in the REPL and call its `main` function:

```
$ cabal repl
...
*Main> main
K3MP'5
```

See the
[docs](https://www.haskell.org/cabal/users-guide/developing-packages.html)
for full information on `cabal`.

