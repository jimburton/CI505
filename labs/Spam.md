# Exercises with the `spamfilter` case study

In this week's lab exercise you will download the program we used as a
case study in a recent lecture and make several improvements to it.

1. Create your own local copy of the program by cloning the `spamfilter` repository: 

```
$ git clone https://github.com/jimburton/spamfilter
```

2. Use `cabal` to build the program:

```
$ cd spamfilter
spamfilter$ cabal build
```

Running `cabal build` will download and build the libraries that
`spamfilter` depends on, then install the program in a subfolder of
the repository. Next, run the shell script that sets up the database:

```
spamfilter$ ./setupdb.sh
```

3. Train the program and use it to classify an email message. Download at least one tarball
  (the Linux version of a zip file) of ham and all of the tarballs of spam from this page:
  https://spamassassin.apache.org/old/publiccorpus/. Extract the contents of the tarballs.
  E.g.:

```
$ tar xjvf XXX-easy_ham.tar.bz2
$ tar xjvf XXX-spam.tar.bz2
```

Then train the program:

```
spamfilter$ cabal run spamfilter train Spam /path/to/spam
spamfilter$ cabal run spamfilter train Ham /path/to/ham
```

Then try classifying some messages. The files in the directory `etc/mail` contain email
messages and are named to indicate whether they are supposed to be ham or spam:

```
spamfilter$ cabal run spamfilter classify etc/mail/ham1.email
```

4. Now make some improvements to the program. First, so that you will
   always be able to go back ot the original code, create a new branch
   in git and switch to it:
   
```
spamfilter$ git checkout -b mybranch
```

  As your first improvement, create a "usage" (or "help") message that
  explains how to use the program. This message should be printed when
  the (new) `help` option is supplied, and also when no options or an
  unrecognised option is supplied. Write the functions that do this
  within the `Main` module.
  
5. At the moment, the `train` option can take the path to a single file or a
  directory. If the path to a directory is supplied, the files within the directory are read
  recursively and used for training. 
  
  The `classify` option, on the other hand, can only deal with the path to a single
  file. Modify the program so that all messages in a directory can be classified at once. Start
  off by reading the functions in the `Train` module, especially

```Haskell
getRecursiveContents :: FilePath -> IO [FilePath]
```

  Keep in mind the design goal of a clear separation between pure and impure code -- that is,
  make sure that you don't pollute the pure code in the `Classify` module with IO actions.


