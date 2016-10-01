# hsinstall


## Synopsis

Install Haskell software


## Description

This is a utility to install Haskell programs on a system using
stack. Even though stack has an `install` command, I found it to be
not enough for my needs. This software tries to install the binaries,
the LICENSE file and also the resources directory if it finds one.

Installations can be performed in one of two directory
structures. FHS, or the Filesystem Hierarchy Standard (most UNIX-like
systems) and what I call "bundle" which is a portable directory
for the app and all of its files. They look like this:

bundle is sort-of a self-contained structure like this:

     $PREFIX/
       $PROJECT-$VERSION/
         bin/...
         doc/LICENSE
         resources/...

fhs is the more traditional UNIX structure like this:

     $PREFIX/
       bin/...
       share/
         $PROJECT-$VERSION/
           doc/LICENSE
           resources/...

There are two parts to hsinstall that are intended to work 
together. The first part is a Haskell shell script,
`util/install.hs`. Take a copy of this script and check it into
a project you're working on. This will be your installation
script. Running the script with the `--help` switch will explain
the options. Near the top of the script are default values for
these options that should be tuned to what your project needs.

The other part of hsinstall is a library. The install script will try
to install a `resources` directory if it finds one. the HSInstall
library code is then used in your code to locate the resources
at runtime.

Note that you only need this library if your software has data files
it needs to locate at runtime in the installation directories. Many
programs don't have this requirement and can ignore the library
altogether.

The application in this project, in the `app` dir, is a demo of
using the library to locate resources. It has no use other than as
a live example.

The `install.hs` script is deliberately not being compiled so that
it's flexible and hackable by developers to serve whatever additional
installation needs they may have for a given project. It's also
deliberately self-contained, relying on nothing other than core
libraries that ship with the GHC.


## Development

For developers who need to build against a local copy of hsinstall
I found this technique useful. Clone a copy of the source code and
install it locally:

      $ darcs clone http://hub.darcs.net/dino/hsinstall
      $ cd hsinstall
      $ stack install

In another project (nearby on your system, say), modify `stack.yaml`:

      extra-package-dbs:
      - ../hsinstall/.stack-work/install/x86_64-linux/lts-7.0/8.0.1/pkgdb

And then you should be able to build against this copy of
hsinstall. Of course, these are just examples, the version numbers
above will almost certainly be different.


## Contact

### Authors

Dino Morelli <dino@ui3.info>
