2.2 (2018-11-07)

  * Added actual exception handling
  * Reworded usage and README text


2.1 (2018-10-22)

  * Switched to a a lighter-weight here-document library
  * Updated API documentation


2.0 (2018-10-22)

  * Redesigned as a binary utility to be installed on a system, not a script
  * Added AppImage creation feature
  * Got rid of "bundle" style deployment, it's all FHS now with a prefix
  * Removed old sample usage app
  * Moved stackage resolver up to lts-12.13
  * Moved copyright date up to 2018


1.6 (2017-07-01)

  * Changed base lower bound from 4.9 to 4.8
  * Updated stack snapshot to lts-8.21
  * Added HCAR listing content
  * Removed defunct cabal stability field
  * Adjusted some documentation in the README
  * Moved copyright date up to 2017


1.5 (2016-10-16)

  * Now creating bin directory prior to stack install
  * Removed comments from auto-generated stack.yaml


1.4 (2016-10-11)

  * Added missing files to extra-source-files
  * Added switch for making a symlink to the app directory


1.3 (2016-10-07)

  * Fixed error in docs


1.2 (2016-10-07)

  * Added example additional script copying code
  * Updated developer instructions
  * Added a tested-with line to the cabal file


1.1 (2016-10-07)

  * Updated to stackage lts-7.2


1.0 (2016-10-02)

  * Cleaned up cabal file
  * Wrote API docs
  * Cleaned up README
  * Aborting the installation if `stack install` fails
  * Added instructions for compiling install.hs
  * Added library for locating resources at runtime
  * Initial release
