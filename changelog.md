2.6 (2020-01-12)

  * Moved stack resolver up to lts-14.20
  * Switched from fmt library to Text.Printf
  * Try to force hpack cabal file creation if it's missing
  * Moved copyright date up to 2020
  * Fixed various problems in the README


2.5 (2019-12-01)

  * Using an infoHelper for the --version switch
  * Moved Stackage snapshot up to lts-14.8
  * Updated usage to reflect the new template dir behavior
  * Changed lib/HSInstall/Resources.hs to Paths.hs
  * Added custom code for recursive file copying
  * Fixed bug with default prefix directory
  * Removed the --delete switch
  * Rolled EXE argument into --mk-appimage switch
  * Changed how resources and AppImage source files are handled
  * Added a handy tip about git clean
  * Removed code to force cabal file creation


2.4 (2019-04-26)

  * Moved copyright date up to 2019
  * Separated code into more modules for clarity
  * Switched from GetOpt to optparse-applicative
  * Fixed API doc wording
  * Adjusted code with hlint's advice
  * Now trying to force stack/hpack to generate the cabal file if it's missing
  * Added app version number to AppImage filename


2.3 (2019-03-03)

  * Changed deployment dir name to EXECUTABLE.AppDir
  * Switched all uses of Text.Printf to the Fmt library
  * Moved Stackage resolver up to lts-13.9


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


1.1 (2016-10-03)

  * Updated to stackage lts-7.2


1.0 (2016-10-03)

  * Cleaned up cabal file
  * Wrote API docs
  * Cleaned up README
  * Aborting the installation if `stack install` fails
  * Added instructions for compiling install.hs
  * Added library for locating resources at runtime
  * Initial release
