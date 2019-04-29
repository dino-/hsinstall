# hsinstall


## Synopsis

Install Haskell software


## Description

### OVERVIEW

hsinstall is a tool for deploying software projects into directory structures
suitable for installation on a system. It builds upon the `stack install`
command and adds these features:

- Copying the `LICENSE` file into the deployment directory
- Copying a static directory stucture (named `pack`) onto the destination
  prefix directory that can contain additional binaries or scripts, resources,
  documentation, etc. (more on this later in PACK DIRECTORY)
- Building an AppDir directory structure for a project and producing an
  [AppImage](https://appimage.org/)

It will be necessary to have the Haskell stack tool on your PATH:  
https://docs.haskellstack.org/en/stable/README/

If the AppImage features are desired, you must have these tools on your PATH:
linuxdeploy: https://github.com/linuxdeploy/linuxdeploy/releases
linuxdeploy-plugin-appimage: https://github.com/linuxdeploy/linuxdeploy-plugin-appimage/releases

### MODES

hsinstall operates in two ways: build a distribution directory or build an AppImage.

If the -i,--mk-appimage switch is omitted, hsinstall will construct a
distribution directory containing all binaries in the project, the license file
and the contents of the `pack` directory if present. This could be used as the
source for a distribution-specific packaging procedure or used as-is on a
non-Linux system. The default prefix is `AppDir/usr`

The second mode includes everything above plus AppImage creation and is
triggered by the -i,--mk-appimage switch. This will change the PREFIX to
`<EXE>.AppDir/usr`. And only that single executable will be copied to the
`<EXE>.AppDir/usr/bin` directory. AppImages are intended to contain exactly one
binary each.

Regardless of which mode is being used, the directory layout will be a standard
[FHS](http://www.pathname.com/fhs/) shape, common in UNIX-like operating systems. Like this:

    <PREFIX>/
      bin/  <- stack will install your binaries here
      share/
        <PROJECT-NAME>/  <-- this is the share directory
          doc/LICENSE
          resources/  <-- Optional data files directory, see PACK DIRECTORY below

### APPIMAGE CREATION

Even for a first-time AppImaging, this tool should produce a working AppImage.
If missing, it will create default `.desktop` and `.svg` files in `pack/share`.
Customize or replace these to fit your project, and then check these two files
into source control for future builds. For more info, see PACK DIRECTORY below.

The default `.desktop` file Categories will be populated with 'Utility;'. We
recommend adjusting this using the XDG list of registered categories:
https://specifications.freedesktop.org/menu-spec/latest/apa.html

If your application is a command-line program, append this line to the end of
the default `.desktop` file: 'Terminal=true'

If your application isn't a command-line tool, we recommend using a proper icon
instead of the hsinstall default, which is a command shell icon.

### PACK DIRECTORY

If present, hsinstall will copy the contents of the `pack` directory onto
`<PREFIX>`. Here's a sample of a fully-featured pack directory:

    pack/
      bin/  <-- Put additional binaries and scripts to be deployed here
      share/
        applications/  <-- Only for AppImage
          <EXE>.desktop  <-- Will be generated by first-time AppImage creation attempt
        <PROJECT-NAME>/  <-- Only needed if you have resources
          resources/  <-- Put data files the software will need at runtime here
        icons/  <-- Only for AppImage
          hicolor/
            scalable/
              apps/
                <EXE>.svg  <-- Will be generated by first-time AppImage creation attempt

In order to locate the resources files at runtime, the hsinstall project
includes a library to construct paths relative to the executable. See this
source code for help with integrating this into your app:
https://github.com/dino-/hsinstall/blob/master/src/lib/HSInstall/Resources.hs


## Development

Browse [the source](https://github.com/dino-/hsinstall)

Get source with git and build

    $ git clone https://github.com/dino-/hsinstall.git
    $ cd hsinstall
    $ stack build
    $ stack haddock --no-haddock-deps hsinstall

If you have the abovementioned `linuxdeploy-*` programs on your path, we can do
something *really* cool. Use this freshly-built hsinstall to package itself
into an AppImage:

    $ stack exec hsinstall -- --mk-appimage hsinstall

And you should see an `hsinstall-y.z-x86_64.AppImage` binary in `.`

Tip: Use `git clean -df` to blow away untracked things like the AppDir and
AppImage artifacts.


## Contact

### Authors

Dino Morelli <dino@ui3.info>
