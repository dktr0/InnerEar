#### Note for Windows users

On Windows 10 there is a feature called `Microsoft-Windows-Subsystem-Linux` which is the recommended way to build the project. Follow the [microsoft installation instructions](https://docs.microsoft.com/en-us/windows/wsl/install-win10) and choose a distribution of your choice (tested with the Ubuntu distribution). Use the newly installed shell to build Inner Ear. 

With the Ubuntu distribution you can access your regular windows drives through the `/mnt` directory and from windows you can find your files at `%LOCALAPPDATA%\Packages\CanonicalGroupLimited.UbuntuonWindows_79rhkp1fndgsc\LocalState\rootfs` but becareful when editing these files through windows apps as this is not supported and may cause data corruption. You can build the project and boot the server from within the subsystem and still use your regular browser to visit `localhost:<port>` on the port that the server is running on. See `make buildReleaseTest`.

# Building the Inner Ear client

Make sure you have node.js installed (see note below) and an up to date version of stack (i.e. Haskell Stack) installed, then change into the client subdirectory of the InnerEar project and do stack setup followed by stack build. The first
time you do this it may take a very long time as stack downloads and boots the GHCJS Haskell-to-Javascript
compiler:

```
cd ~/InnerEar/client
stack setup
stack build
```

Note: a likely source of opaque problems during stack setup and/or stack build is incompatible versions of node.js. Downgrading to a specific version of node.js is often a solution. At the time of writing node-6.2.2 is confirmed as working on some systems.

# Building the Inner Ear server

Make sure you have an up to date version of stack (i.e. Haskell Stack) installed, then change into the
server subdirectory of the InnerEar project and do stack setup followed by stack build. The first
time you do this it may take a somewhat long time as stack downloads and sets up a sandboxed version of
GHC, the Glasgow Haskell Compiler.

```
cd ~/InnerEar/server
stack setup
stack build
```
