# Building the Inner Ear client

Make sure you have node.js installed (see note below) and an up to date version of stack (i.e. Haskell Stack) installed, then change into the client subdirectory of the InnerEar project and do stack setup followed by stack build. The first
time you do this it may take a very long time as stack downloads and boots the GHCJS Haskell-to-Javascript
compiler:

````
cd ~/InnerEar/client
stack setup
stack build
````

Note: a likely source of opaque problems during stack setup and/or stack build is incompatible versions of node.js. Downgrading to a specific version of node.js is often a solution. At the time of writing node-6.2.2 is confirmed as working on some systems.

# Building the Inner Ear server

Make sure you have an up to date version of stack (i.e. Haskell Stack) installed, then change into the
server subdirectory of the InnerEar project and do stack setup followed by stack build. The first
time you do this it may take a somewhat long time as stack downloads and sets up a sandboxed version of
GHC, the Glasgow Haskell Compiler.

````
cd ~/InnerEar/server
stack setup
stack build
````
