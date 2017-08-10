setupClient:
	cd client; stack setup

buildClient:
	cd client; stack build

buildClientWithStatic:
	cd client; stack build;  cp -Rf ../static/ $$(stack path --local-install-root)/bin/InnerEarClient.jsexe/;

buildClientWithStaticForced:
	cd client; stack build --force-dirty;  cp -Rf ../static/ $$(stack path --local-install-root)/bin/InnerEarClient.jsexe/;


openClient:
	cd client; open $$(stack path --local-install-root)/bin/InnerEarClient.jsexe/index.html

setupServer:
	cd server; stack setup

buildServer:
	cd server; stack build
