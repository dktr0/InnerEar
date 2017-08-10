setupClient:
	cd client; stack setup

buildClient:
	cd client; stack build

buildClientWithStatic:
	cd client; stack build;  cp -Rf ../static/ $$(stack path --local-install-root)/bin/InnerEarClient.jsexe/;

buildClientWithStaticForced:
	cd client; stack build --force-dirty;  cp -Rf ../static/ $$(stack path --local-install-root)/bin/InnerEarClient.jsexe/;

bootTemporaryWebServer:
	cd client; cd $$(stack path --local-install-root)/bin/InnerEarClient.jsexe/; npm install
	cd client; node $$(stack path --local-install-root)/bin/InnerEarClient.jsexe/temporaryWebServer.js;


openClient:
	cd client; open $$(stack path --local-install-root)/bin/InnerEarClient.jsexe/index.html

setupServer:
	cd server; stack setup

buildServer:
	cd server; stack build

bootServer:
	cd server; cd $$(stack path --local-install-root)/bin/; ./InnerEarServer
