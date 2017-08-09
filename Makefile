setupClient:
	cd client; stack setup

buildClient:
	cd client; stack build

openClient:
	cd client; open $$(stack path --local-install-root)/bin/InnerEarClient.jsexe/index.html

setupServer:
	cd server; stack setup

buildServer:
	cd server; stack build
