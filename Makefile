setupClient:
	cd client && stack setup

buildClient:
	cd client && stack build
	cd client && cp -Rf ../static/* $$(stack path --local-install-root)/bin/InnerEarClient.jsexe/

staticClient:
	cd client && cp -Rf ../static/* $$(stack path --local-install-root)/bin/InnerEarClient.jsexe/	

buildClientForceDirty:
	cd client && stack build --force-dirty
	cd client && cp -Rf ../static/* $$(stack path --local-install-root)/bin/InnerEarClient.jsexe/

cleanClient:
	cd client && stack clean

bootTemporaryWebServer:
	cd client && cd $$(stack path --local-install-root)/bin/InnerEarClient.jsexe/; npm install
	cd client && node $$(stack path --local-install-root)/bin/InnerEarClient.jsexe/temporaryWebServer.js;

openClient:
	cd client && open $$(stack path --local-install-root)/bin/InnerEarClient.jsexe/index.html

setupServer:
	cd server && stack setup

buildServer:
	cd server && stack build --ghc-options='-threaded'

cleanServer:
	cd server && stack clean

bootServer:
	cd release && ./InnerEarServer

release:
	-rm -rf release
	mkdir release
	cd client && cp -Rf $$(stack path --local-install-root)/bin/InnerEarClient.jsexe ../release/
	cd server && cp -Rf $$(stack path --local-install-root)/bin/InnerEarServer ../release/InnerEarServer

updateClientWithRunningServer: buildClient
	cd client && cp -Rf $$(stack path --local-install-root)/bin/InnerEarClient.jsexe ../release/

buildReleaseTest: buildClient buildServer release bootServer

ghciServer:
	cd server && stack ghci


# development builds

PRODUCTION_WORK_DIR=.stack-work-production/
PRODUCTION_INSTALL_DIR=$$(stack --work-dir $(PRODUCTION_WORK_DIR) path --local-install-root)/bin/InnerEarClient.jsexe

prodBuildClient:
	cd client && stack --work-dir $(PRODUCTION_WORK_DIR) build --ghc-options="-DGHCJS_BROWSER -O2"
	cd client && google-closure-compiler "$(PRODUCTION_INSTALL_DIR)/all.js" --compilation_level=ADVANCED_OPTIMIZATIONS --jscomp_off=checkVars --js_output_file="$(PRODUCTION_INSTALL_DIR)/all.min.js"
	cd client && gzip -fk $(PRODUCTION_WORK_DIR)/all.min.js

releaseProdClient:
	cd client && cp -Rf $(PRODUCTION_INSTALL_DIR) ../release/
	cd client && cp -Rf ../static/* ../release/InnerEarClient.jsexe/
	cd client && cp $(PRODUCTION_INSTALL_DIR)/all.min.js.gz ../release/InnerEarClient.jsexe/
	cp client/index.html release/InnerEarClient.jsexe/index.html

# profile builds

profBuildClient:
	cd client && stack build --profile
