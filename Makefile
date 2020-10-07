build:
	make imgup
	make urlcpy
	stack build

imgup:
	cd jssrc/mediaupload && echo "javascript build..." && npm run build

urlcpy:
	cd jssrc/urlcpy && echo "javascript build..." && npm run build

run:
	./bin/rundev.sh

clean:
	rm -rf Houbou.cabal .stack-work/ yesod-devel jssrc/*/elm-stuff
