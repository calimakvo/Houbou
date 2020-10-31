build:
	make imgup
	make urlcpy
	make prev
	stack build

imgup:
	cd jssrc/mediaupload && echo "javascript build..." && npm run build

urlcpy:
	cd jssrc/urlcpy && echo "javascript build..." && npm run build

prev:
	cd jssrc/preview && echo "javascript build..." && npm run build

run:
	./bin/rundev.sh

clean:
	rm -rf Houbou.cabal .stack-work/ yesod-devel jssrc/*/elm-stuff
