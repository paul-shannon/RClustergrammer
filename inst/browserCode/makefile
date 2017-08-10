build: webpack assemble pkg

pkg: buildPackage installPackage

assemble:
	(cd ./dist; sed s/\`//g bundle.js > bundleClean.js; m4 -P rclustergrammer.html-template > rclustergrammer.html)
	ls -l ./dist/rclustergrammer.html

buildPackage:
	(cd ../../..; R CMD build --no-build-vignettes RClustergrammer)

installPackage:
	(cd ../../..; R CMD INSTALL `ls -at RClustergrammer_* | head -1`)

webpack:
	npm update
	webpack

test:
	(cd ./dist; pwd)

all: clean install run

clean:
	- rm -rf dist/
	- rm -rf node_modules

install:
	npm install
