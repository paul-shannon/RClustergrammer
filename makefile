RcodeOnly:
	(cd ..; R CMD build --no-build-vignettes RClustergrammer && R CMD INSTALL `ls -at RClustergrammer_* | head -1`)
