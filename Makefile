all: help

.PHONY=clean site help

help:
	@echo "make RELEASE_URLS.txt: update the shinycannon download links. You should do this after releasing shinycannon."
	@echo "make site: build docs/vignettes and the doc site, which is hosted on github"
	@echo "make clean: clean the docs and doc site"

# Updates RELEASE_URLS.txt file.
# Should be done manually when a new version of shinycannon is released.
# RELEASE_URLS.txt is read by index.Rmd
RELEASE_URLS.txt:
	rm -f $@
	wget https://s3.amazonaws.com/rstudio-shinycannon-build/$@

site:
	R -e 'devtools::document()'
	R CMD INSTALL --no-multiarch --with-keep.source .
	HEADLESS=TRUE Rscript scripts/test_sessions.R && rm Rplots.pdf
	Rscript scripts/build_vignettes.R
	Rscript scripts/build_docs.R

clean:
	rm -rf output
	find vignettes/test_sessions/ -mindepth 1 -type d -delete
