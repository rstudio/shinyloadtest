all: RELEASE_URLS.txt

.PHONY=clean site

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
