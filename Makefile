all: help

.PHONY=clean site help urls devcheck devbuild devdocument devinstall

help:
	@echo "make urls: update the shinycannon download links. You should do this after releasing shinycannon."
	@echo "make site: build docs/vignettes and the doc site, which is hosted on github"
	@echo "make devcheck: runs devtools::check() without building vignettes, since we have a separate/special process for doing that (make site, above)"
	@echo "make devbuild: runs devtools::build() without building vignettes, since we have a separate/special process for doing that (make site, above)"
	@echo "make clean: clean the docs and doc site"

# Updates RELEASE_URLS.csv file.
# Should be done manually when a new version of shinycannon is released.
# RELEASE_URLS.csv is read by index.Rmd
urls:
	rm -f RELEASE_URLS.csv
	wget https://s3.amazonaws.com/rstudio-shinycannon-build/RELEASE_URLS.csv

index.md: index.Rmd
	R --quiet --no-restore -e 'rmarkdown::render("index.Rmd", output_format = rmarkdown::md_document())'

site: index.md devinstall
	HEADLESS=TRUE Rscript scripts/test_sessions.R && rm Rplots.pdf
	Rscript scripts/build_vignettes.R
	Rscript scripts/build_docs.R

devdocument:
	R --quiet --no-restore -e 'devtools::document()'

devinstall: devdocument
	R CMD INSTALL --no-multiarch --with-keep.source .

devcheck:
	R --quiet --no-restore -e 'devtools::check(vignettes = FALSE)'

devbuild:
	R --quiet --no-restore -e 'devtools::build(vignettes = FALSE)'

clean:
	rm -f index.md
	rm -rf output
	find vignettes/test_sessions/ -mindepth 1 -type d -exec rm -rf '{}' ';'
