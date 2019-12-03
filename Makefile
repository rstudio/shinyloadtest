all: help

.PHONY=clean site help urls rcmdcheck build_site devinstall

help:
	@echo "make urls: update the shinycannon download links. You should do this after releasing shinycannon."
	@echo "make site: installs pkgs and build docs/vignettes and the doc site, which is hosted on github"
	@echo "make rcmdcheck: builds and checks using R CMD check --as-cran"
	@echo "make prep_vignettes: prepares vignettes files to produce images in pkgdown articles"
	@echo "make build_site: builds pkgdown site using scripts/build_docs.R"
	@echo "make clean: clean the docs and doc site"

# Updates RELEASE_URLS.csv file.
# Should be done manually when a new version of shinycannon is released.
# RELEASE_URLS.csv is read by index.Rmd
urls:
	rm -f RELEASE_URLS.csv
	wget https://s3.amazonaws.com/rstudio-shinycannon-build/RELEASE_URLS.csv

index.md: index.Rmd
	R --quiet --no-restore -e 'rmarkdown::render("index.Rmd", output_format = rmarkdown::md_document())'

devinstall:
	Rscript data-raw/slt_demo_data.R
	R -e 'devtools::document()'
	R CMD INSTALL --no-multiarch --with-keep.source .

site: index.md devinstall build_site

build_site: prep_vignettes
	R --quiet --no-restore -e 'unlink("./docs", recursive = TRUE); pkgdown::build_site()'

prep_vignettes: index.md
	HEADLESS=TRUE Rscript scripts/test_sessions.R && rm Rplots.pdf

rcmdcheck:
	R CMD check --as-cran `Rscript -e 'cat(devtools::build(quiet = TRUE))'`

clean:
	rm -f index.md
	rm -rf output
	find vignettes/test_sessions/ -mindepth 1 -type d -exec rm -rf '{}' ';'
