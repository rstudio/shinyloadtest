all: help

.PHONY=clean urls site readme devinstall build_site prep_vignettes rcmdcheck demo_data

help:
	@echo "make urls: update the shinycannon download links. You should do this after releasing shinycannon."
	@echo "make site: installs pkgs and build docs/vignettes and the doc site, which is hosted on github"
	@echo "make readme: builds README.Rmd"
	@echo "make demo_data: update the latest demo data such as `slt_demo_data_4"
	@echo "make devinstall: install shinyloadtest"
	@echo "make prep_vignettes: prepares vignettes files to produce images in pkgdown articles"
	@echo "make rcmdcheck: builds and checks using R CMD check --as-cran"
	@echo "make clean: clean the docs and doc site"

# Updates RELEASE_URLS.csv file.
# Should be done manually when a new version of shinycannon is released.
# RELEASE_URLS.csv is read by index.Rmd
urls:
	Rscript scripts/fetch_release_urls.R

site: devinstall readme prep_vignettes urls
	R --quiet --no-restore -e 'unlink("./docs", recursive = TRUE); pkgdown::build_site(new_process = FALSE)'

readme: README.Rmd
	R --quiet --no-restore -e 'devtools::build_readme(quiet = FALSE)'

demo_data:
	R CMD INSTALL --no-multiarch --with-keep.source .
	Rscript data-raw/slt_demo_data.R

devinstall: demo_data
	R --quiet --no-restore -e 'devtools::document()'
	R CMD INSTALL --no-multiarch --with-keep.source .

prep_vignettes: devinstall
	HEADLESS=TRUE Rscript scripts/test_sessions.R && rm Rplots.pdf

rcmdcheck:
	R CMD check --as-cran `Rscript -e 'cat(devtools::build(quiet = TRUE))'`



clean_docs:
	rm -rf docs
clean_readme:
	rm -f README.md
clean_output:
	rm -rf output
clean_test_sessions:
	find vignettes/test_sessions -mindepth 1 -maxdepth 1 -type d -exec rm '-rf' '{}' ';'

clean: clean_docs clean_readme clean_output clean_test_sessions
