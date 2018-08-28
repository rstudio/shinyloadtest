all: RELEASE_URLS.txt

# Updates RELEASE_URLS.txt file.
# Should be done manually when a new version of shinycannon is released.
# RELEASE_URLS.txt is read by index.Rmd
RELEASE_URLS.txt:
	wget https://s3.amazonaws.com/rstudio-shinycannon-build/$@

.PHONY=clean
clean:
	rm -f RELEASE_URLS.txt
