SHELL := $(shell which bash)
VERSION_PATTERN := [0-9]\.[0-9]\.[0-9]-[0-9a-f]\{7\}

all: README.md

RELEASE_URLS.txt:
	wget https://s3.amazonaws.com/rstudio-shinycannon-build/$@

README.md: README.md.template RELEASE_URLS.txt
	SHINYCANNON_VERSION=$$(sed -e 's/.*\($(VERSION_PATTERN)\).*/\1/' RELEASE_URLS.txt | head -1 | tr -d '\n')\
		envsubst < $< > $@
	paste <(awk -F "." '{printf("[shinycannon_%s]:\n", $$NF)}' RELEASE_URLS.txt) RELEASE_URLS.txt >> $@

.PHONY: clean
clean:
	rm -f RELEASE_URLS.txt README.md shinycannon_version.txt
