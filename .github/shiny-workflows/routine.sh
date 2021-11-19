#!/bin/bash -e

make demo_data
git add man data || echo "No data updates to add"
git commit -m 'Resave data (GitHub Action)' || echo "No data changes to commit"

make urls
git add vignettes/RELEASE_URLS.csv
git commit -m 'Update shinycannon RELEASE_URLS.csv (GitHub Actions)' || echo "No release url changes to commit"
