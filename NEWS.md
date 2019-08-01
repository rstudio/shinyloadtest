shinyloadtest 1.0.0.9000
=======

### Bug fixes

* Server type detection is now tolerant of invalid HTML/XML markup produced by
  the target application
  ([#115](https://github.com/rstudio/shinyloadtest/pull/115),
  [rstudio/shinycannon#38](https://github.com/rstudio/shinycannon/issues/38))

### Improvements

* The application server type is now captured in recording files. A
  corresponding enhancement was made to `shinycannon` that will produce a
  warning if the recording server type differs from the target
  ([#107](https://github.com/rstudio/shinyloadtest/pull/107),
  [rstudio/shinycannon#36](https://github.com/rstudio/shinycannon/pull/36))

