# `shinyloadtest` Release Notes

## 0.2.0 - **Breaking Release**  

`shinyloadtest` underwent significant changes between the prototype 0.1.0 and 0.2.0. The original prototype used parallel R procceses, [shinytest](https://github.com/rstudio/shinytest), and phantomJS to run load tests. The new implementation uses a custom built node tool called [proxyrec](https://github.com/rstudio/proxyrec).

The new tools is more efficient at generating load. Be aware of the known limitations:

- sub apps can not be tested
- apps with file uploads can not be tested

The helper addin was also deprecated in favor of users directly interacting with [proxyrec](https://github.com/rstudio/proxyrec).

## 0.1.0

This prototype release has been deprecated, but is still available at: https://github.com/rstudio/shinyloadtest/releases.
