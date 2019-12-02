# 2019-12-01 1.0.0

This is a re-submission addressing the following reviewer feedback e-mail:

> The Description field is intended to be a (one paragraph) description
> of what the package does and why it may be useful. Please elaborate.
> 
> Please add \value to .Rd files that are not data files and explain the 
> functions results in the documentation.
> f.i.: slt_plot.Rd
> If a function does not return a value, please document that too, e.g. 
> \value{None}.
> 
> Please add small executable examples in your Rd-files to illustrate the 
> use of the exported function but also enable automatic testing.
> 
> Please fix and resubmit, and document what was changed in the submission 
> comments.
> 
> Best,
> Jelena Saf

1. The Description field in DESCRIPTION has been expanded.
2. Added \value to all the slt_* function documentation

TODO: add examples and value to load_runs, record_session, shinyloadtest_report

## Test environments

* Ubuntu 18.04, R 3.6.1
* Ubuntu 16.04, R 3.5, R 3.6, devel
* win-builder

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies

There are currently no downstream dependencies of this package.

# 2019-11-22 1.0.0

This is the initial release of shinyloadtest to CRAN.

## Test environments

* Ubuntu 18.04, R 3.6.1
* Ubuntu 16.04, R 3.5, R 3.6, devel
* win-builder

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies

There are currently no downstream dependencies of this package.
