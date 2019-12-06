# 2019-12-06 1.0.0

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

The following changes addressing reviewer feedback were made:

1. The Description field in DESCRIPTION has been expanded.
2. \value was added to all slt_* function documentation
3. Running \examples were added to the slt_plot topic
4. \value was added to load_runs()
5. A \dontrun \example was added to record_session(). \dontrun used because record_session() starts a server.
6. A \dontrun \example was added to shinyloadtest_report(). \dontrun used because CRAN servers don't have a recent enough pandoc for it to work.
7. Example datasets suitable for passing to shinyloadtest_report() were added: slt_demo_data_1, slt_demo_data_4, and slt_demo_data_16

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
