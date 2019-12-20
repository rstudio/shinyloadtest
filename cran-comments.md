# 2019-12-20 1.0.1

This is a re-submission addressing the following reviewer feedback e-mail:

> Please omit the redundant 'Tools for' from your description.
> 
> Please add more small executable examples in your Rd-files to illustrate
> the use of the exported function but also enable automatic testing.
> 
> Please add \value to .Rd files that are not data files and explain the
> functions results in the documentation.
> f.i.: shinyloadtest_report.Rd
> If a function does not return a value, please document that too, e.g.
> \value{None}.
> 
> Please fix and resubmit, and document what was changed in the submission
> comments.
> 
> Best,
> Jelena Saf

The following work was performed in response to reviewer feedback:

1. Redundant wording in DESCRIPTION was removed.
2. Return value was added to the shinyloadtest_report() documentation.
3. In an effort to add more runnable examples, I removed \dontrun from 
   the shinyloadtest_report() examples, but this fails on CRAN test 
   servers because the version of pandoc is not recent enough (< 2.2). 
   This is unfortunate, because shinyloadtest_report() exercises nearly 
   every other function in the package. Instead, we added a unit test 
   involving shinyloadtest_report() that runs on Travis CI.

# 2019-12-12 1.0.1

This is a re-submission because an incorrect version of the package was 
previously submitted as version 1.0.0. It also includes a bug fix.

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

* macOS 10.15.1: R 3.6.1
* Ubuntu 18.04: R 3.6.1
* Travis Ubuntu 16.04: oldrelease, release, devel
* win-builder: oldrelease, release, devel
* rhub: windows-x86_64-devel, ubuntu-gcc-release

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE:
  * checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Alan Dipert <barret@rstudio.com>'

    New submission


## Downstream dependencies

There are currently no downstream dependencies of this package.









-------------------------------------------------------------------------------
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
