OpenML plugin for R
===================

[![Build Status](https://travis-ci.org/openml/r.svg)](https://travis-ci.org/openml/r)

Allows to interface the OpenML server to download tasks and upload results.
Work in progress.

### Installation of the package
* Install the latest versions of all dependencies from CRAN:
```r
install.packages(c("mlr", "checkmate", "data.table", "digest", "RCurl", "stringi", "XML", "RWeka", "devtools"))
```
* Install OpenML's current development version from Github:
```r
devtools::install_github("openml/r")
```
* After attaching the package, you are ready to start:
```r
library(OpenML)
```

### Tutorial
There's a short [tutorial](https://github.com/openml/r/blob/master/doc/knitted/1-Introduction.md) with which we want to help you get started. If you have further questions or issues, please use the [issue tracker](https://github.com/openml/r/issues).

