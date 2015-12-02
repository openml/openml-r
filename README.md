[![Stories in Ready](https://badge.waffle.io/openml/r.png?label=ready&title=Ready)](https://waffle.io/openml/r)
OpenML plugin for R
===================

[![Build Status](https://travis-ci.org/openml/r.svg)](https://travis-ci.org/openml/r)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/OpenML)](http://cran.r-project.org/web/packages/OpenML)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/OpenML)](http://cran.rstudio.com/web/packages/OpenML/index.html)
[![Coverage Status](https://coveralls.io/repos/openml/r/badge.svg?branch=master&service=github)](https://coveralls.io/github/openml/r?branch=master)

Allows to interface the [OpenML](http://www.openml.org/frontend/page/home) server to download tasks and upload results. This package is still *work in progress*.

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
There's a short [tutorial](https://github.com/openml/r/blob/master/doc/knitted/1-Introduction.md) with which we want to help you to get started. If you have further questions or issues, please use the [issue tracker](https://github.com/openml/r/issues).

