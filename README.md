# R interface to [OpenML](http://www.openml.org/) 

[![CRAN Status Badge](http://www.r-pkg.org/badges/version/OpenML)](http://cran.r-project.org/web/packages/OpenML)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/OpenML)](http://cran.rstudio.com/web/packages/OpenML/index.html)
[![Build Status](https://travis-ci.org/openml/openml-r.svg?branch=master)](https://travis-ci.org/openml/openml-r)
[![Build status](https://ci.appveyor.com/api/projects/status/mevevtyr538faqdv/branch/master?svg=true)](https://ci.appveyor.com/project/jakobbossek/openml-r/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/openml/openml-r/badge.svg?branch=master)](https://coveralls.io/github/openml/openml-r?branch=master)
[![Issues in TODOs](https://badge.waffle.io/openml/openml-r.png?label=TODO&title=TODOs)](https://waffle.io/openml/openml-r)
[![Join the chat at https://gitter.im/openml/r](https://badges.gitter.im/openml/openml-r.svg)](https://gitter.im/openml/r?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

OpenML aims to create a frictionless, collaborative environment for exploring machine learning. This R package allows to interface the [OpenML](http://www.openml.org/frontend/page/home) server to download datasets, tasks, runs and upload results (see  [Tutorial](http://openml.github.io/openml-r)).

# Installation of the package

There is no CRAN release yet. Use `devtools` to install the current development version of the OpenML R package from GitHub:
```r
devtools::install_github("openml/openml-r")
```
Furthermore,  you need [farff](https://github.com/mlr-org/farff) installed to process [ARFF](http://www.cs.waikato.ac.nz/ml/weka/arff.html) files:
```r
install.packages("farff")
```
Alternatively you can make use of the [RWeka](https://cran.r-project.org/web/packages/RWeka/index.html) R package to process ARFF files. However, in particular for larger ARFF files, [farff](https://github.com/mlr-org/farff) is considerably faster than RWeka.

# Contact

Found some nasty bugs? Please use the [issue tracker](https://github.com/openml/openml-r/issues) to report on bugs or missing features. Pay attention to explain the problem as good as possible (in the best case with a `traceback()` result and a `sessionInfo()`). Moreover, a reproducible example is desirable.
