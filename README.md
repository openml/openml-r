# R interface to [OpenML](http://www.openml.org/) 

[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/OpenML)](http://cran.r-project.org/web/packages/OpenML)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/OpenML)](http://cran.rstudio.com/web/packages/OpenML/index.html)
[![Build Status](https://travis-ci.org/openml/openml-r.svg?branch=master)](https://travis-ci.org/openml/openml-r)
[![Build status](https://ci.appveyor.com/api/projects/status/mevevtyr538faqdv/branch/master?svg=true)](https://ci.appveyor.com/project/jakobbossek/openml-r/branch/master)
[![codecov](https://codecov.io/gh/openml/openml-r/branch/master/graph/badge.svg)](https://codecov.io/gh/openml/openml-r)
[![Chat on slack](https://img.shields.io/badge/chat-on%20slack-yellow.svg)](https://openml.slack.com/messages/C289V36JG)

OpenML aims to create a frictionless, collaborative environment for exploring machine learning. This R package allows to interface the [OpenML](http://www.openml.org/frontend/page/home) server to download datasets, tasks, runs and upload results (see  [Tutorial](http://openml.github.io/openml-r)).

# How to cite

To cite the OpenML R package in publications, please use our paper entitled [`OpenML`: An `R` Package to Connect to the Machine Learning Platform `OpenML`](http://dx.doi.org/10.1007/s00180-017-0742-2). You can also [download the Bibtex file.](https://citation-needed.springer.com/v2/references/10.1007/s00180-017-0742-2?format=bibtex&flavour=citation).

# Installation of the package

- Install the stable version from [CRAN](https://cran.r-project.org/web/packages/OpenML/index.html)
```r
install.packages("OpenML")
```
or

- Install the development version from GitHub (using `devtools`)
```r
devtools::install_github("openml/openml-r")
```

Furthermore, you need [farff](https://github.com/mlr-org/farff) installed to process [ARFF](http://www.cs.waikato.ac.nz/ml/weka/arff.html) files:
```r
install.packages("farff")
```
Alternatively you can make use of the [RWeka](https://cran.r-project.org/web/packages/RWeka/index.html) R package to process ARFF files. However, in particular for larger ARFF files, [farff](https://github.com/mlr-org/farff) is considerably faster than RWeka.

# Contact

Found some nasty bugs? Please use the [issue tracker](https://github.com/openml/openml-r/issues) to report on bugs or missing features. Pay attention to explain the problem as good as possible (in the best case with a `traceback()` result and a `sessionInfo()`). Moreover, a reproducible example is desirable.
