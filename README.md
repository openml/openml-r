# R interface to [OpenML.org](http://www.openml.org/) 

[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![Rdoc](http://www.rdocumentation.org/badges/version/OpenML)](http://www.rdocumentation.org/packages/OpenML)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/OpenML)](http://cran.r-project.org/web/packages/OpenML)
[![Build Status](https://travis-ci.org/openml/openml-r.svg?branch=master)](https://travis-ci.org/openml/openml-r)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/OpenML)](http://cran.rstudio.com/web/packages/OpenML/index.html)
[![codecov](https://codecov.io/gh/openml/openml-r/branch/master/graph/badge.svg)](https://codecov.io/gh/openml/openml-r)

[OpenML.org](http://www.openml.org/frontend/page/home) is an online machine learning platform where researchers can access open data, download and upload data sets, share their machine learning tasks and experiments and organize them online to work and collaborate with other researchers. 
The R interface allows to query for data sets with specific properties, and allows the downloading and uploading of data sets, tasks, flows and runs.

For more information, have a look at our
- <a href="https://openml.github.io/openml-r/articles/OpenML.html" target="_blank">R Tutorial</a>
- <a href="https://github.com/openml/openml-r/blob/master/vignettes/openml-cheatsheet.pdf" target="_blank">R Cheatsheet</a>
- <a href="https://openml.github.io/openml-r/reference" target="_blank">Function Reference</a>
- <a href="http://dx.doi.org/10.1007/s00180-017-0742-2" target="_blank">OpenML R Package Publication</a>
- <a href="https://www.openml.org/api_docs" target="_blank">OpenML API Guide</a>

# How to cite

To cite the OpenML R package in publications, please use our paper entitled <a href="http://dx.doi.org/10.1007/s00180-017-0742-2" target="_blank">`OpenML`: An `R` Package to Connect to the Machine Learning Platform `OpenML`</a> [<a href="https://citation-needed.springer.com/v2/references/10.1007/s00180-017-0742-2?format=bibtex&flavour=citation" target="_blank">BibTex</a>]

See also <a href="https://www.openml.org/cite" target="_blank">here</a> how to cite the OpenML project itself.

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
