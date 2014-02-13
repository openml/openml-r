library(methods)
library(devtools)
library(testthat)

if (interactive()) {
  library(BBmisc)
  library(XML)
  library(RCurl)
  library(RWeka)
  library(mlr)
  load_all(".")
} else {
  library(openML)  
}
test_dir("inst/tests")
