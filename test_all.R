library(methods)
library(devtools)
library(testthat)
library(mlr)

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
