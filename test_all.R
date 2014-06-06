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
  library(OpenML)  
}
test_dir("tests/testthat")
