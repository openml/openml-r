context("makeRunParameterList")

test_that("makeRunParameterList", {
  lrn <- makeLearner("classif.J48")
  run.pars <- makeRunParameterList(lrn)
  expect_true(length(run.pars) == 0)
  
  lrn <- makeLearner("classif.J48", par.vals=list(U = TRUE, M = 4))
  run.pars <- makeRunParameterList(lrn)
  expect_true(is.list(run.pars) && length(run.pars) == 2 && run.pars[[1]]@name == "U" &&
    run.pars[[1]]@value == "TRUE" && run.pars[[2]]@name == "M" && run.pars[[2]]@value == "4" &&
    length(run.pars[[1]]@component) == 0 && length(run.pars[[2]]@component) == 0)
  
  lrn <- makeLearner("classif.J48", M = 4)
  run.pars <- makeRunParameterList(lrn, component="subcomponent")
  expect_true(is.list(run.pars) && length(run.pars) == 1 && run.pars[[1]]@name == "M" &&
    run.pars[[1]]@value == "4" && run.pars[[1]]@component == "subcomponent")
})  