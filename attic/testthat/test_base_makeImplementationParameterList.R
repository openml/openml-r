context("makeImplementationParameterList")

test_that("makeImplementationParameterList", {
  lrn = makeLearner("classif.JRip")
  par.list = makeImplementationParameterList(lrn)
  for (i in seq_along(par.list)) {
    expect_is(par.list[[i]], "OpenMLImplementationParameter")
  }
  
  bag = makeBaggingWrapper(lrn)
  par.list.2 = makeImplementationParameterList(bag)
  
  expect_true(all(extractSubList(par.list, "name") %nin% extractSubList(par.list.2, "name")))
})
