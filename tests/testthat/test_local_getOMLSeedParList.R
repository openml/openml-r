context("getOMLSeedParList")

test_that("getOMLSeedParList", {
  with_test_cache({
    lrn = makeLearner("classif.rpart", xval = 1)

    task = getOMLTask(59)
    res = runTaskMlr(task, lrn)
    
    par = getOMLSeedParList(res$run)
    expect_is(par, "OMLSeedParList")
    expect_equal(unname(extractSubList(par, "name")), c("openml.seed", "openml.kind", "openml.normal.kind")) 
    expect_is(par[[1]], "OMLRunParameter")
  })
})