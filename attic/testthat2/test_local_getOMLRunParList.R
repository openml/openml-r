context("getOMLRunParList")

test_that("getOMLRunParList", {
  with_test_cache({
    lrn = makeLearner("classif.rpart", xval = 1)
    par.defaults = getDefaults(getParamSet(lrn))
    par.vals = getHyperPars(lrn)
    # get names of parameters with values that differ from the defaults
    ind = vlapply(names(par.vals), function(x) !isTRUE(all.equal(par.defaults[[x]], par.vals[[x]])))
    par.diff = names(par.vals[ind])
    
    task = getOMLTask(59)
    res = runTaskMlr(task, lrn)
    
    par = getOMLRunParList(res$run)
    expect_is(par, "OMLRunParList")
    expect_equal(unname(extractSubList(par, "name")), par.diff) 
    expect_subset(unname(extractSubList(par, "component")), unlist(strsplit(lrn$id, "[.]"))[-1])
    expect_is(par[[1]], "OMLRunParameter")
  })
})