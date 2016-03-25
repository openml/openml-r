context("makeOMLRunParList")

test_that("makeOMLRunParList", {
  nodesize = 1:2
  replace = c(TRUE, FALSE)
  
  for(ns in nodesize) {
    for (rep in replace) {
      lrn = makeLearner("classif.randomForest", ntree = 300, nodesize = ns, replace = rep)
      par.defaults = getDefaults(lrn$par.set)
      par.vals = lrn$par.vals
      # get names of parameters with values that differ from the defaults
      ind = vlapply(names(par.vals), function(x) !isTRUE(all.equal(par.defaults[[x]], par.vals[[x]])))
      par.diff = names(par.vals[ind])
      
      oml.par.list = makeOMLRunParList(lrn)
      expect_equal(extractSubList(oml.par.list, "name"), par.diff) 
    }
  }
})
