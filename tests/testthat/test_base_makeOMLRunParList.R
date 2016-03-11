context("makeOMLRunParList")

test_that("makeOMLRunParList", {
  nodesize = 1:2
  nsplit = 0:1
  
  for(ns in nodesize) {
    for (nsp in nsplit) {
      lrn = makeLearner("classif.randomForestSRC", ntree = 300, nodesize = ns, nsplit = nsp)
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
