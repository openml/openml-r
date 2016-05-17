context("makeOMLRunParList")

test_that("makeOMLRunParList", {
  nodesize = 1:2
  replace = c(TRUE, FALSE)
  
  rf = makeLearner("classif.randomForest")
  
  lrnList = list(
    rf,
    makeFilterWrapper(rf, fw.perc = 0.5),
    makeOversampleWrapper(rf, osw.rate = 1),
    makeImputeWrapper(rf, class = imputeMedian()),
    makeOversampleWrapper(makeFilterWrapper(rf), osw.rate = 1)
  )
  
  for(lrn in lrnList) {
    for(ns in nodesize) {
      for (rep in replace) {
        #lrn = makeLearner("classif.randomForest", ntree = 300, nodesize = ns, replace = rep)
        lrn = setHyperPars(lrn, ntree = 300, nodesize = ns, replace = rep)
        par.defaults = getDefaults(getParamSet(lrn))
        par.vals = getHyperPars(lrn)
        # get names of parameters with values that differ from the defaults
        ind = vlapply(names(par.vals), function(x) !isTRUE(all.equal(par.defaults[[x]], par.vals[[x]])))
        par.diff = names(par.vals[ind])
        
        oml.par.list = makeOMLRunParList(lrn)
        expect_is(oml.par.list, "OMLRunParList")
        expect_equal(unname(extractSubList(oml.par.list, "name")), par.diff) 
        expect_subset(unname(extractSubList(oml.par.list, "component")), unlist(strsplit(lrn$id, "[.]"))[-1])
        for (i in seq_along(oml.par.list)) expect_is(oml.par.list[[i]], "OMLRunParameter")
        
        # check isSeedPar
        expect_true(all(!isSeedPar(oml.par.list)))
        
        # check convertOMLRunParListToList
        expect_equal(convertOMLRunParListToList(oml.par.list), lapply(par.vals[ind], as.character))
      }
    }
  }
})
