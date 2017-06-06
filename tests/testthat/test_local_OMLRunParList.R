context("OMLRunParList")

test_that("OMLRunParList", {
  nodesize = 1:2
  rf = makeLearner("classif.randomForest")
  lrn.list = list(
    rf,
    makeFilterWrapper(rf, fw.perc = 0.5, fw.method = "variance"),
    makeOversampleWrapper(rf, osw.rate = 1),
    makeImputeWrapper(rf, class = imputeMedian()),
    makeOversampleWrapper(makeFilterWrapper(rf, fw.method = "variance"), osw.rate = 1)
  )

  for(lrn in lrn.list) {
    for(ns in nodesize) {
      lrn = setHyperPars(lrn, ntree = 300, nodesize = ns)
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

      # check convertListToOMLRunParList
      expect_equal(oml.par.list,
        convertListToOMLRunParList(convertOMLRunParListToList(oml.par.list), component = extractSubList(oml.par.list, "component")))
    }
  }

  # test untyped param and vector param
  pars = list(sampsize = c(30, 30), mtry = 3, strata = iris$Species)
  lrn = makeLearner("classif.randomForest", par.vals = pars)
  expect_equal(convertOMLRunParListToList(makeOMLRunParList(lrn), getParamSet(lrn)), pars)
  expect_equal(pars,
    convertOMLRunParListToList(convertListToOMLRunParList(pars, getParamSet(lrn)), getParamSet(lrn)))

  # check getOMLRunParList
  with_test_cache({
    run = getOMLRun(1L)
    par = getOMLRunParList(run)
    expect_is(par, "OMLRunParList")
    expect_character(extractSubList(par, "name"))
    expect_character(extractSubList(par, "value"))
    expect_vector(extractSubList(par, "component"))
    for (i in seq_along(par)) expect_is(par[[i]], "OMLRunParameter")
    expect_data_frame(as.data.frame(par), nrow = 6, ncol = 3)
    expect_data_table(as.data.table(par), nrow = 6, ncol = 3)
    expect_equal(names(as.data.frame(par)), c("name", "value", "component"))
  })
})
