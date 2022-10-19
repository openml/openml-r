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

  for (lrn in lrn.list) {
    for (ns in nodesize) {
      args = list(ntree = 300, nodesize = ns, replace = TRUE)
      lrn = setHyperPars(lrn, par.vals = args)
      par.defaults = ParamHelpers::getDefaults(ParamHelpers::getParamSet(lrn))
      par.vals = getHyperPars(lrn)

      oml.par.list = makeOMLRunParList(lrn)
      expect_s3_class(oml.par.list, "OMLRunParList")
      expect_list(oml.par.list, types = "OMLRunParameter")

      # check if all parameters from par.vals are included (especially default values)
      expect_equal(names(oml.par.list), names(par.vals))
      expect_subset(extractSubList(oml.par.list, "component", use.names = FALSE), unlist(strsplit(lrn$id, "[.]"))[-1])

      # check isSeedPar
      expect_true(all(!isSeedPar(oml.par.list)))

      # check convertOMLRunParListToList
      expect_equal(convertOMLRunParListToList(oml.par.list), lapply(par.vals, as.character))

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
    expect_s3_class(par, "OMLRunParList")
    expect_character(extractSubList(par, "name"))
    expect_character(extractSubList(par, "value"))
    expect_atomic_vector(extractSubList(par, "component"))
    for (i in seq_along(par)) expect_s3_class(par[[i]], "OMLRunParameter")
    expect_data_frame(as.data.frame(par), nrow = 6, ncol = 3)
    expect_data_table(as.data.table(par), nrow = 6, ncol = 3)
    expect_equal(names(as.data.frame(par)), c("name", "value", "component"))
  })
})

