context("getOMLFlow")

test_that("getOMLFlow", {
  with_test_cache({
    expect_error(getOMLFlow(430408, cache.only = TRUE), "not found in cache")
    flow = getOMLFlow(2)
    expect_is(flow, "OMLFlow")
    expect_output(print(flow), "Flow")
    expect_equal(flow$flow.id, 2)
 })
  
  with_empty_cache({
    flow = getOMLFlow(3880)
    expect_is(flow, "OMLFlow")
    expect_output(print(flow), "Flow|3880")
    expect_string(flow$binary.url)
    expect_equal(flow$binary.format, "Rds")
    expect_file(flow$binary.path)
    expect_string(flow$binary.md5)
    for (i in seq_along(flow$parameters)) 
      expect_output(print(flow$parameters[[i]]), "Parameter")

    # flow converter
    lrn1 = convertOMLFlowToMlrLearner(flow)
    expect_is(lrn1, "Learner")
    converted.flow = convertMlrLearnerToOMLFlow(lrn1)
    expect_is(converted.flow, "OMLFlow")
    
    # check 
    lrn = makeOversampleWrapper(makeFilterWrapper(makeImputeWrapper(makeLearner("classif.logreg"), 
      classes = list(numeric = imputeMedian(), integer = imputeMedian())), fw.perc = 0.5, fw.method = "variance"))
    expect_equal(removeAllHyperPars(lrn), lrn1)
  })
})

# 
# # create a own flow
# lrnW = makeFilterWrapper(makeImputeWrapper(makeLearner("classif.logreg"), classes = list(numeric = imputeMedian(), integer = imputeMedian())), fw.perc = 0.5, fw.method = "variance")
# lrnW = removeAllHyperPars(lrnW)
# flow.id = uploadOMLFlow(lrnW)
# flow = getOMLFlow(3876)
# lrn = convertOMLFlowToMlrLearner(flow)
# converted.flow = convertMlrLearnerToOMLFlow(lrn)
