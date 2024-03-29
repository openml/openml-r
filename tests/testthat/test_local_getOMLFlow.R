test_that("getOMLFlow", {
  with_test_cache({
    expect_error(getOMLFlow(430408, cache.only = TRUE), "not found in cache")
    flow = getOMLFlow(2)
    expect_s3_class(flow, "OMLFlow")
    expect_output(print(flow), "Flow")
    expect_equal(flow$flow.id, 2)

    # check flow created with R
    flow.r = getOMLFlow(5270)
    expect_s3_class(flow.r, "OMLFlow")
    expect_output(print(flow.r), "Flow")
    expect_string(flow.r$binary.url)
    expect_equal(flow.r$binary.format, "Rds")
    expect_file(flow.r$binary.path)
    expect_string(flow.r$binary.md5)
    for (i in seq_along(flow.r$parameters))
      expect_output(print(flow.r$parameters[[i]]), "Parameter")

    # flow converter
    expect_error(convertOMLFlowToMlr(flow), "This flow can not be converted")
    lrn1 = convertOMLFlowToMlr(flow.r)
    expect_s3_class(lrn1, "Learner")
    converted.flow = convertMlrLearnerToOMLFlow(lrn1)
    expect_s3_class(converted.flow, "OMLFlow")

    # check
    #lrn = makeOversampleWrapper(makeFilterWrapper(makeImputeWrapper(makeLearner("classif.logreg"),
    #  classes = list(numeric = imputeMedian(), integer = imputeMedian())), fw.perc = 0.5, fw.method = "variance"))
    #expect_equal(removeAllHyperPars(lrn), lrn1)
 })
})
