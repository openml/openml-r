test_that("convertOMLMlrRunToBMR", {
  with_test_cache({
    task = getOMLTask(59)
    run1 = runTaskMlr(task, makeLearner("classif.rpart"))
    run2 = runTaskMlr(task, makeLearner("classif.lda"))

    bmr = convertOMLMlrRunToBMR(run1, run2)
    expect_s3_class(bmr, "BenchmarkResult")
    expect_equal(getBMRLearnerIds(bmr), c("classif.rpart", "classif.lda"))
  })
})
