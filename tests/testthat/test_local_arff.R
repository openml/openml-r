test_that("check arff files", {
  with_test_cache({
    lrn = makeLearner("classif.rpart")
    task = getOMLTask(59)
    res = runTaskMlr(task, lrn)

    output = tempfile()
    setOMLConfig(arff.reader = "farff")
    arff.writer(res$run$predictions, file = output)
    run.arff = arff.reader(output)

    # skip on APPVEYOR (problems with java)
    if (!identical(Sys.getenv("APPVEYOR"), "True")) {
      output = tempfile()
      setOMLConfig(arff.reader = "RWeka")
      arff.writer(res$run$predictions, file = output)
      run.arff = arff.reader(output)
    }
  })
})
