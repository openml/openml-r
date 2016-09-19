context("runTaskFlow")

test_that("runTaskFlow", {
  with_test_cache({
    checkRun = function(res) {
      expect_is(res, "OMLMlrRun")
      expect_equal(length(res), 3L)
      expect_is(res$run$predictions, "data.frame")
      expect_is(res$bmr, "BenchmarkResult")
      expect_is(res$flow, "OMLFlow")
    }
    getOMLRunEvaluations = function(run, measures, aggr = TRUE) {
      evals = run$output.data$evaluations
      evals = evals[evals$name %in% measures, ]
      if (aggr) return(evals[is.na(evals$fold) & is.na(evals[,"repeat"]), ]) else
        return(evals[!is.na(evals$fold) & !is.na(evals[,"repeat"]), ])
    }
    
    task = getOMLTask(59) #3
    
    # do the same again and use a learner with parameter values
    lrn = makeFilterWrapper(makeLearner("classif.randomForest", mtry = 2, ntree = 5), fw.method = "variance", fw.perc = 0.5)
    run = getOMLRun(539275)
    
    flow = getOMLFlow(run$flow.id)
    res2 = runTaskFlow(task, flow, par.list = getOMLRunParList(run), seed = getOMLSeedParList(run))
    checkRun(res2)
    par.list = convertOMLRunParListToList(getOMLRunParList(run))
    res3 = runTaskFlow(task, flow, par.list = par.list, seed = 1)
    checkRun(res3)
    names(par.list)[1] = "renamed"
    expect_error(runTaskFlow(task, flow, par.list = par.list, seed = 1))
    
    # compare with acc value computed from run
    acc = getOMLRunEvaluations(run, "predictive_accuracy", aggr = FALSE)$value
    acc2 = getBMRPerformances(res2$bmr)[[1]][[1]][["acc"]]
    expect_equal(round(acc, 5), round(acc2, 5))
    
    # check if changing the seed yields different results (for RF it should)
    res3 = runTaskFlow(task, flow, par.list = getOMLRunParList(run), seed = 123456)
    checkRun(res3)
    acc3 = getBMRPerformances(res3$bmr)[[1]][[1]][["acc"]]
    expect_false(isTRUE(all.equal(round(acc, 5), round(acc3, 5))))
    
    # check if passing parameters as list works
    lrn = makeLearner("classif.randomForest", mtry = 2)
    task = getOMLTask(task.id = 59L)
    run.mlr = runTaskMlr(task, lrn)
    run.flow = runTaskFlow(task, convertMlrLearnerToOMLFlow(lrn), par.list = getHyperPars(lrn))
    checkRun(run.flow)
    
    expect_equal(getBMRPerformances(run.mlr$bmr, as.df = TRUE)$acc,
      getBMRPerformances(run.flow$bmr, as.df = TRUE)$acc)
  })
})
