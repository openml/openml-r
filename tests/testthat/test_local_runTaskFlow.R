context("runTaskFlow")

test_that("runTaskFlow", {
  with_empty_cache({
    with_write_access({
      checkRun = function(res) {
        expect_is(res, "OMLMlrRun")
        expect_equal(length(res), 3L)
        expect_is(res$run$predictions, "data.frame")
        expect_is(res$bmr, "BenchmarkResult")
        expect_is(res$flow, "OMLFlow")
      }
      
      task = getOMLTask(3736) #3
      #task$input$evaluation.measures = "area_under_roc_curve"
      
      # # use a learner without parameter values
      # lrn = makeLearner("classif.logreg")
      # res = runTaskMlr(task, lrn)
      # 
      # # upload run, download it again and try to reproduce it locally
      # run.id = uploadOMLRun(res)
      # run = getOMLRun(run.id)
      # flow = getOMLFlow(run$flow.id)
      # res2 = runTaskFlow(task, flow, par.list = getOMLRunParList(run), seed = getOMLSeedParList(run))
      # checkRun(res2)
      # expect_equal(res$bmr$results[[1]][[1]]$measures.test$acc,
      #   res2$bmr$results[[1]][[1]]$measures.test$acc)
      
      # do the same again and use a learner with parameter values
      lrn = makeLearner("classif.randomForest", mtry = 2, ntree = 5)
      res = runTaskMlr(task, lrn)
      run.id = uploadOMLRun(res)
      run = getOMLRun(run.id)
      flow = getOMLFlow(run$flow.id)
      res2 = runTaskFlow(task, flow, par.list = getOMLRunParList(run), seed = getOMLSeedParList(run))
      checkRun(res2)
      expect_equal(res$bmr$results[[1]][[1]]$measures.test$acc,
        res2$bmr$results[[1]][[1]]$measures.test$acc)
      
      # check if changing the seed yields different results (for RF it should)
      res3 = runTaskFlow(task, flow, par.list = getOMLRunParList(run), seed = 123456)
      checkRun(res3)
      expect_false(isTRUE(all.equal(res$bmr$results[[1]][[1]]$measures.test$acc, 
        res3$bmr$results[[1]][[1]]$measures.test$acc)))
    })
  })
})
