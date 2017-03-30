context("openml r paper")

test_that("openml r paper", {
  with_empty_cache({
    with_main_server({
      # listing tasks
      tasks = listOMLTasks(task.type = "Supervised Classification",
        number.of.classes = 2, number.of.instances = c(500, 999),
        number.of.features = c(1, 100), number.of.missing.values = 0)
      expect_data_frame(tasks, min.rows = 2)
      expect_subset(c("task.id", "name", "number.of.instances", "number.of.features"), colnames(tasks))
      expect_subset(c("diabetes", "tic-tac-toe"), tasks$name)

      # listing run evaluations
      res = listOMLRunEvaluations(task.id = 37, tag = "openml_r_paper")
      expect_data_frame(res, min.rows = 19)
      expect_gt(max(res$predictive.accuracy), 0.77)
      expect_lt(min(res$predictive.accuracy), 0.74)

      # get data sets
      checkOMLDataSet(getOMLDataSet(data.id = 15))

      # get flows
      flow = getOMLFlow(4782)
      expect_is(flow, "OMLFlow")
      expect_is(convertOMLFlowToMlr(flow), "Learner")

      # get runs
      run = getOMLRun(run.id = 1816245)
      expect_is(run, "OMLRun")

      # create runs with mlr learners
      lrn = makeLearner("classif.randomForest", mtry = 2)
      task = getOMLTask(task.id = 37)
      expect_is(task, "OMLTask")
      run.mlr = runTaskMlr(task, lrn)
      expect_is(run.mlr, "OMLMlrRun")

      # create runs with downloaded flows
      flow = getOMLFlow(4782)
      run.flow = runTaskFlow(task, flow, par.list = list(mtry = 2))
      expect_is(run.flow, "OMLMlrRun")

      ## ----05-case-study, child="05-case-study.Rnw",cache=FALSE----------------
      tasks = listOMLTasks(data.tag = "uci",
        task.type = "Supervised Classification", number.of.classes = 2,
        number.of.missing.values = 0, number.of.instances = c(100, 999),
        estimation.procedure = "10-fold Crossvalidation")

      expect_data_frame(tasks, min.rows = 6)
      expect_subset(c("task.id", "name", "number.of.instances", "number.of.features"), colnames(tasks))
      expect_subset(c("diabetes", "sonar", "haberman", "tic-tac-toe", "heart-statlog", "ionosphere"), tasks$name)

      evals = listOMLRunEvaluations(tag = "study_30")
      tasks = listOMLTasks(tag = "study_30")
      flows = listOMLFlows(tag = "study_30")
      ds = listOMLDataSets(tag = "study_30")

      expect_data_frame(evals, min.rows = 36)
      expect_data_frame(flows, min.rows = 6)
      expect_data_frame(tasks, min.rows = 6)
      expect_data_frame(ds, min.rows = 6)

      expect_subset(tasks$task.id, evals$task.id)
      expect_subset(flows$flow.id, evals$flow.id)
      expect_subset(ds$name, evals$data.name)
    })
  })
})
