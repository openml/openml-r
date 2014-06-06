context("runTask")

test_that("runTask", {
  for(id in 1:2) {
    task = downloadOpenMLTask(id)
    if(task$task.type == "Supervised Classification") {
      lrn = makeLearner("classif.rpart")
    } else {
      lrn = makeLearner("regr.lm")
    }
    results = runTask(task, lrn)
    expect_is(results, "data.frame")
    nr_classes = length(levels(results$prediction))
    expect_true(ncol(results) == 4 + nr_classes)
    expect_true(nrow(results) == max(results[, 1] + 1) * max(results$row_id + 1))
    expect_is(apply(results[, 5:(4+nr_classes)], 1, function(x) expect_equal(sum(x), 1)), "list")
    expect_true(all(table(results$row_id) == max(results[, 1] + 1)))
  }
})
