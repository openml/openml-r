context("runTask")

test_that("runTask", {
  for(id in 1:10) {
    print(id)
    task <- downloadOpenMLTask(id)
    if(task@task.type == "Supervised Classification") {
      lrn <- makeLearner("classif.rpart")
    } else {
      lrn <- makeLearner("regr.lm")
    }
    results <- runTask(task, lrn)
    expect_is(results, "data.frame")
    expect_true(ncol(results) == 4 + length(levels(result$prediction)))
    expect_true(nrow(results) == max(result[, 1]) * max(result$row_id))
    expect_is(apply(results[, 5:7], 1, function(x) expect_equal(sum(x), 1)), "NULL")
    expect_true(all(table(results$fold) == nrow(results)/max(result$fold)))
    expect_true(all(table(results$row_id) == 2))
  }
})  