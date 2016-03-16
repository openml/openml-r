context(".listOMLRuns")

test_that(".listOMLRuns", {
  exp.names = c("run.id", "task.id", "setup.id", "flow.id", "uploader", "error.message")

  rl = .listOMLRuns(task.id = 2L)
  expect_is(rl, "data.frame")
  expect_true(all(rl$task.id == 2L))
  expect_true(setequal(names(rl), exp.names))

  # subset only runs without error
  rl = rl[is.na(rl$error.message),]
  run.id = unique(rl$run.id)
  flow.id = unique(rl$flow.id)
  uploader.id = unique(rl$uploader)
  # get minimum length of ids
  min.len = min(c(length(uploader.id), length(flow.id), length(run.id)))

  runs = .listOMLRuns(run.id = run.id[1:100])
  expect_equal(sort(run.id[1:100]), sort(runs$run.id))
  expect_equal(colnames(runs), exp.names)

  for (i in c("run.id", "flow.id", "uploader.id")) {
    id = get(i)[length(get(i))]
    if (i == "uploader.id") i = "uploader"
    rl = do.call(".listOMLRuns", setNames(list(id), i))
    expect_true(all(rl[,i] == id))
    expect_true(setequal(names(rl), exp.names))
  }

})
