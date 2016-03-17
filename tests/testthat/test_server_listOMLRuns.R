context(".listOMLRuns/server")

test_that(".listOMLRuns", {
  exp.names = c("run.id", "task.id", "setup.id", "flow.id", "uploader", "error.message")

  rs = .listOMLRuns(task.id = 2L)
  expect_data_frame(rs, ncols = 6L, col.names = "unique")
  expect_true(all(rs$task.id == 2L))
  expect_set_equal(names(rs), exp.names)

  # subset only runs without error
  rs = rs[is.na(rs$error.message), ]
  run.id = unique(rs$run.id)
  flow.id = unique(rs$flow.id)
  uploader.id = unique(rs$uploader)

  runs = .listOMLRuns(run.id = run.id[1:100])
  expect_equal(sort(run.id[1:100]), sort(runs$run.id))
  expect_set_equal(names(runs), exp.names)

  for (i in c("run.id", "flow.id", "uploader.id")) {
    id = get(i)[length(get(i))]
    if (i == "uploader.id")
      i = "uploader"
    rs = do.call(".listOMLRuns", setNames(list(id), i))
    expect_true(all(rs[, i] == id))
    expect_set_equal(names(rs), exp.names)
  }
})
