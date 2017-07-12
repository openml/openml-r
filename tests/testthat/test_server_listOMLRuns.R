context("listOMLRuns")

test_that("listOMLRuns", {
  exp.names = c("run.id", "task.id", "setup.id", "flow.id", "uploader", "error.message", "tags")

  with_main_server({
    rs = .listOMLRuns(task.id = 2L, limit = 1000)
    expect_data_frame(rs, ncols = 7L, min.rows = 50, col.names = "unique")
    expect_true(all(rs$task.id == 2L))
    expect_set_equal(names(rs), exp.names)

    # filter runs with tag: study_1
    #rs2 = .listOMLRuns(task.id = 2L, tag = "study_1")
    #rs3 = .listOMLRuns(task.id = 3L, tag = "study_1")
    #rs4 = .listOMLRunEvaluations(task.id = 2:3, tag = "study_1")
    #expect_equal(nrow(rs4), nrow(rs2) + nrow(rs3))

    # check that all returns runs have the desired tag (github issue 270)
    test.tag = "myspecialtag"
    rs = .listOMLRuns(tag = test.tag)
    expect_data_frame(rs, col.names = "unique")
    expect_true(all(grepl(test.tag, rs$tags)))
  })
})
