context("listOMLSetup")

skip_on_cran()

test_that("listOMLSetup", {
  exp.cols = c("setup.id", "flow.id", "parameter.name", "data.type", "default.value", "value")
  df = .listOMLSetup(limit = 1)
  expect_data_frame(df)
  if (nrow(df) > 0) {
    expect_subset(exp.cols, colnames(df))
    expect_true(length(unique(df$setup.id)) == 1)
  }

  df = .listOMLSetup(limit = 10)
  expect_data_frame(df)
  if (nrow(df) > 0) {
    expect_subset(exp.cols, colnames(df))
    expect_true(length(unique(df$setup.id)) <= 10)
  }

  with_main_server({
    df = .listOMLSetup(limit = 10)
    expect_data_frame(df)
    if (nrow(df) > 0) {
      expect_subset(exp.cols, colnames(df))
      expect_true(length(unique(df$setup.id)) <= 10)
    }

    df = .listOMLSetup(flow.id = 5685)
    expect_data_frame(df)
    if (nrow(df) > 0) {
      expect_subset(exp.cols, colnames(df))
      expect_true(unique(df$flow.id) == 5685)
    }

    df = .listOMLSetup(setup.id = 9008)
    expect_data_frame(df)
    if (nrow(df) > 0) {
      expect_subset(exp.cols, colnames(df))
      expect_true(unique(df$setup.id) == 9008)
      expect_true(length(unique(df$flow.id)) == 1)
    }
  })
})
