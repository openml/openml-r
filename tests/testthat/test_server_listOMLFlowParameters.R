context("listOMLFlowParameters")

skip_on_cran()

test_that("listOMLFlowParameters", {
  exp.cols = c("setup.id", "flow.id", "parameter.name", "data.type", "default.value", "value")
  df = .listOMLFlowParameters(limit = 1)
  expect_data_frame(df)
  expect_subset(exp.cols, colnames(df))
  expect_true(length(unique(df$setup.id)) == 1)
  
  df = .listOMLFlowParameters(limit = 10)
  expect_data_frame(df)
  expect_subset(exp.cols, colnames(df))
  expect_true(length(unique(df$setup.id)) <= 10)
  
  with_main_server({
    df = .listOMLFlowParameters(flow.id = 5685) # works 
    expect_data_frame(df)
    expect_subset(exp.cols, colnames(df))
    expect_true(unique(df$flow.id) == 5685)
    
    df = .listOMLFlowParameters(setup.id = 9008)
    expect_data_frame(df)
    expect_subset(exp.cols, colnames(df))
    expect_true(unique(df$setup.id) == 9008)
    expect_true(length(unique(df$flow.id)) == 1)
  })
})
