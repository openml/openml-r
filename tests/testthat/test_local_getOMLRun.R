test_that("getOMLRun", {
  with_test_cache({
    # check invalid input
    expect_error(getOMLRun(-10, cache.only = TRUE))
    expect_error(getOMLRun(5235, cache.only = TRUE), "not found in cache")

    run = getOMLRun(1L)
    expect_output(print(run), "OpenML Run")
    expect_true(!is.na(run$flow.name))

    # check IO data
    run.IOdata = run$output.data
    expect_s3_class(run.IOdata, "OMLIOData")
    expect_output(print(run.IOdata), "Data Sets")

    # check parameter settings (get first parameter only)
    for (param.setting in run$parameter.setting) {
      param.setting$component = "Component" # just to trigger the codeblock
      expect_s3_class(param.setting, "OMLRunParameter")
      expect_output(print(param.setting), "Component")
    }

    expect_data_frame(run$predictions)
  })
})
