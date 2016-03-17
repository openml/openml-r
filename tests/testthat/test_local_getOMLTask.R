context("getOMLTask/local")

test_that("getOMLTask", {
  with_test_cache({
    task = getOMLTask(59)
    expect_is(task, "OMLTask")
    expect_equal(task$task.id, 59)
    expect_equal(task$task.type, "Supervised Classification")
    expect_is(task$input$data.set, "OMLDataSet")
    expect_data_frame(task$input$data.set$data, nrows = 150, ncols = 5, any.missing = FALSE)
    expect_equal(task$input$data.set, getOMLDataSet(61))
    expect_character(task$tags, min.len = 1L, any.missing = FALSE)
    expect_list(task$output, names = "unique")
    expect_list(task$parameters, names = "unique")
 })
})