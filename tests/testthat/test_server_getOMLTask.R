test_that("getOMLTask", {
  with_empty_cache({
    with_main_server({
      expect_s3_class(getOMLTask(34537), "OMLTask")
    })
 })
})
