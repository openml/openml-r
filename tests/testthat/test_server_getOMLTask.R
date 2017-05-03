context("getOMLTask")

test_that("getOMLTask", {
  with_empty_cache({
    with_main_server({
      expect_is(getOMLTask(34537), "OMLTask")
    })
 })
})
