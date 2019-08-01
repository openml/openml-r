context("clearOMLCache")

test_that("clearOMLCache", {
  with_test_server({
    with_empty_cache({
      empty = list.files(getOMLConfig()$cachedir, all.files = TRUE, recursive = TRUE)
      flow = getOMLFlow(1L)
      not.empty = list.files(getOMLConfig()$cachedir, all.files = TRUE, recursive = TRUE)
      expect_true(length(empty) < length(not.empty))
      clearOMLCache()
      empty2 = list.files(getOMLConfig()$cachedir, all.files = TRUE, recursive = TRUE)
      expect_equal(length(empty), length(empty2))
    })
  })
})
