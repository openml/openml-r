context("listOMLTaskTypes")

skip_on_cran()

test_that("listTaskTypes", {
  tts = .listOMLTaskTypes()
  expect_data_frame(tts, min.rows = 2L, ncols = 2L, col.names = "unique")
  expect_set_equal(names(tts), c("id", "name"))
  expect_integer(tts$id, unique = TRUE)
  expect_character(tts$name, unique = TRUE)
})
