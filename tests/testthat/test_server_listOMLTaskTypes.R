context(".listOMLTaskTypes")

test_that(".listOMLDataSets", {
  tts = .listOMLTaskTypes()
  expect_data_frame(tts, min.rows = 2L, ncols = 2L, col.names = "unique")
  expect_set_equal(names(tts), c("id", "name"))
})
