# test_that("getCachedOMLDataSetStatus", {
#   with_empty_cache({
#     status = getCachedOMLDataSetStatus(limit = 100)
#     expect_true(identical(dim(status), c(0L, 0L)))
# 
#     data.ids = 1:2
#     populateOMLCache(data.ids = data.ids)
# 
#     status = getCachedOMLDataSetStatus(limit = 100)
#     expect_s3_class(status, "data.frame")
#     expect_equal(nrow(status), length(data.ids))
#     expect_true(all(data.ids %in% status$data.id))
#   })
# })
