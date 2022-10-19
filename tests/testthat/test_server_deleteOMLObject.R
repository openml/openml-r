# test_that("deleteOMLObject", {
#   with_main_server({
#     expect_error(deleteOMLObject(id = 1, object = "run"), "Run is not owned by you")
#   })
#   expect_error(deleteOMLObject(id = 123456789, object = "run"),
#     "Run does not exist")
# 
#   # local sanity check (account needs read-write permissions)
#   with_test_cache({
#     run = getOMLRun(219)
#     flow = getOMLFlow(run$flow.id)
#   })
#   flow.id = uploadOMLFlow(flow)
#   run$flow.id = flow.id
#   run.id = uploadOMLRun(run)
#   del = deleteOMLObject(id = run.id, object = "run")
#   expect_s3_class(del, "response")
#   expect_equal(httr::status_code(del), 200)
#   expect_error(deleteOMLObject(id = run.id, object = "run"),
#     "Run does not exist")
# })
