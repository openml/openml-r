# test_that("uploadOMLTask returns an task.id if task successfully created", {
#   with_test_server({
#     ds = getOMLDataSet(20)
#     task.id = try(uploadOMLTask("Supervised Classification", data.id = ds$desc$id,
#       target.feature = ds$target.features,
#       estimation.procedure = "4-fold Crossvalidation"))
#     if (is.error(task.id)) {
#       id = as.numeric(gsub(".*\\[|\\].*", "", as.character(task.id)))
#       deleteOMLObject(id, object = "task")
#       task.id = uploadOMLTask("Supervised Classification", data.id = ds$desc$id,
#         target.feature = ds$target.features,
#         estimation.procedure = "4-fold Crossvalidation")
#     }
#     expect_s3_class(task.id, "integer")
#     expect_s3_class(getOMLTask(task.id), "OMLTask")
#     # error if it is uploaded again
#     expect_error(uploadOMLTask("Supervised Classification", data.id = ds$desc$id,
#       target.feature = ds$target.features,
#       estimation.procedure = "4-fold Crossvalidation"))
#     deleteOMLObject(task.id, object = "task")
#   })
# })
