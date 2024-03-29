# test_that("uploadOMLFlow", {
#   with_test_cache({
#     # get a flow
#     flow = getOMLFlow(5270)
#     expect_s3_class(flow, "OMLFlow")
#     expect_s3_class(flow$flow.id, "integer")
#     # FIXME: sourcefiles and/or binaryfiles should be automatically set
#     # (downloaded from flow$source.url) if they are available
# 
#     # create a own flow
#     lrn = makeLearner("classif.rpart")
#     lrnW = makeFilterWrapper(makeImputeWrapper(lrn, classes = list(numeric = imputeMedian(), integer = imputeMedian())), fw.perc = 0.5, fw.method = "variance")
# 
#     # reupload flow
#     flow$binary.md5 = flow$source.md5 = NA
#     flow$external.version = paste0("R_0-v2.", collapse(sample(letters, 8), sep = ""))
#     expect_message(flow.id <- uploadOMLFlow(flow), "Flow successfully uploaded.") # nolint
#     expect_message(deleteOMLObject(flow.id, object = "flow"), "succesfully deleted.")
# 
#     # remove binary path and try to upload
#     flow$external.version = paste0("R_0-v2.", collapse(sample(letters, 8), sep = ""))
#     binary.path = flow$binary.path
#     flow$binary.path = flow$binary.md5 = NA
#     expect_error(uploadOMLFlow(flow), "You must provide an existing binaryfile.")
# 
#     # add learner object
#     flow$object = readRDS(binary.path)
#     expect_message(flow.id <- uploadOMLFlow(flow), "Flow successfully uploaded.") # nolint
#     expect_message(deleteOMLObject(flow.id, object = "flow"), "succesfully deleted.")
#     expect_s3_class(flow.id, "integer")
# 
#     flow.id = uploadOMLFlow(lrn)
#     expect_s3_class(flow.id, "integer")
#     expect_message(uploadOMLFlow(lrn), "Flow already exists")
#     #deleteOMLObject(flow.id, object = "flow")
# 
#     # upload wrapped learner
#     flow.id = uploadOMLFlow(lrnW)
#     expect_s3_class(flow.id, "integer")
#     expect_message(uploadOMLFlow(lrnW), "Flow already exists")
#   })
# })
