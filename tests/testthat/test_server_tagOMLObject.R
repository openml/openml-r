# test_that("tagOMLObject", {
#   # skip test locally to avoid that users set tags here (only travis should do this otherwise they can't be removed)
#   skip_if_not(identical(Sys.getenv("TRAVIS"), "true"), message = "skip local tagging")
# 
#   obj = c("data", "task", "flow")#, "run")
#   test.tags = c(collapse(sample(letters, 9), sep = ""), collapse(sample(letters, 9), sep = ""))
#   get.fun = setNames(c("getOMLDataSet", "getOMLTask", "getOMLFlow", "getOMLRun"), obj)
# 
#   for (i in obj) {
#     id = 1
#     # get data/task/flow/run and its tags
#     with_empty_cache({
#       # tag object and check if tag exists in downloaded object
#       rm = try(untagOMLObject(id = id, object = i, tags = test.tags), silent = TRUE)
#       expect_null(tagOMLObject(id = id, object = i, tags = test.tags))
#       down.obj = do.call(get.fun[i], list(id))
#       tags = if (i == "data") down.obj$desc$tags else down.obj$tags
#       expect_subset(test.tags, tags)
# 
#       # try to tag it again
#       expect_error(tagOMLObject(id = id, object = i, tags = test.tags),
#         "Entity already tagged by this tag")
# 
#       # remove tag
#       expect_null(untagOMLObject(id = id, object = i, tags = test.tags))
# 
#       # try to remove tag again
#       expect_error(untagOMLObject(id = id, object = i, tags = test.tags),
#         "Tag not found")
# 
#       # tagging should not work without APIkey
#       with_reset_config({
#         setOMLConfig(apikey = "")
#         expect_error(tagOMLObject(id = id, object = i, tags = test.tags))
#         expect_error(untagOMLObject(id = id, object = i, tags = test.tags))
#       })
#     })
# 
#     with_empty_cache({
#       # check if tag is not in downloaded object
#       down.obj = do.call(get.fun[i], list(id))
#       tags = if (i == "data") down.obj$desc$tags else down.obj$tags
#       expect_true(!isSubset(test.tags, tags))
#     })
#   }
# })
