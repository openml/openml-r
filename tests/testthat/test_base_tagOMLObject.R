context("tagOMLObject")

test_that("tagOMLObject", {
  obj = c("data", "task", "flow", "run")
  get.fun = setNames(c("getOMLDataSet", "getOMLTask", "getOMLFlow", "getOMLRun"), obj)
  for (i in obj) {
    id = ifelse(i == "flow", 100, 1)
    # if tag already exist remove it and try to tag it again
    addTag = tryCatch(tagOMLObject(id = id, object = i, method = "add", tag = "test_base_tagOMLObject"),
      error = function(e) ifelse(grepl("Entity already tagged", e), TRUE, stop(e)))
    if (isTRUE(addTag)) {
      removeTag = tagOMLObject(id = id, object = i, method = "remove", tag = "test_base_tagOMLObject")
      expect_equal(httr::status_code(removeTag), 200)
      addTag = tagOMLObject(id = id, object = i, method = "add", tag = "test_base_tagOMLObject")
    }
    expect_equal(httr::status_code(addTag), 200)
    # get data/task/flow/run and its tags
    clearOMLCache()
    down.obj = do.call(get.fun[i], list(id))
    tags = if(i == "data") down.obj$desc$tags else down.obj$tags
    expect_true(isSubset("test_base_tagOMLObject", tags))
    expect_error(tagOMLObject(id = id, object = i, method = "add", tag = "test_base_tagOMLObject"), 
      "Entity already tagged by this tag")
    expect_error(tagOMLObject(id = 123456789, object = i, method = "add", tag = "test_base_tagOMLObject"), 
      "Entity not found|Timeout")
    
    
    # remvoe tags
    removeTag = tagOMLObject(id = id, object = i, method = "remove", tag = "test_base_tagOMLObject")
    expect_equal(httr::status_code(removeTag), 200)
    expect_error(tagOMLObject(id = id, object = i, method = "remove", tag = "test_base_tagOMLObject"),
      "Tag not found")
    expect_error(tagOMLObject(id = 123456789, object = i, method = "remove", tag = "test_base_tagOMLObject"),
      "not found|Timeout")
    # get data/task/flow/run and its tags
    clearOMLCache()
    down.obj = do.call(get.fun[i], list(id))
    tags = if(i == "data") down.obj$desc$tags else down.obj$tags
    expect_true(!isSubset("test_base_tagOMLObject", tags))
    
  }
})
