context("tagOMLObject")

test_that("tagOMLObject", {
  obj = c("data", "task", "flow", "run")
  test.tags = c("test_base_tagOMLObject1", "test_base_tagOMLObject2")
  get.fun = setNames(c("getOMLDataSet", "getOMLTask", "getOMLFlow", "getOMLRun"), obj)
  
  for (i in obj) {
    id = ifelse(i == "flow", 100, 1)
    # if tag already exist remove it and try to tag it again
    addTag = tryCatch(tagOMLObject(id = id, object = i, tags = test.tags),
      error = function(e) ifelse(grepl("Entity already tagged", e), TRUE, stop(e)))
    if (isTRUE(addTag)) {
      removeTag = untagOMLObject(id = id, object = i, tags = test.tags)
      expect_true(all(sapply(removeTag, function(x) httr::status_code(x) == 200)))
      addTag = tagOMLObject(id = id, object = i, tags = test.tags)
    }
    expect_true(all(sapply(addTag, function(x) httr::status_code(x) == 200)))
    
    # get data/task/flow/run and its tags
    with_empty_cache({
      down.obj = do.call(get.fun[i], list(id))
      tags = if (i == "data") down.obj$desc$tags else down.obj$tags
      expect_true(isSubset(test.tags, tags))
      expect_error(tagOMLObject(id = id, object = i, tags = test.tags),
        "Entity already tagged by this tag")
      
      # remove tags
      removeTag = untagOMLObject(id = id, object = i, tags = test.tags)
      expect_true(all(sapply(removeTag, function(x) httr::status_code(x) == 200)))
      expect_error(untagOMLObject(id = id, object = i, tags = test.tags),
        "Tag not found")
    })
    
    with_empty_cache({
      # get data/task/flow/run and its tags
      down.obj = do.call(get.fun[i], list(id))
      tags = if (i == "data") down.obj$desc$tags else down.obj$tags
      expect_true(!isSubset(test.tags, tags))
    })
  }
})
