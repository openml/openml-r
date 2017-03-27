context("tagOMLObject")

test_that("tagOMLObject", {
  obj = c("data", "task", "flow", "run")
  test.tags = c(collapse(sample(letters, 8), sep = ""), collapse(sample(letters, 8), sep = ""))
  #c("test_base_tagOMLObject1", "test_base_tagOMLObject2")
  get.fun = setNames(c("getOMLDataSet", "getOMLTask", "getOMLFlow", "getOMLRun"), obj)

  for (i in obj) {
    id = 1 #ifelse(i == "flow", 100, ifelse(i == "run", 528271, 1))
    # if tag already exist remove it and try to tag it again
    addTag = try(tagOMLObject(id = id, object = i, tags = test.tags))
    if (isTRUE(addTag)) {
      untagOMLObject(id = id, object = i, tags = test.tags)
      tagOMLObject(id = id, object = i, tags = test.tags)
    }

    # get data/task/flow/run and its tags
    with_empty_cache({
      down.obj = do.call(get.fun[i], list(id))
      tags = if (i == "data") down.obj$desc$tags else down.obj$tags
      expect_true(isSubset(test.tags, tags))
      expect_error(tagOMLObject(id = id, object = i, tags = test.tags),
        "Entity already tagged by this tag")

      # remove tags
      untagOMLObject(id = id, object = i, tags = test.tags)
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
