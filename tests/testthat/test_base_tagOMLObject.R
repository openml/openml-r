context("tagOMLObject")

test_that("tagOMLObject", {
  
  obj = c("data", "flow", "run")
  for (i in obj) {
    addTag = tagOMLObject(id = 1, object = i, method = "add", tag = "test_base_tagOMLObject")
    expect_equal(httr::status_code(addTag), 200)
    expect_error(tagOMLObject(id = 1, object = i, method = "add", tag = "test_base_tagOMLObject"), 
      "Entity already tagged by this tag")
    expect_error(tagOMLObject(id = 123456789, object = i, method = "add", tag = "test_base_tagOMLObject"), 
      "Entity not found")
    
    removeTag = tagOMLObject(id = 1, object = i, method = "remove", tag = "test_base_tagOMLObject")
    expect_equal(httr::status_code(removeTag), 200)
    expect_error(tagOMLObject(id = 1, object = i, method = "remove", tag = "test_base_tagOMLObject"),
      "Tag not found")
    expect_error(tagOMLObject(id = 123456789, object = i, method = "remove", tag = "test_base_tagOMLObject"),
      "not found")
  }
})
