context("deleteOMLObject")

test_that("deleteOMLObject", {
  run = getOMLRun(1)
  run.id = uploadOMLRun(run)
  
  # local sanity check (account needs read-write permissions)
  if (!identical(Sys.getenv("TRAVIS"), "true")) {
    del = deleteOMLObject(id = run.id, object = "run")
    expect_equal(httr::status_code(del), 200)
    expect_error(deleteOMLObject(id = run.id, object = "run"), "Run does not exists")
  }
  
  expect_error(deleteOMLObject(id = 1, object = "run"), "Run is not owned by you")
  expect_error(deleteOMLObject(id = 123456789, object = "run"), "Run does not exists")
})
