context("deleteOMLObject")

test_that("deleteOMLObject", {
  expect_error(deleteOMLObject(id = 1, object = "run"), "Run is not owned by you")
  expect_error(deleteOMLObject(id = 123456789, object = "run"), "Run does not exists")

  # local sanity check (account needs read-write permissions)
  with_write_access({
    run = getOMLRun(1)
    run.id = uploadOMLRun(run)
    del = deleteOMLObject(id = run.id, object = "run")
    expect_is(del, "response")
    expect_equal(httr::status_code(del), 200)
    expect_error(deleteOMLObject(id = run.id, object = "run"), "Run does not exists")
  })
})
