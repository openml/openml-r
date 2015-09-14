context("populateOMLCache")

test_that("populateOMLCache", {
  # initial cleanup
  clearOMLCache()

  # Helper to check if cached files exist
  #
  # @param type [character(1)]
  #   Type of cache element, e.g., datasets.
  # @param ids [integer]
  #   The IDs of the elements which should be available in cache.
  expect_exists_in_cache = function(type, ids) {
    finder = switch(type,
      tasks = findCachedTask,
      datasets = findCachedData,
      runs = findCachedRun,
      flows = findCachedFlow
    )
    for (id in ids) {
      res = finder(id)
      expect_true(!is.null(res))
      # get found sublist
      lapply(names(res), function(element) {
        expect_true(res[[element]]$found, info = sprintf("For type = %s, element = %s, ID = %i file
          is not found.", type, element, id))
      })
    }
  }

  # check if stuff is stored
  populateOMLCache(dids = 1:2)
  expect_exists_in_cache("datasets", 1:2)
  populateOMLCache(task.ids = 1:2)
  expect_exists_in_cache("tasks", 1:2)
  populateOMLCache(run.ids = 1:2)
  expect_exists_in_cache("runs", 1:2)
  populateOMLCache(flow.ids = 2:3)
  expect_exists_in_cache("flows", 2:3)

  # cleanup
  clearOMLCache()
})
