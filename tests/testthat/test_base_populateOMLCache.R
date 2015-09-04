context("populateOMLCache")

test_that("populateOMLCache", {
  # initial cleanup
  clearOMLCache()

  # expect errors if invalid stuff or no stuff is passed
  expect_error(populateOMLCache(task.ids = -1))
  expect_error(populateOMLCache())

  # Helper to check if cached files exist
  #
  # @param type [character(1)]
  #   Type of cache element, e.g., datasets.
  # @param ids [integer]
  #   The IDs of the elements which should be available in cache.
  print(getOMLConfig()$cachedir)
  expect_exists_in_cache = function(res, type, ids) {
    finder = switch(type,
      tasks = findCachedTask,
      datasets = findCachedDataset,
      runs = findCachedRun
    )
    for (id in ids) {
      res = finder(id)
      #print(res)
      expect_true(!is.null(res))
      # get found sublist
      lapply(names(res), function(element) {
        expect_true(res[[element]]$found, info = sprintf("For type = %s, element = %s, ID = %i file
          is not found.", type, element, id))
      })
    }
  }

  # check if tasks are stored
  expect_exists_in_cache(populateOMLCache(dids = 1:2), "runs", 1:2)
  expect_exists_in_cache(populateOMLCache(task.ids = 1:2), "tasks", 1:2)
  expect_exists_in_cache(populateOMLCache(run.ids = 1:2), "runs", 1:2)

  # cleanup
  clearOMLCache()
})
