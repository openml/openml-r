context("populateOMLCache")

test_that("populateOMLCache", {
  with_test_server({
    with_empty_cache({
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

      # long test:
      #data.ids = listOMLDataSets()$data.id
      #task.ids = listOMLTasks()$task.id
      #flows = listOMLFlows()
      #flow.ids = flows[!grepl("openml.evaluation", flows$name), "flow.id"]
      data.ids = 1:2
      task.ids = 1:2
      flow.ids = 1:2

      # check if stuff is stored
      populateOMLCache(data.ids = data.ids)
      expect_exists_in_cache("datasets", data.ids)
      populateOMLCache(task.ids = task.ids)
      expect_exists_in_cache("tasks", task.ids)
      #populateOMLCache(run.ids = 1:2)
      #expect_exists_in_cache("runs", 1:2)
      populateOMLCache(flow.ids = flow.ids)
      expect_exists_in_cache("flows", flow.ids)
    })
  })
})
