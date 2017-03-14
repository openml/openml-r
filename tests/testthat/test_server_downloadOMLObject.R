context("downloadOMLObject")

test_that("downloadOMLObject", {
  with_empty_cache({
    grid = expand.grid(id = 1, object = c("data", "task", "flow", "run"), stringsAsFactors = FALSE)
    for (i in 1:nrow(grid)) {
      # check if there is error when file is not in cache but cache.only option is active
      expect_error(downloadOMLObject(id = grid[i, "id"], object = grid[i, "object"], cache.only = TRUE), "not found in cache")
      # check if all files have been downloaded and exist in cache dir now
      down = downloadOMLObject(id = grid[i, "id"], object = grid[i, "object"])
      for (j in 1:length(down$files)) {
        expect_true(identical(file.exists(down$files[[j]]$path), down$files[[j]]$found))
      }
    }
    
    with_empty_cache({
      reset_config({
        setOMLConfig(apikey = collapse(rep("a", 32), ""))
        for (i in 1:nrow(grid)) {
          # check if error is print when xml contains an error node
          expect_error(downloadOMLObject(id = grid[i, "id"], object = grid[i, "object"]), "Authentication failed")
        }
      })
    })
  })
})
