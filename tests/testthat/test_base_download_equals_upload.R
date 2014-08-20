context("is downloaded = uploaded implementation")

# test, if a downloaded implementation contains all the information of the uploaded implementation
test_that("is downloaded = uploaded implementation", {
  learner = makeLearner("classif.J48")
  impl.ul = createOpenMLImplementationForMLRLearner(learner)
  impl.dl = downloadOpenMLImplementation(id = 362, dir = tempdir(), clean.up = TRUE)

  slots = names(formals(makeOpenMLImplementation))
  server.added.slots = c("id", "uploader", "version", "upload.date", "source.url",
    "binary.url", "parameter", "components")
  
  # FIXME: get commented lines working!
  for(i in setdiff(slots, server.added.slots)) {
  #   expect_true(all(impl.dl[[i]] == impl.ul[[i]]))
  }
  
  compareParVals = function(slot = "parameter", subslot) {
    expect_true(setequal(sapply(impl.dl[[slot]], function(x) x[[subslot]]), 
      sapply(impl.ul[[slot]], function(x) x[[subslot]])))
  }
  
  compareParVals(subslot = "name")
  compareParVals(subslot = "data.type")
  # compareParVals(subslot = "default.value")
  # compareParVals(subslot = "description")
})
