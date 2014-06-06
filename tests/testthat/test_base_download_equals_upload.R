context("is downloaded = uploaded implementation")
#test, if a downloaded implementation contains contains all the information of the uploaded implementation
test_that("is downloaded = uploaded implementation", {
  learner = makeLearner("classif.rpart")
  impl_ul = createOpenMLImplementationForMLRLearner(learner, version = "2.0")
  expect_error(uploadOpenMLImplementation(impl_ul, session.hash = hash))
  impl_dl = downloadOpenMLImplementation("classif.rpart(2.0)")
  
  slots = names(getSlots("OpenMLImplementation"))
  
  for(i in slots) {
    if(i %nin% c("id", "uploader", "version", "external.version", "upload.date", "source.url", 
      "binary.url", "parameter", "components")) {
      expect_true(all(slot(impl_dl, i) == slot(impl_ul, i)))  
    }
  }
  expect_true(all(sort(unlist(lapply(impl_dl$parameter, function(a) a$name))) 
    == sort(unlist(lapply(impl_ul$parameter, function(a) a$name)))))
  expect_true(all(sort(unlist(lapply(impl_dl$parameter, function(a) a$data.type))) 
    == sort(unlist(lapply(impl_ul$parameter, function(a) a$data.type)))))
  dv_dl = sort(unlist(lapply(impl_dl$parameter, function(a) a$default.value)))
  dv_dl = dv_dl[dv_dl != ""]
  dv_ul = sort(unlist(lapply(impl_ul$parameter, function(a) a$default.value)))
  dv_ul = dv_ul[dv_ul != ""]
  expect_true(all(dv_dl == dv_ul))
  expect_true(all(sort(unlist(lapply(impl_dl$parameter, function(a) a$description))) 
    == sort(unlist(lapply(impl_ul$parameter, function(a) a$description)))))
})  
