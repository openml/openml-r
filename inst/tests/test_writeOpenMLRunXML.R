context("writeOpenMLRunXML")

test_that("writeOpenMLRunXML", {
  lrn <- makeLearner("classif.rpart", par.vals=list(minsplit = 5, maxdepth = 10))
  run.pars <- makeRunParameterList(lrn)
  
  desc <- OpenMLRun("1", "classif.rpart(1.0)", run.pars)
  file <- tempfile()
  writeOpenMLRunXML(desc, file)
  
  doc <- parseXMLResponse(file, "Getting run results", "run")
  task.id <- xmlRValS(doc, "/oml:run/oml:task_id")
  impl.id <- xmlRValS(doc, "/oml:run/oml:implementation_id")
  
  expect_true(task.id == "1" && impl.id == "classif.rpart(1.0)")
  
  extractInfoFromParSet <- function(doc, path) {
    ns <- getNodeSet(doc, path)
    return(unlist(lapply(ns, function(x) xmlValue(x))))
  }
  
  names <- extractInfoFromParSet(doc, "/oml:run/oml:parameter_setting/oml:name")
  values <- extractInfoFromParSet(doc, "/oml:run/oml:parameter_setting/oml:value")
  
  for(i in seq_along(run.pars)) {
    expect_true(run.pars[[i]]@name == names[i] && run.pars[[i]]@value == values[i])
  }
  unlink(file)
}
  