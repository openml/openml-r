convertOpenMLDataToMlr = function(data.desc, target = data.desc$default.target.attribute,
  remove.target.NAs = TRUE) {
  
  assertClass(data.desc, "OpenMLDataSetDescription")
  assertChoice(target, data.desc$new.col.names)
  assertFlag(remove.target.NAs)
  
  data = data.desc$data.set
  name = data.desc$name
  
  assertDataFrame(data)
  assertString(name)
  
  classif = is.factor(data[, target])
    
  if (remove.target.NAs)
    data = subset(data, !is.na(data[, target]))
  # FIXME: some data sets have empty factor levels, mlr does not like this
  # fix this for now by removing
  data = droplevels(data)
  
  if (classif) {
    mlr.task = makeClassifTask(id = name, data = data, target = target)
  } else {
    mlr.task = makeRegrTask(id = name, data = data, target = target)
  }
  
  return(mlr.task)
}