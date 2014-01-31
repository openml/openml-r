downloadOpenMLDataAsMlrTask <- function(name, dir = tempdir(), clean.up = TRUE, show.info = TRUE) {
  checkArg(name, "character", len = 1L, na.ok = FALSE)
  
  fn.data.set.desc <- file.path(dir, "data_set_description.xml")
  fn.data.set <- file.path(dir, "data_set.ARFF")
  
  #Get ID for given Dataset Name
  SQL <- paste0("SELECT did, default_target_attribute FROM dataset WHERE name = '",name,"'")
  sqlRes = openMLSQLQuery(SQL)
  id <- sqlRes[1,"did"]
  id <- as.integer(as.character(id))
  target <- as.character(sqlRes[1,"default_target_attribute"])
  
  if (show.info) {
    messagef("Downloading task %s from OpenML repository.", name)
    messagef("Intermediate files (XML and ARFF) will be stored in : %s", dir)
  }
  
  openML:::downloadOpenMLDataSetDescription(id = id, file = fn.data.set.desc, show.info = show.info)
  task.data.desc <- openML:::parseOpenMLDataSetDescription(file=fn.data.set.desc)
  openML:::downloadOpenMLDataSet(task.data.desc@url, fn.data.set, show.info)
  task.data.desc@data.set <- openML:::parseOpenMLDataSet(task.data.desc, fn.data.set)
  
  if (clean.up) {
    unlink(fn.data.set.desc)
    unlink(fn.data.set)
    if (show.info)
      messagef("All intermediate XML and ARFF files are now removed.")
  }
  
  #FIXME: Dirty from now on:
  classif <- is.factor(task.data.desc@data.set[,target])
  
  data <- task.data.desc@data.set
  #FIXME some data sets have empty factor levels, mlr does not like this
  # fix this for now by removing
  data <- droplevels(data)
  
  # FIXME: hack to convert bad feature names
  feature.ind <- which(colnames(data) %nin% target)
  feature.names <- colnames(data)[feature.ind]
  feature.names <- str_replace_all(feature.names, pattern=c("\\-"), replacement="_")  
  feature.names <- str_replace_all(feature.names, pattern=c("/"), replacement="_")  
  colnames(data)[feature.ind] <- feature.names
  
  if (classif) {
    mlr.task <- makeClassifTask(id = name, data = data, target = target)
  } else {
    mlr.task <- makeRegrTask(id = name, data = data, target = target)
  }
  
  mlr.task

}