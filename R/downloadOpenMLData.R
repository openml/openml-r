downloadOpenMLData = function(name, version = 1, dir = tempdir(), clean.up = TRUE,
  show.info = TRUE) {
  
  fn.data.set.desc = fn.data.set = NULL
  
  assertString(name)
  assertDirectory(dir, access = "w")
  assertFlag(clean.up)
  assertFlag(show.info)
  
  on.exit({
    if (clean.up) {
      if (!is.null(fn.data.set.desc)) unlink(fn.data.set.desc)
      if (!is.null(fn.data.set)) unlink(fn.data.set)
      if (show.info)
        messagef("All intermediate XML and ARFF files are now removed.")
    }
  })
  
  # get id for given dataset name
  query = paste0("SELECT did, default_target_attribute, version FROM dataset WHERE name = '",
                 name, "'")
  
  res = runSQLQuery(query)
  if (nrow(res) == 0L)
    stopf("No data set on OpenML server found for: %s", name)
  
  if (version %nin% res$version) {
    warningf("Version '%i' not available. Downloading latest version instead. \n", version)
    version = max(res$version)
  } 
  row = which(res$version == version)
  
  id = res[row, "did"]
  target = res[row, "default_target_attribute"]
  
  if (show.info) {
    messagef("Downloading data set '%s' from OpenML repository.", name)
    messagef("Intermediate files (XML and ARFF) will be stored in : %s", dir)
  }
  
  fn.data.set.desc = file.path(dir, sprintf("data_set_desc_%s_v%i.xml", name, version))
  fn.data.set = file.path(dir, sprintf("data_set_%s_v%i.arff", name, version))
  
  downloadOpenMLDataSetDescription(id = id, file = fn.data.set.desc, show.info = show.info)
  data.desc = parseOpenMLDataSetDescription(file = fn.data.set.desc)
  downloadOpenMLDataSet(data.desc$url, fn.data.set, show.info)
  data = parseOpenMLDataSet(data.desc, fn.data.set)
  
  data.desc$original.col.names = colnames(data)
  
  target.ind = which(colnames(data) %in% target)
  colnames(data) = make.names(colnames(data), unique = TRUE)
  target = colnames(data)[target.ind]
  
  data.desc$new.col.names = colnames(data)
  data.desc$data.set = data
  data.desc$default.target.attribute = target
  
  return(data.desc)
}