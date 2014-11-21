##### getters for paths

getCacheSubDir = function(subdir, id = NULL) {
  conf = getOMLConfig()
  if (is.null(id))
    file.path(conf$cachedir, subdir)
  else
    file.path(conf$cachedir, subdir, id)
}

getCacheFilePath = function(subdir, id, file) {
  conf = getOMLConfig()
  file.path(conf$cachedir, subdir, id, file)
}

# parent path for data sets
getCacheDataSetsDir = function() {
  getCacheSubDir("datasets")
}

# parent path for tasks
getCacheTasksDir = function() {
  getCacheSubDir("tasks")
}

getCacheRunsDir = function() {
  getCacheSubDir("runs")
}

getCacheDataSetPath = function(id, file) {
  getCacheFilePath("datasets", id, file)
}

getCacheTaskPath = function(id, file) {
  getCacheFilePath("tasks", id, file)
}

getCacheRunPath = function(id, file) {
  getCacheFilePath("runs", id, file)
}

##### creating stuff on init
createDir = function(dir, verbosity = NULL) {
  if (!file.exists(dir)) {
    if (dir.create(dir, recursive = TRUE))
      showInfo(verbosity, "Created dir: %s", dir)
    else
      stopf("Error in creating dir: %s", dir)
  }
}

createCacheSubDirs = function(verbosity = NULL) {
  conf = getOMLConfig()
  cd = conf$cachedir
  createDir(getCacheDataSetsDir(), verbosity)
  createDir(getCacheTasksDir(), verbosity)
  createDir(getCacheRunsDir(), verbosity)
}

# tries to find expected elements in cache dir. caller can request to create path if not found.
# return:
#  - path to dir,
#  - whether it was created,
#  - a flag per element wheter it already existed
# unsuccessful request to create = exception
findInCache = function(subdir, id, create, elements) {
  created = FALSE
  path = getCacheSubDir(subdir, id)
  found = file.exists(path)
  if (!found && create) {
    if (!dir.create(path))
      stopf("Error in creating cache dir: %s", path)
    created = TRUE
  }
  el = extractSubList(str_split(elements, "[.]"), element = 1L)
  el = paste0(el, ".found")
  found.list = as.list(elements %in% list.files(path))
  found.list = setNames(found.list, el)
  c(list(path = path, created = created), found.list)
}

findInCacheDataSet = function(id, create) {
  findInCache("datasets", id, create, elements = c("dataset.arff", "description.xml"))
}

findInCacheTask = function(id, create) {
  findInCache("tasks", id, create, elements = c("datasplits.arff", "task.xml"))
}

findInCacheRun = function(id, create) {
  findInCache("runs", id, create, elements = "predictions.arff")
}

clearOMLCache = function() {
  conf = getOMLConfig()
  cd = conf$cachedir
  unlink(cd, recursive = TRUE)
  createDir(cd)
  createCacheSubDirs()
}
