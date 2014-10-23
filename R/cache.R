##### getters for paths

getCacheSubDir = function(subdir, id = NULL) {
  conf = getOpenMLConfig()
  if (is.null(id))
    file.path(conf$cachedir, subdir)
  else
    file.path(conf$cachedir, subdir, id)
}

getCacheFilePath = function(subdir, id, file) {
  conf = getOpenMLConfig()
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

getDataSetPath = function(id, file) {
  getCacheFilePath("datasets", id, file)
}


##### creating stuff on init
createDir = function(dir, verbosity = NULL) {
  ex = file.exists(dir)
  if (!ex) {
    ok = dir.create(dir)
    if (ok)
      showInfo(verbosity, "Created dir: %s", dir)
    else
      stopf("Error in creating dir: %s", dir)
  }
}

createCacheSubDirs = function(verbosity = NULL) {
  conf = getOpenMLConfig()
  cd = conf$cachedir
  showInfo(verbosity, "Creating chache sub dirs in: %s", cd)
  createDir(getCacheDataSetsDir(), verbosity)
}


# tries to find object path in cache dir. caller can request to create path if not found.
# return:
#  - path to dir,
#  - whether it already existed
#  - whether it was created
# unsuccessful request to create = exception
findInCache = function(subdir, id, create) {
  created = FALSE
  path = getCacheSubDir(subdir, id)
  found = file.exists(path)
  if (!found && create) {
    ok = dir.create(path)
    if (!ok)
      stopf("Error in creating cache dir: %s", path)
    created = TRUE
  }
  list(path = path, found = found, created = created)
}

findInCacheDataSet = function(id, create) {
  findInCache("datasets", id, create)
}

clearOMLCache = function() {
  conf = getOpenMLConfig()
  cd = conf$cachedir
  unlink(cd, recursive = TRUE)
  createDir(cd)
  createCacheSubDirs()
}
