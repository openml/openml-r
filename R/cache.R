##### creating stuff on init

# @title Create a directory recursively.
#
# @param dir [character(1)]
#   Directory name.
# @param verbosity [logical(1)]
#   Should the function be verbose?
createDir = function(dir, verbosity = NULL) {
  if (!file.exists(dir)) {
    if (dir.create(dir, recursive = TRUE))
      showInfo(verbosity, "Created dir: %s", dir)
    else
      stopf("Error creating dir: %s", dir)
  }
}

# @title Create cache subdirectories for task, datasets, ...
#
# @description Stuff is stored as follows: cache_dir/type/id/, e.g.,
# path_to_my_cache_dir/datasets/134/
#
# @param verbosity [logical(1)]
#   Should the function be verbose?
createCacheSubDirs = function(verbosity = NULL) {
  conf = getOMLConfig()
  cd = conf$cachedir
  createDir(file.path(cd, "datasets"), verbosity)
  createDir(file.path(cd, "tasks"), verbosity)
  createDir(file.path(cd, "runs"), verbosity)
  createDir(file.path(cd, "flows"), verbosity)
}

# @title Get cache directory.
#
# @param subdir [character(1)]
#   Subdirectory indicating the type of cache, e.g, datasets.
# @param id [integer(1)]
#   ID of the cached element.
# @param elements [character]
#   List of files which are stored in the cache file.
# @return [list]
getCacheURI = function(subdir, id, elements) {
  path = file.path(getOMLConfig()$cachedir, subdir, id)
  if (!isDirectory(path) && !dir.create(path, recursive = TRUE))
    stopf("Unable to create directory '%s'", path)
  path = file.path(path, elements)
  size = file.info(path)$size
  found = !is.na(size) & size > 0L
  setNames(Map(list, path = path, found = found), elements)
}

findCachedDataset = function(id) {
  getCacheURI("datasets", id,
    elements = c("dataset.arff", "description.xml"))
}

findCachedTask = function(id) {
  getCacheURI("tasks", id,
    elements = c("datasplits.arff", "task.xml"))
}

findCachedRun = function(id) {
  getCacheURI("runs", id,
    elements = c("predictions.arff", "run.xml"))
}

findCachedFlow = function(id, elements = list()) {
  getCacheURI("flows", id, c(elements, list("flow.xml")))
}

# @title Check if stuff is cached.
isCached = function(subdir, id) {
  path = file.path(getOMLConfig()$cachedir, subdir, id)
  size = file.info(path)$size
  return(isDirectory(path) && !is.na(size) && size > 0L)
}

# @title Clear cache directories
clearOMLCache = function() {
  conf = getOMLConfig()
  cd = conf$cachedir
  unlink(cd, recursive = TRUE)
  createDir(cd)
  createCacheSubDirs()
}
