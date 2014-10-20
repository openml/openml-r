#' @title Download an OpenML data set including its description.
#'
#' @description Given a name and (optionally) a version of a data set, this function will download
#'   the whole data set description from the OpenML server. Note that this does not include data
#'   splits or other task-related information. Tasks can be downloaded with
#'   \code{\link{downloadOpenMLTask}}.
#'
#' @param name [\code{character(1)}]\cr
#'   The name of the data set.
#' @param version [\code{integer(1)}]\cr
#'   Version that should be downloaded. If the desired version is not available, the latest version
#'   of the data set is downloaded. Default is \code{1}.
#' @param dir [\code{character(1)}]\cr
#'   The directory where intermediate files are stored.
#'   Default is the session's temporary directory.
#' @param clean.up [\code{loigcal(1)}]\cr
#'   Should the downloaded files be removed from disk at the end?
#'   Default is \code{TRUE}.
#' @template arg_showinfo
#' @return [\code{\link{OpenMLDataSetDescription}}]
#' @export
#' @seealso \code{\link{toMlr}}, \code{\link{downloadOpenMLTask}}
downloadOpenMLData = function(name, version = 1, dir = getOpenMLOption("cache.dir"), clean.up = TRUE,
  show.info = getOpenMLOption("show.info")) {

  assertString(name)
  assertCount(version, positive = TRUE)
  assertDirectory(dir, access = "w")
  assertFlag(clean.up)
  assertFlag(show.info)

  dsets = getOpenMLDatasetNames()
  res = dsets[dsets$name == name, ]

  if (nrow(res) == 0L)
    stopf("No data set on OpenML server found for: %s", name)

  if (version %nin% res$version) {
    warningf("Version '%i' not available. Downloading latest version instead. \n", version)
    version = max(res$version)
  }

  id = res[res$version == version, "did"]

  if (show.info) {
    messagef("Downloading data set '%s' from OpenML repository.", name)
    messagef("Intermediate files (XML and ARFF) will be stored in : %s", dir)
  }

  fn.data.set.desc = file.path(dir, sprintf("data_set_desc_%s_v%i.xml", name, version))
  fn.data.set = file.path(dir, sprintf("data_set_%s_v%i.arff", name, version))

  on.exit({
    if (clean.up) {
      unlink(fn.data.set.desc)
      unlink(fn.data.set)
      if (show.info)
        messagef("All intermediate XML and ARFF files are now removed.")
    }
  })

  downloadOpenMLDataSetDescription(id = id, file = fn.data.set.desc, show.info = show.info)
  data.desc = parseOpenMLDataSetDescription(file = fn.data.set.desc)
  downloadOpenMLDataSet(data.desc$url, fn.data.set, show.info)
  data = parseOpenMLDataSet(data.desc, fn.data.set)

  def.target = data.desc$default.target.attribute

  data.desc$original.col.names = colnames(data)

  target.ind = which(colnames(data) %in% def.target)
  colnames(data) = make.names(colnames(data), unique = TRUE)
  def.target = colnames(data)[target.ind]

  data.desc$new.col.names = colnames(data)
  data.desc$data.set = data
  data.desc$default.target.attribute = def.target

  return(data.desc)
}
