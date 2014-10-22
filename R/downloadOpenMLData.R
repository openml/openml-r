#' @title Download an OpenML data set including its description.
#'
#' @description
#' Given a name and (optionally) a version of a data set, this function will download
#' the whole data set description from the OpenML server. Note that this does not include data
#' splits or other task-related information. Tasks can be downloaded with
#' \code{\link{downloadOpenMLTask}}.
#'
#' @param name [\code{character(1)}]\cr
#'   The name of the data set.
#' @param version [\code{integer(1)}]\cr
#'   Version that should be downloaded. If the desired version is not available, the latest version
#'   of the data set is downloaded. Default is \code{1}.
#' @template arg_ignore.cache
#' @template arg_showinfo
#' @return [\code{\link{OpenMLDataSetDescription}}]
#' @export
#' @seealso \code{\link{toMlr}}, \code{\link{downloadOpenMLTask}}
downloadOpenMLData = function(name, version = 1, ignore.cache = FALSE, show.info = getOpenMLOption("show.info")) {
  assertFlag(show.info)

  id = guessDataIdFromName(name, version)
  if (show.info)
    messagef("Downloading data set '%s' (id=%i) from OpenML repository.", name, id)

  data.desc = downloadOpenMLDataSetDescription(id = id, ignore.cache = ignore.cache, show.info = show.info)
  data = downloadOpenMLDataSet(id, data.desc, ignore.cache = ignore.cache, show.info = show.info)

  # FIXME: we're doing something completely different than the function name suggests
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


# FIXME: export this, make downloadOpenMLData use id by default
guessDataIdFromName = function(name, version = 1) {
  assertString(name)
  assertCount(version, positive = TRUE)
  dsets = getOpenMLDatasetNames()
  res = dsets[dsets$name == name, ]

  if (nrow(res) == 0L)
    stopf("No data set on OpenML server found for: %s", name)

  if (version %nin% res$version) {
    warningf("Version '%i' not available. Downloading latest version instead. \n", version)
    version = max(res$version)
  }

  res[res$version == version, "did"]
}
