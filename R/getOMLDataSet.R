#' @title Get an OpenML data set.
#'
#' @description Given a data set ID, the corresponding \code{\link{OMLDataSet}}
#' will be downloaded (if not in cache) and returned.
#'
#' Note that data splits and other task-related information are not included in
#' an \code{\link{OMLDataSet}}. Tasks can be downloaded with \code{\link{getOMLTask}}.
#'
#' @note
#' One of \code{data.id} or \code{data.name} must be passed.
#'
#' @template arg_data.id
#' @param data.name [\code{character(1)}]\cr
#'   Data set name.
#'   This is an alternative to \code{data.id}.
#'   Default is \code{NULL}.
#' @param data.version [\code{integer(1)}]\cr
#'   Version number of the data set with name \code{data.name}.
#'   Default is \code{NULL}.
#'   Ignored if \code{data.id} is passed.
#' @template arg_cache_only
#' @template arg_verbosity
#' @return [\code{\link{OMLDataSet}}].
#' @family downloading functions
#' @family data set-related functions
#' @example inst/examples/getOMLDataSet.R
#' @export
getOMLDataSet = function(data.id = NULL, data.name = NULL, data.version = NULL, cache.only = FALSE, verbosity = NULL) {
  if (!xor(is.null(data.id), is.null(data.name)))
    stopf("You must provide either a data.id or a data.name, but not both.")

  assertFlag(cache.only)

  if (is.null(data.name)) {
    data.id = asInt(data.id, lower = 0)
    return(getOMLDataSetById(data.id = data.id, cache.only = cache.only, verbosity = verbosity))
  }
  getOMLDataSetByName(data.name = data.name, data.version = data.version, cache.only = cache.only, verbosity = verbosity)
}

# Helper function to get data set by data name (and version number).
# (Makes use of getOMLDataSetById)
getOMLDataSetByName = function(data.name = NULL, data.version = NULL, cache.only = FALSE, verbosity = NULL) {
  # else get list of datasets RESTRICTED to the given name
  data.sets = .listOMLDataSets(data.name = data.name, verbosity = verbosity)

  # match by name
  matching.ids = which(data.sets$name == data.name)
  matching.sets = data.sets[matching.ids, , drop = FALSE] # nolint

  # otherwise we have multiple matches and need to consider the version
  data.id = if (is.null(data.version)) {
    # in this case we default to the newest version
    showInfo(verbosity, "Multiple version available, but no data.version passed! Returning the newest version.")
    matching.sets[getMaxIndex(matching.sets$version), "data.id"]
  } else {
    data.version = asInt(data.version, lower = 0)
    matching.sets[matching.sets$version == data.version, "data.id"]
  }

  if (is.null(data.id) || length(data.id) == 0) {
    stopf("Version %i does not exist for dataset '%s'. Available versions: %s",
      data.version, data.name, collapse(matching.sets$version, sep = ", "))
  }

  # get number of matches ...
  n.matches = length(matching.ids)
  # ... and react accordingly
  if (n.matches == 0)
    stopf("No dataset with name '%s' found.", data.name)
  if (n.matches == 1)
    return(getOMLDataSetById(data.id = matching.sets$data.id, cache.only = cache.only, verbosity = verbosity))

  return(getOMLDataSetById(data.id = data.id, cache.only = cache.only, verbosity = verbosity))
}

# Helper function to get data set by data ID.
getOMLDataSetById = function(data.id = NULL, cache.only = FALSE, verbosity = NULL) {
  down = downloadOMLObject(data.id, object = "data", cache.only = cache.only, verbosity = verbosity)
  f = down$files

  # parse data set description
  data.desc = parseOMLDataSetDescription(down$doc)

  # warn if dataset not cached and deactivated
  if (data.desc$status == "deactivated") {
    warningf("Data set has been deactivated.")
  } else if (data.desc$status == "in_preparation") {
    warningf("Data set is in preparation and will be activated soon.")
  }

  # now read data file
  data = arff.reader(f$dataset.arff$path)

  if (!is.na(data.desc$row.id.attribute)) {
    if (is.na(data.desc$ignore.attribute))
      data.desc$ignore.attribute = data.desc$row.id.attribute
    else
      data.desc$ignore.attribute = c(data.desc$ignore.attribute, data.desc$row.id.attribute)
  }
  data = setRowNames(data, as.character(seq_row(data) - 1L))

  def.target = data.desc$default.target.attribute
  target.ind = which(colnames(data) %in% def.target)

  colnames.old = colnames(data)
  colnames(data) = make.names(colnames(data), unique = TRUE)
  colnames.new = colnames(data)

  # overwrite default target attribute to make sure that it's the actual name of the column
  data.desc$default.target.attribute = colnames.new[target.ind]

  makeOMLDataSet(
    desc = data.desc,
    data = data,
    colnames.old = colnames.old,
    colnames.new = colnames.new,
    target.features = data.desc$default.target.attribute
  )
}

parseOMLDataSetDescription = function(doc) {
  default.target.attribute = xmlOValS(doc, "/oml:data_set_description/oml:default_target_attribute")
  default.target.attribute = if (!is.null(default.target.attribute)) unlist(strsplit(default.target.attribute, ",")) else ""

  args = filterNull(list(
    id = xmlRValI(doc, "/oml:data_set_description/oml:id"),
    name = xmlRValS(doc, "/oml:data_set_description/oml:name"),
    version = xmlRValS(doc, "/oml:data_set_description/oml:version"),
    description = xmlRValS(doc, "/oml:data_set_description/oml:description"),
    format = xmlRValS(doc, "/oml:data_set_description/oml:format"),
    creator = xmlOValsMultNsS(doc, "/oml:data_set_description/oml:creator"),
    contributor = xmlOValsMultNsS(doc, "/oml:data_set_description/oml:contributor"),
    collection.date = xmlOValS(doc, "/oml:data_set_description/oml:collection_date"),
    upload.date = xmlRValD(doc, "/oml:data_set_description/oml:upload_date"),
    language = xmlOValS(doc, "/oml:data_set_description/oml:language"),
    licence = xmlOValS(doc, "/oml:data_set_description/oml:licence"),
    url = xmlRValS(doc, "/oml:data_set_description/oml:url"),
    default.target.attribute = default.target.attribute,
    row.id.attribute = xmlOValS(doc, "/oml:data_set_description/oml:row_id_attribute"),
    ignore.attribute = xmlOValsMultNsS(doc, "/oml:data_set_description/oml:ignore_attribute"),
    version.label = xmlOValS(doc, "/oml:data_set_description/oml:version_label"),
    citation = xmlOValS(doc, "/oml:data_set_description/oml:citation"),
    visibility = xmlOValS(doc, "/oml:data_set_description/oml:visibility"),
    original.data.url = xmlOValS(doc, "/oml:data_set_description/oml:original_data_url"),
    paper.url = xmlOValS(doc, "/oml:data_set_description/oml:paper.url"),
    update.comment = xmlOValS(doc, "/oml:data_set_description/oml:update.comment"),
    md5.checksum = xmlRValS(doc, "/oml:data_set_description/oml:md5_checksum"),
    status = xmlRValS(doc, "/oml:data_set_description/oml:status"),
    tags = xmlOValsMultNsS(doc, "/oml:data_set_description/oml:tag")
  ))
  do.call(makeOMLDataSetDescription, args)
}
