#' @title Download an OpenML data set.
#'
#' @description
#' Given the id, this function will download the whole data set, including the ARFF and
#' the description XML from the OpenML server. Note that this does not include data
#' splits or other task-related information. Tasks can be downloaded with
#' \code{\link{downloadOMLTask}}.
#'
#' @param id [\code{integer(1)}]\cr
#'   The id of the data set.
#' @template arg_hash
#' @template arg_ignore.cache
#' @template arg_verbosity
#' @return [\code{\link{OMLDataSet}}]
#' @export
downloadOMLDataSet = function(id, session.hash, ignore.cache = FALSE, verbosity = NULL) {
  id = asInt(id)
  showInfo(verbosity, "Getting data set '%i' from OpenML repository.", id)

  f = findInCacheDataSet(id, create = TRUE)

  # get XML description
  if (!f$description.found || ignore.cache) {
    data.desc.contents = downloadOMLDataSetDescription(id, verbosity, session.hash)
  } else {
    showInfo(verbosity, "Data set description found in cache.")
    data.desc.contents = readLines(getCacheFilePath("datasets", id, "description.xml"))
  }
  data.desc.xml = parseXMLResponse(data.desc.contents, "Getting data set description", "data_set_description", as.text = TRUE)
  data.desc = parseOMLDataSetDescription(data.desc.xml)

  # now get data file
  if (!f$dataset.found || ignore.cache) {
    data = downloadOMLDataFile(id, data.desc, verbosity)
  } else {
    showInfo(verbosity, "Data set found in cache.")
    data = read.arff(getCacheDataSetPath(id, "dataset.arff"))
    data = parseOMLDataFile(data.desc, data)
  }

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
    colnames.new = colnames.new
  )
}

# get the XML description
downloadOMLDataSetDescription = function(id, verbosity = NULL, session.hash) {
  id = asInt(id)
  path = getCacheDataSetPath(id, "description.xml")
  url = getAPIURL("openml.data.description", data.id = id)
  contents = postFormOML(url, path, verbosity, session_hash = session.hash)
}

# parse the xml description
parseOMLDataSetDescription = function(doc) {
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
    default.target.attribute = xmlOValS(doc, "/oml:data_set_description/oml:default_target_attribute"),
    row.id.attribute = xmlOValS(doc, "/oml:data_set_description/oml:row_id_attribute"),
    ignore.attribute = xmlOValsMultNsS(doc, "/oml:data_set_description/oml:ignore_attribute"),
    version.label = xmlOValS(doc, "/oml:data_set_description/oml:version_label"),
    citation = xmlOValS(doc, "/oml:data_set_description/oml:citation"),
    visibility = xmlOValS(doc, "/oml:data_set_description/oml:visibility"),
    original.data.url = xmlOValS(doc, "/oml:data_set_description/oml:original_data_url"),
    paper.url = xmlOValS(doc, "/oml:data_set_description/oml:paper.url"),
    update.comment = xmlOValS(doc, "/oml:data_set_description/oml:update.comment"),
    md5.checksum = xmlRValS(doc, "/oml:data_set_description/oml:md5_checksum")
  ))
  do.call(makeOMLDataSetDescription, args)
}

# download the ARFF itself
downloadOMLDataFile = function(id, desc, verbosity = NULL) {
  path = getCacheDataSetPath(id, "dataset.arff")
  data = downloadARFF(desc$url, file = path, verbosity)
  parseOMLDataFile(desc, data)
}

# parse the data set from given file on disk and data set description
parseOMLDataFile = function(desc, data) {
  if (!is.na(desc$row.id.attribute)) {
    rowid = data[, desc$row.id.attribute]
    data[, desc$row.id.attribute] = NULL
  } else {
    rowid = seq_row(data) - 1L
  }
  setRowNames(data, as.character(rowid))
}
