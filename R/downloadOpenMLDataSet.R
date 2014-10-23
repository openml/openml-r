#' @title Download an OpenML data set.
#'
#' @description
#' Given the id, this function will download the whole data set, including the ARFF and
#' the description XML from the OpenML server. Note that this does not include data
#' splits or other task-related information. Tasks can be downloaded with
#' \code{\link{downloadOpenMLTask}}.
#'
#' @param id [\code{integer(1)}]\cr
#'   The id of the data set.
#' @template arg_ignore.cache
#' @template arg_showinfo
#' @return [\code{\link{OpenMLDataSetDescription}}]
#' @export
#' @seealso \code{\link{toMlr}}, \code{\link{downloadOpenMLTask}}
downloadOpenMLDataSet = function(id, ignore.cache = FALSE, verbosity = NULL) {
  id = asInt(id)
  showInfo(verbosity, "Downloading data set '%i' from OpenML repository.", id)

  f = findInCacheDataSet(id, create = TRUE)

  # get XML description
  if (!f$found || ignore.cache) {
    data.desc.contents = downloadOpenMLDataSetDescription(id, verbosity)
  } else {
    data.desc.contents = readLines(getCacheFilePath("datasets", id, "description.xml"))
  }
  data.desc.xml = parseXMLResponse(data.desc.contents, "Getting data set description", "data_set_description", as.text = TRUE)
  data.desc = parseOpenMLDataSetDescription(data.desc.xml)

  # now get data file
  if (!f$found || ignore.cache) {
    data = downloadOpenMLDataFile(id, data.desc, verbosity)
  } else {
    data = parseOpenMLDataFile()
  }

  def.target = data.desc$default.target.attribute
  target.ind = which(colnames(data) %in% def.target)
  default.target.attribute = colnames(data)[target.ind]

  colnames.old = colnames(data)
  colnames(data) = make.names(colnames(data), unique = TRUE)
  colnames.new = colnames(data)

  makeS3Obj("OpenMLDataSet",
    data = data,
    colnames.new = colnames.new,
    colnames.old = colnames.old
  )
}

# get the XML description
downloadOpenMLDataSetDescription = function(id, verbosity = NULL) {
  id = asInt(id)
  path = getDataSetPath(id, "description.xml")
  url = getAPIURL("openml.data.description", data.id = id)
  contents = downloadXML(url, path, verbosity)
}

# parse the xml description
parseOpenMLDataSetDescription = function(doc) {
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
    md5.checksum = xmlRValS(doc, "/oml:data_set_description/oml:md5_checksum"),
    data.set = data.frame()
  ))
  do.call(makeOpenMLDataSetDescription, args)
}

# download the ARFF itself
downloadOpenMLDataFile = function(id, desc, verbosity = NULL) {
  path = getDataSetPath(id, "dataset.arff")
  data = downloadARFF(desc$url, file = path, verbosity)
  parseOpenMLDataFile(desc, data)
}

# parse the data set from given file on disk and data set description
parseOpenMLDataFile = function(desc, data) {
  if (!is.na(desc$row.id.attribute)) {
    rowid = data[, desc$row.id.attribute]
    data[, desc$row.id.attribute] = NULL
  } else {
    rowid = seq_row(data) - 1L
  }
  setRowNames(data, as.character(rowid))
}

