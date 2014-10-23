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
downloadOpenMLDataSet = function(id, ignore.cache = FALSE, show.info = getOpenMLOption("show.info")) {
  assertFlag(show.info)
  id = asInt(id)
  # id = guessDataIdFromName(name, version)
  if (show.info)
    messagef("Downloading data set '%s' (id=%i) from OpenML repository.", name, id)

  data.desc = downloadOpenMLDataSetDescription(id = id, ignore.cache = ignore.cache, show.info = show.info)
  data = downloadOpenMLDataSet(id, data.desc, ignore.cache = ignore.cache, show.info = show.info)

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

# guessDataIdFromName = function(name, version = 1) {
#   assertString(name)
#   assertCount(version, positive = TRUE)
#   dsets = getOpenMLDatasetNames()
#   res = dsets[dsets$name == name, ]

#   if (nrow(res) == 0L)
#     stopf("No data set on OpenML server found for: %s", name)

#   if (version %nin% res$version) {
#     warningf("Version '%i' not available. Downloading latest version instead. \n", version)
#     version = max(res$version)
#   }

#   res[res$version == version, "did"]
# }

# download the ARFF itself
downloadOpenMLDataFile = function(id, desc = NULL, ignore.cache = FALSE, show.info = getOpenMLOption("show.info")) {
  fn = file.path("datasets", id, sprintf("%s.arff", id))
  if (is.null(desc))
    desc = downloadOpenMLDataSetDescription(id)
  data = downloadARFF(desc$url, file = fn, ignore.cache = ignore.cache, show.info = show.info)
  parseOpenMLDataSet(desc, data)
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

# get the XML description
downloadOpenMLDataSetDescription = function(id, ignore.cache = FALSE, show.info = getOpenMLOption("show.info")) {
  id = asInt(id)
  fn = file.path("descriptions", id, sprintf("%i.xml", id))
  url = getAPIURL("openml.data.description", data.id = id)
  contents = downloadXML(url, file = fn, ignore.cache = ignore.cache, show.info = show.info)
  doc = parseXMLResponse(contents, "Getting data set description", "data_set_description", as.text = TRUE)
  parseOpenMLDataSetDescription(doc)
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

