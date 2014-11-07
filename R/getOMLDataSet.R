#' @title Get an OpenML data set.
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
#' @template arg_verbosity
#' @return [\code{\link{OMLDataSet}}]
#' @export
getOMLDataSet = function(id, session.hash = getSessionHash(), verbosity = NULL) {
  id = asInt(id)
  showInfo(verbosity, "Getting data set '%i' from OpenML repository.", id)

  f = findInCacheDataSet(id, create = TRUE)

  # get XML description
  if (!f$description.found) {
    data.desc.contents = downloadOMLDataSetDescription(id, verbosity = verbosity, session.hash = session.hash)
  } else {
    showInfo(verbosity, "Data set description found in cache.")
    data.desc.contents = readLines(getCacheFilePath("datasets", id, "description.xml"))
  }
  data.desc.xml = parseXMLResponse(data.desc.contents, "Getting data set description", "data_set_description", as.text = TRUE)
  data.desc = parseOMLDataSetDescription(data.desc.xml)

  # now get data file
  if (!f$dataset.found) {
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

# FIXME: cleanup ... too many functions

downloadOMLDataSetDescription = function(id, verbosity = NULL, session.hash = getSessionHash()) {
  id = asInt(id)
  path = getCacheDataSetPath(id, "description.xml")
  url = getAPIURL("openml.data.description", data.id = id)
  downloadXML(url, path, verbosity, session_hash = session.hash)
}

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

#' @title Construct OMLDataSet.
#'
#' @param desc [\code{\link{OMLDataSetDescription}}]\cr
#'   The data set's description.
#' @param data [\code{data.frame}]\cr
#'   The data set.
#' @param colnames.old [\code{character}]\cr
#'   The original column names of the data set. These might not be valid names in R!
#' @param colnames.new [\code{character}]\cr
#'   New column names that are actually used in R. These are valid and unique and differ from the
#'   original column names only if those are invalid.
#' @export
#' @aliases OMLDataSet
makeOMLDataSet = function(desc, data, colnames.old, colnames.new) {
  assertClass(desc, "OMLDataSetDescription")
  assertDataFrame(data)
  assertCharacter(colnames.old, any.missing = FALSE)
  assertCharacter(colnames.old, any.missing = FALSE)

  makeS3Obj("OMLDataSet",
    desc = desc, data = data, colnames.old = colnames.old, colnames.new = colnames.new)
}

# ***** Methods *****

#' @export
print.OMLDataSet = function(x, ...) {
  print.OMLDataSetDescription(x$desc)
}

#' @title Construct OMLDataSetDescription.
#'
#' @param id [\code{integer(1)}]\cr
#'    ID, autogenerated by the server.
#' @param name [\code{character(1)}]\cr
#'   The  name of the data set.
#' @param version [\code{character(1)}]\cr
#'   Version of the data set, added by the server.
#' @param description [\code{character(1)}]\cr
#'   Description of the data set, given by the uploader.
#' @param format [\code{character(1)}]\cr
#'   Format of the data set. Typically, this is "arff".
#' @param creator [\code{character}]\cr
#'   The person(s), that created this data set. Optional.
#' @param contributor [\code{character}]\cr
#'   People, that contibuted to this version of the data set (e.g., by reformatting). Optional.
#' @param collection.date [\code{character(1)}]\cr
#'   The date the data was originally collected. Given by the uploader. Optional.
#' @param upload.date [\code{\link[base]{POSIXt}}]\cr
#'   The date the data was uploaded. Added by the server.
#' @param language [\code{character(1)}]\cr
#'   Language in which the data is represented. Starts with 1 upper case letter, rest lower case,
#'   e.g. 'English'
#' @param licence [\code{character(1)}]\cr
#'   Licence of the data. \code{NA} means: Public Domain or "don't know/care".
#' @param url [\code{character(1)}]\cr
#'   Valid URL that points to the data file.
#' @param default.target.attribute [\code{character}]\cr
#'   The default target attribute, if it exists. Of course, tasks can be defined that use
#'   another attribute as target.
#' @param row.id.attribute [\code{character(1)}]\cr
#'   The attribute that represents the row-id column, if present in the data set. Else \code{NA}.
#' @param ignore.attribute [\code{character(1)}]\cr
#'   Attributes that should be excluded in modelling, such as identifiers and indexes. Optional.
#' @param version.label [\code{character(1)}]\cr
#'   Version label provided by user, something relevant to the user. Can also be a date,
#'   hash, or some other type of id.
#' @param citation [\code{character(1)}]\cr
#'   Reference(s) that should be cited when building on this data.
#' @param visibility [\code{character(1)}]\cr
#'   Who can see the data set. Typical values: 'Everyone', 'All my friends', 'Only me'.
#'   Can also be any of the user's circles.
#' @param original.data.url [\code{character(1)}]\cr
#'   For derived data, the url to the original data set.
#'   This can be an OpenML data set, e.g. 'http://openml.org/d/1'.
#' @param paper.url [\code{character(1)}]\cr
#'   Link to a paper describing the data set.
#' @param update.comment [\code{character(1)}]\cr
#'   When the data set is updated, add an explanation here.
#' @param md5.checksum [\code{character(1)}]\cr
#'   MD5 checksum to check if the data set is downloaded without corruption.
#' @export
#' @aliases OMLDataSetDescription
makeOMLDataSetDescription = function(id, name, version, description, format,
  creator = NA_character_, contributor = NA_character_, collection.date = NA_character_, upload.date,
  language = NA_character_, licence = NA_character_, url, default.target.attribute = NA_character_,
  row.id.attribute = NA_character_, ignore.attribute = NA_character_, version.label = NA_character_,
  citation = NA_character_, visibility = NA_character_, original.data.url = NA_character_,
  paper.url = NA_character_, update.comment = NA_character_, md5.checksum = NA_character_) {

  assertInt(id)
  assertString(name)
  assertString(version)
  assertString(description)
  assertString(format)
  assertCharacter(creator)
  assertCharacter(contributor)
  assertString(collection.date, na.ok = TRUE)
  assertClass(upload.date, "POSIXt")
  assertString(language, na.ok = TRUE)
  assertString(licence, na.ok = TRUE)
  assertString(url)
  assertString(default.target.attribute, na.ok = TRUE)
  assertString(row.id.attribute, na.ok = TRUE)
  assertCharacter(ignore.attribute)
  assertString(version.label, na.ok = TRUE)
  assertString(citation, na.ok = TRUE)
  assertString(visibility, na.ok = TRUE)
  assertString(original.data.url, na.ok = TRUE)
  assertString(paper.url, na.ok = TRUE)
  assertString(update.comment, na.ok = TRUE)
  assertString(md5.checksum, na.ok = TRUE)

  makeS3Obj("OMLDataSetDescription",
    id = id, name = name, version = version, description = description, format = format,
    creator = creator, contributor = contributor, collection.date = collection.date,
    upload.date = upload.date, language = language, licence = licence, url = url,
    default.target.attribute = default.target.attribute, row.id.attribute = row.id.attribute,
    ignore.attribute = ignore.attribute, version.label = version.label, citation = citation,
    visibility = visibility, original.data.url = original.data.url, paper.url = paper.url,
    update.comment = update.comment, md5.checksum = md5.checksum)
}

# ***** Methods *****

#' @export
print.OMLDataSetDescription = function(x, ...) {
  catfNotNA = function(text, obj) {
    if (!all(is.na(obj)))
      catf(text, collapse(obj, sep = "; "))
  }
  # Wrong indentation to see alignment
  catf('\nData Set "%s" :: (Version = %s, OpenML ID = %i)', x$name, x$version, x$id)
  catfNotNA('\tCollection Date         : %s', x$collection.date)
  catfNotNA('\tCreator(s)              : %s', x$creator)
  catfNotNA('\tDefault Target Attribute: %s', x$default.target.attribute)
}

downloadOMLDataFile = function(id, desc, verbosity = NULL) {
  path = getCacheDataSetPath(id, "dataset.arff")
  data = downloadARFF(desc$url, file = path, verbosity)
  parseOMLDataFile(desc, data)
}

parseOMLDataFile = function(desc, data) {
  if (!is.na(desc$row.id.attribute)) {
    rowid = data[, desc$row.id.attribute]
    data[, desc$row.id.attribute] = NULL
  } else {
    rowid = seq_row(data) - 1L
  }
  setRowNames(data, as.character(rowid))
}
