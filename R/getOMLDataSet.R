#' @title Get an OpenML data set.
#'
#' @description
#' Given a data set ID, the corresponding \code{\link{OMLDataSet}} will be downloaded (if not in cache)
#' and returned.
#'
#' Given an \code{\link{OMLTask}}, it is checked if the related data set is already encapsulated in the
#' task object. If not, the task's data set ID will be used to download it.
#'
#' Note that data splits and other task-related information are not included in an \code{\link{OMLDataSet}}.
#' Tasks can be downloaded with \code{\link{getOMLTask}}.
#'
#' @param x [\code{integer(1)} | \code{\link{OMLTask}}]\cr
#'   Either a data set ID or a task.
#' @param check.status [\code{logical(1)}]\cr
#'   If this is set to \code{TRUE}, only data sets with active status are returned. 
#'   Default is \code{FALSE}.
#' @template arg_hash
#' @template arg_verbosity
#' @return [\code{\link{OMLDataSet}}]
#' @family download
#' @export
getOMLDataSet = function(x, check.status, session.hash, verbosity) {
  UseMethod("getOMLDataSet")
}

#' @rdname getOMLDataSet
#' @export
getOMLDataSet.OMLTask = function(x, check.status = FALSE, session.hash = getSessionHash(), verbosity = NULL) {
  return(x$input$data.set)
}

#' @rdname getOMLDataSet
#' @export
getOMLDataSet.numeric = function(x, check.status = FALSE, session.hash = getSessionHash(), verbosity = NULL) {
  id = asInt(x, lower = 0)

  if (check.status) {
    l = listOMLDataSets(verbosity = 0L)
    status = l[l$did == id, "status"]
    if (status == "deactivated") {
      stop("Data set has been deactivated.")
    } else if (status == "in_preparation") {
      stop("Data set is in preparation. You can download it as soon as it's active.")
    }
  }
  showInfo(verbosity, "Getting data set '%i' from OpenML repository.", id)
  f = findCachedDataset(id)

  # get XML description
  if (!f$description.xml$found) {
    url = getAPIURL("openml.data.description", data.id = id)
    data.desc.contents = downloadXML(url, f$description.xml$path, verbosity, session_hash = session.hash)
  } else {
    showInfo(verbosity, "Data set description found in cache.")
    data.desc.contents = readLines(f$description.xml$path)
  }

  # parse data set description
  data.desc.xml = parseXMLResponse(data.desc.contents, "Getting data set description", "data_set_description", as.text = TRUE)
  args = filterNull(list(
    id = xmlRValI(data.desc.xml, "/oml:data_set_description/oml:id"),
    name = xmlRValS(data.desc.xml, "/oml:data_set_description/oml:name"),
    version = xmlRValS(data.desc.xml, "/oml:data_set_description/oml:version"),
    description = xmlRValS(data.desc.xml, "/oml:data_set_description/oml:description"),
    format = xmlRValS(data.desc.xml, "/oml:data_set_description/oml:format"),
    creator = xmlOValsMultNsS(data.desc.xml, "/oml:data_set_description/oml:creator"),
    contributor = xmlOValsMultNsS(data.desc.xml, "/oml:data_set_description/oml:contributor"),
    collection.date = xmlOValS(data.desc.xml, "/oml:data_set_description/oml:collection_date"),
    upload.date = xmlRValD(data.desc.xml, "/oml:data_set_description/oml:upload_date"),
    language = xmlOValS(data.desc.xml, "/oml:data_set_description/oml:language"),
    licence = xmlOValS(data.desc.xml, "/oml:data_set_description/oml:licence"),
    url = xmlRValS(data.desc.xml, "/oml:data_set_description/oml:url"),
    default.target.attribute = xmlOValS(data.desc.xml, "/oml:data_set_description/oml:default_target_attribute"),
    row.id.attribute = xmlOValS(data.desc.xml, "/oml:data_set_description/oml:row_id_attribute"),
    ignore.attribute = xmlOValsMultNsS(data.desc.xml, "/oml:data_set_description/oml:ignore_attribute"),
    version.label = xmlOValS(data.desc.xml, "/oml:data_set_description/oml:version_label"),
    citation = xmlOValS(data.desc.xml, "/oml:data_set_description/oml:citation"),
    visibility = xmlOValS(data.desc.xml, "/oml:data_set_description/oml:visibility"),
    original.data.url = xmlOValS(data.desc.xml, "/oml:data_set_description/oml:original_data_url"),
    paper.url = xmlOValS(data.desc.xml, "/oml:data_set_description/oml:paper.url"),
    update.comment = xmlOValS(data.desc.xml, "/oml:data_set_description/oml:update.comment"),
    md5.checksum = xmlRValS(data.desc.xml, "/oml:data_set_description/oml:md5_checksum")
  ))
  data.desc = do.call(makeOMLDataSetDescription, args)

  # now get data file
  if (!f$dataset.arff$found) {
    data = downloadARFF(data.desc$url, file = f$dataset.arff$path, verbosity)
  } else {
    showInfo(verbosity, "Data set found in cache.")
    data = read.arff(f$dataset.arff$path)
  }
  data = parseOMLDataFile(data.desc, data)

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

#' @title OMLDataSet.
#'
#' @description An \code{OMLDataSet} consists of an \code{OMLDataSetDescription}, a
#' \code{data.frame} containing the data set and, finally, old and new column names.
#'
#' The \code{OMLDataSetDescription} provides information on the data set, like the ID, name,
#' version, etc. To see a full list of all elements, please see the
#' \href{https://github.com/openml/website/blob/master/openml_OS/views/pages/rest_api/xsd/openml.data.upload.xsd}{XSD}.
#'
#' The slot \code{colnames.old} contains the original names, i.e., the column names that were
#' uploaded to the server, while \code{colnames.new} contains the names that you will see when
#' working with the data in R.
#' Most of the time, old and new column names are identical. Only if the original names are
#' not valid, the new ones will differ.
#' @name OMLDataSet
NULL

makeOMLDataSet = function(desc, data, colnames.old, colnames.new) {
  assertClass(desc, "OMLDataSetDescription")
  assertDataFrame(data)
  assertCharacter(colnames.old, any.missing = FALSE)
  assertCharacter(colnames.new, any.missing = FALSE)

  makeS3Obj("OMLDataSet",
    desc = desc, data = data, colnames.old = colnames.old, colnames.new = colnames.new)
}

#' @export
print.OMLDataSet = function(x, ...) {
  print.OMLDataSetDescription(x$desc)
}

# @title Construct OMLDataSetDescription.
#
# @param id [\code{integer(1)}]\cr
#    ID, autogenerated by the server.
# @param name [\code{character(1)}]\cr
#   The  name of the data set.
# @param version [\code{character(1)}]\cr
#   Version of the data set, added by the server.
# @param description [\code{character(1)}]\cr
#   Description of the data set, given by the uploader.
# @param format [\code{character(1)}]\cr
#   Format of the data set. Typically, this is "arff".
# @param creator [\code{character}]\cr
#   The person(s), that created this data set. Optional.
# @param contributor [\code{character}]\cr
#   People, that contibuted to this version of the data set (e.g., by reformatting). Optional.
# @param collection.date [\code{character(1)}]\cr
#   The date the data was originally collected. Given by the uploader. Optional.
# @param upload.date [\code{\link[base]{POSIXt}}]\cr
#   The date the data was uploaded. Added by the server.
# @param language [\code{character(1)}]\cr
#   Language in which the data is represented. Starts with 1 upper case letter, rest lower case,
#   e.g. 'English'
# @param licence [\code{character(1)}]\cr
#   Licence of the data. \code{NA} means: Public Domain or "don't know/care".
# @param url [\code{character(1)}]\cr
#   Valid URL that points to the data file.
# @param default.target.attribute [\code{character}]\cr
#   The default target attribute, if it exists. Of course, tasks can be defined that use
#   another attribute as target.
# @param row.id.attribute [\code{character(1)}]\cr
#   The attribute that represents the row-id column, if present in the data set. Else \code{NA}.
# @param ignore.attribute [\code{character(1)}]\cr
#   Attributes that should be excluded in modelling, such as identifiers and indexes. Optional.
# @param version.label [\code{character(1)}]\cr
#   Version label provided by user, something relevant to the user. Can also be a date,
#   hash, or some other type of id.
# @param citation [\code{character(1)}]\cr
#   Reference(s) that should be cited when building on this data.
# @param visibility [\code{character(1)}]\cr
#   Who can see the data set. Typical values: 'Everyone', 'All my friends', 'Only me'.
#   Can also be any of the user's circles.
# @param original.data.url [\code{character(1)}]\cr
#   For derived data, the url to the original data set.
#   This can be an OpenML data set, e.g. 'http://openml.org/d/1'.
# @param paper.url [\code{character(1)}]\cr
#   Link to a paper describing the data set.
# @param update.comment [\code{character(1)}]\cr
#   When the data set is updated, add an explanation here.
# @param md5.checksum [\code{character(1)}]\cr
#   MD5 checksum to check if the data set is downloaded without corruption.
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

#' @export
print.OMLDataSetDescription = function(x, ...) {
  catfNotNA = function(text, obj) {
    if (!all(is.na(obj)))
      catf(text, collapse(obj, sep = "; "))
  }
  # Wrong indentation to see alignment
  catf('\nData Set "%s" :: (Version = %s, OpenML ID = %i)', x$name, x$version, x$id)
  catfNotNA('  Collection Date         : %s', x$collection.date)
  catfNotNA('  Creator(s)              : %s', x$creator)
  catfNotNA('  Default Target Attribute: %s', x$default.target.attribute)
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
