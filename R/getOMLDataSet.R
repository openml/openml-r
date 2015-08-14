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
    data.desc.contents = downloadXML(url, f$description.xml$path, verbosity, 
      post = FALSE, session_hash = session.hash)
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
