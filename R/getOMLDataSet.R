#' @title Get an OpenML data set.
#'
#' @description Given a data set ID, the corresponding \code{\link{OMLDataSet}}
#' will be downloaded (if not in cache) and returned.
#'
#' Note that data splits and other task-related information are not included in
#' an \code{\link{OMLDataSet}}. Tasks can be downloaded with \code{\link{getOMLTask}}.
#'
#' @param did [\code{integer(1)}]\cr
#'   Data set ID.
#' @template arg_cache_only
#' @template arg_verbosity
#' @return [\code{\link{OMLDataSet}}].
#' @family downloading functions
#' @family data set-related functions
#' @example inst/examples/getOMLDataSet.R
#' @export
getOMLDataSet = function(did, cache.only = FALSE, verbosity = NULL) {
  did = asInt(did, lower = 0)
  assertFlag(cache.only)

  down = downloadOMLObject(did, object = "data", cache.only = cache.only, verbosity = verbosity)
  # print(down$files)
  # print(readLines(down$files$description.xml$path))

  f = down$files

  # parse data set description
  data.desc = parseOMLDataSetDescription(down$doc)

  # warn if dataset not cached and deactivated
  if (!cache.only) {
    if (data.desc$status == "deactivated") {
      stop("Data set has been deactivated.")
    } else if (data.desc$status == "in_preparation") {
      stop("Data set is in preparation. You can download it as soon as it's active.")
    }
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
    status = xmlRValS(doc, "/oml:data_set_description/oml:status"),
    tags = xmlOValsMultNsS(doc, "/oml:data_set_description/oml:tag")
  ))
  do.call(makeOMLDataSetDescription, args)
}
