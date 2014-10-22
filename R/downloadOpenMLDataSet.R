# FIXME: export
downloadOpenMLDataSet = function(id, desc = NULL, ignore.cache = FALSE, show.info = getOpenMLOption("show.info")) {
  fn = file.path("datasets", id, sprintf("%s.arff", id))
  if (is.null(desc))
    desc = downloadOpenMLDataSetDescription(id)
  data = downloadARFF(desc$url, file = fn, ignore.cache = ignore.cache, show.info = show.info)
  parseOpenMLDataSet(desc, data)
}

# parse the data set from given file on disk and data set description
parseOpenMLDataSet = function(desc, data) {
  if (!is.na(desc$row.id.attribute)) {
    rowid = data[, desc$row.id.attribute]
    data[, desc$row.id.attribute] = NULL
  } else {
    rowid = seq_row(data) - 1L
  }
  setRowNames(data, as.character(rowid))
}
