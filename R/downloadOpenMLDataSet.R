# download the data set from given url to a file on disk
downloadOpenMLDataSet = function(url, file, show.info) {
  assertString(url)
  assertPathForOutput(file)
  downloadBinaryFile(url, file, show.info)
}

# parse the data set from given file on disk and data set description
parseOpenMLDataSet = function(dsd, file) {
  assertClass(dsd, "OpenMLDataSetDescription")
  assertFile(file, access = "r")
  ds = read.arff(file)
  convertOpenMLDataSet(dsd, ds)
}

# remove rowid coluumn from data and name rows with it instead
convertOpenMLDataSet = function(dsd, ds) {
  if (!is.na(dsd$row.id.attribute)) {
    rowid = ds[, dsd$row.id.attribute]
    ds[, dsd$row.id.attribute] = NULL
  } else {
    rowid = as.character(0:(nrow(ds)-1))
  }
  setRowNames(ds, as.character(rowid))
}

