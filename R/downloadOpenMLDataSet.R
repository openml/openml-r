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

# remove rowid column from data set data.frame and name rows with it instead
# if we dont have rowids we use 0, ..., n-1 for row names
convertOpenMLDataSet = function(dsd, ds) {
  if (!is.na(dsd$row.id.attribute)) {
    rowid = ds[, dsd$row.id.attribute]
    ds[, dsd$row.id.attribute] = NULL
  } else {
    rowid = as.character(0:(nrow(ds)-1))
  }
  setRowNames(ds, as.character(rowid))
}
