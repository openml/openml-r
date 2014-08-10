#FIXME: provide helper functions for error xmls

# download data splits file from URL to local file
downloadOpenMLDataSplits = function(url, file, show.info = TRUE) {
  assertString(url)
  assertPathForOutput(file)
  downloadBinaryFile(url, file, show.info)
}

# parses the splits file into a data.frame
parseOpenMLDataSplits = function(ds, file) {
  assertClass(ds, "data.frame")
  assertFile(file, access = "r")
  splits = read.arff(file)
  convertOpenMLDataSplits(ds, splits)
}

# slightly converts the splits data frame
# rename the "repeat" column to "rep" + and make all indices 1-based, they are 0-based on the server
convertOpenMLDataSplits = function(ds, splits) {
  colnames(splits)[colnames(splits) == "repeat"] = "rep"
  ri = splits$rowid
  rns = rownames(ds)
  splits$rowid = sapply(ri, function(x) which(x == rns))
  splits$rep = splits$rep + 1
  splits$fold = splits$fold + 1
  return(splits)
}
