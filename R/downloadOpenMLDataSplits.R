#FIXME: provide helper functions for error xmls

downloadOpenMLDataSplits = function(url, file, show.info = TRUE) {
  assertString(url)
  assertPathForOutput(file)
  downloadBinaryFile(url, file, show.info)
}

parseOpenMLDataSplits = function(ds, file) {
  assertClass(ds, "data.frame")
  assertFile(file, access = "r")
  splits = read.arff(file)
  convertOpenMLDataSplits(ds, splits)
}

convertOpenMLDataSplits = function(ds, splits) {
  # 'repeat' is a BAD col. name in R
  colnames(splits)[colnames(splits) == "repeat"] = "rep"
  # all counters in OpenML (server) are 0-based, R is 1-based
  ri = splits$rowid
  rns = rownames(ds)
  splits$rowid = sapply(ri, function(x) which(x == rns))
  splits$rep = splits$rep + 1
  splits$fold = splits$fold + 1
  return(splits)
}
