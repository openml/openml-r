#FIXME: provide helper functions for error xmls

# download data splits file from URL to local file
downloadOpenMLDataSplits = function(task, ignore.cache = FALSE, show.info = getOpenMLOption("show.info")) {
  id = task$id
  url = task$estimation.procedure$data.splits.url
  fn = file.path("splits", id, sprintf("%i.arff", id))
  data = downloadARFF(url, fn, ignore.cache = ignore.cache, show.info = show.info)
  parseOpenMLDataSplits(task, data)
}

parseOpenMLDataSplits = function(task, data) {
  # slightly converts the splits data frame
  # rename the "repeat" column to "rep" + and make all indices 1-based, they are 0-based on the server
  colnames(data)[colnames(data) == "repeat"] = "rep"
  ri = data$rowid
  rns = rownames(task$data.desc$data.set)
  # FIXME: use match()!
  data$rowid = sapply(ri, function(x) which(x == rns))
  data$rep = data$rep + 1
  data$fold = data$fold + 1
  return(data)
}
