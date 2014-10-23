downloadXML = function(url, file, verbosity = NULL) {
  showInfo(verbosity, "Downloading '%s' to '%s'", url, file)
  content = getURL(url)
  con = file(file, open = "w")
  on.exit(close(con))
  writeLines(content, con = con)
  return(content)
}

downloadARFF = function(url, file, verbosity = NULL) {
  showInfo(verbosity, "Downloading '%s' to '%s'", url, file)
  #FIXME: get real verbosity level here >= info
  download.file(url, file, mode = "w", quiet = TRUE)
  read.arff(file)
}
