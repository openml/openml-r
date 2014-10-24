# download xml from a url / api call to file
# if the file is NULL, we just retrieve the content, which is returned in any case
downloadXML = function(url, file, verbosity = NULL) {
  showInfo(verbosity, "Downloading '%s' to '%s'", url, ifelse(is.null(file), "<mem>", file))
  content = getURL(url)
  if (!is.null(file)) {
    con = file(file, open = "w")
    on.exit(close(con))
    writeLines(content, con = con)
  }
  return(content)
}

# download an arff to disk
downloadARFF = function(url, file, verbosity = NULL) {
  showInfo(verbosity, "Downloading '%s' to '%s'", url, file)
  #FIXME: get real verbosity level here >= info
  download.file(url, file, mode = "w", quiet = TRUE)
  read.arff(file)
}

postFormOML = function(url, verbosity = NULL, ...) {
  showInfo(verbosity, "Downloading '%s' to '%s'", url, "<mem>")
  content = postForm(url, ...)
  return(content)
}
