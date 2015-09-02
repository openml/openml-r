# creates an API call url of the form server/get.arg?session_hash=myhash
getAPIURL = function(fname, get.arg = NULL) {
  conf = getOMLConfig()
  url = if (is.null(get.arg))
    sprintf("%s/%s?session_hash=%s", conf$server, fname, conf$session.hash)
  else
    sprintf("%s/%s/%s?session_hash=%s", conf$server, fname, get.arg, conf$session.hash)
  return(url)
}

# download xml from a url / api call to file
# if the file is NULL, we just retrieve the content, which is returned in any case
# FIXME: we should try to hit the cache here to avoid the repetitive if-else statements
# FIXME: do we need post mode
downloadXML = function(url, file, verbosity = NULL, post = FALSE) {
  showInfo(verbosity, "Downloading '%s' to '%s'", url, ifelse(is.null(file), "<mem>", file))
  # ddd = list(...)
  if (post) {
    content = do.call(postForm, c(list(uri = url), ddd))
  } else {
    # if (length(ddd) > 0L) {
      # ddd = collapse(paste(names(ddd), ddd, sep = "="), sep = "&")
      # url = paste(url, ddd, sep = "&")
    # }
    content = do.call(getURL, list(url = url))
  }

  if (!is.null(file)) {
    con = file(file, open = "w")
    on.exit(close(con))
    writeLines(content, con = con)
  }
  return(content)
}

# download an arff to disk
# FIXME: we should try to hit the cache here to avoid the repetitive if-else statements
downloadARFF = function(url, file, verbosity = NULL) {
  showInfo(verbosity, "Downloading '%s' to '%s'", url, file)
  #FIXME: get real verbosity level here >= info
  download.file(url, file, mode = "w", quiet = TRUE)
  arff.reader(file)
}
