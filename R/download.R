getAPIURL = function(fname, secure = NULL, ...) {
  url = sprintf("%s/?f=%s",
    getServerURL(secure),
    fname)
  args = list(...)
  if (length(args) > 0L) {
    args = collapse(paste(names(args), args, sep = "="), sep = "&")
    url = paste(url, args, sep = "&")
  }
  return(url)
}

getServerURL = function(secure = NULL) {
  url = getOMLConfig()$server
  if (!is.null(secure)) {
    pr = ifelse(secure, "https://", "http://")
    split = unlist(str_split(url, pattern = "://"))
    if (length(split) == 1L) {
      url = paste0(pr, split)
    } else if (length(split) == 2L) {
      split[1L] = pr
      url = collapse(split, sep = "")
    } else {
      stop("The server URL in your config file looks strange. Try 'http://www.openml.org'.")
    }
  }
  last.char = str_sub(url, str_length(url), str_length(url))
  api = ifelse(last.char == "/", "api", "/api")
  url = paste0(url, api)
  return(url)
}

# download xml from a url / api call to file
# if the file is NULL, we just retrieve the content, which is returned in any case
downloadXML = function(url, file, verbosity = NULL, ...) {
  showInfo(verbosity, "Downloading '%s' to '%s'", url, ifelse(is.null(file), "<mem>", file))
  ddd = list(...)
  content = if (length(ddd) > 0L)
    do.call(postForm, c(list(uri = url), ddd))
  else
    getURL(url)
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
