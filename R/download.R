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
    split = unlist(stri_split_fixed(url, "://"))
    if (length(split) == 1L) {
      url = paste0(pr, split)
    } else if (length(split) == 2L) {
      split[1L] = pr
      url = collapse(split, sep = "")
    } else {
      stop("The server URL in your config file looks strange. Try 'http://www.openml.org'.")
    }
  }
  n = nchar(url)
  last.char = stri_sub(url, n, n)
  api = ifelse(last.char == "/", "api", "/api")
  paste0(url, api)
}

# download xml from a url / api call to file
# if the file is NULL, we just retrieve the content, which is returned in any case
# FIXME: we should try to hit the cache here to avoid the repetitive if-else statements
downloadXML = function(url, file, verbosity = NULL, post = TRUE, ...) {
  showInfo(verbosity, "Downloading '%s' to '%s'", url, ifelse(is.null(file), "<mem>", file))
  ddd = list(...)
  
  if (post) {
    content = do.call(postForm, c(list(uri = url), ddd))
  } else {
    if (length(ddd) > 0L) {
      ddd = collapse(paste(names(ddd), ddd, sep = "="), sep = "&")
      url = paste(url, ddd, sep = "&")
    }
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
  read.arff(file)
}
