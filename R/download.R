# Creates an API call url of the form server/get.arg?session_hash=myhash
#
# @param api.call [character(1)]
#   API method.
# @param api.arg [any]
#   Further unnamed parameter. This is often neccessary for getter API calls, e.g.
#   server/data/{data_id}/?key1=value1&key2=value2. Default is NULL.
# @param ... [any]
#   List of key value pairs.
# @param get.args [list]
#   Named list of key-value pairs.
# @return [character(1)] Full URL respectively API call.
getAPIURL = function(api.call, api.arg = NULL, ..., get.args = NULL) {
  # combine the two ways to pass HTTP GET key value pairs
  args = list(...)
  if (!is.null(get.args)) {
    args = insert(args, get.args)
  }
  # append obligatory session hash
  conf = getOMLConfig()
  args$session.hash = conf$session.hash

  # collapse list to string of form a=z&b=y&c=x
  api.args = namedListToHTTPGETParamList(args)

  # occasionally we need to pass a single API arg additionally
  api.arg = if (!is.null(api.arg)) paste0("/", api.arg) else ""

  url = sprintf("%s/%s%s?%s", conf$server, api.call, api.arg, api.args)
  return(url)
}

# Helper to collapse named list of key-value pairs to the
# HTTP GET list form, i.e., key1=value1&key2=value2.
#
# @param args [list]
#   Named list of arguments.
# @return [character(1)]
namedListToHTTPGETParamList = function(args) {
  collapse(paste(names(args), args, sep = "="), "&")
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
