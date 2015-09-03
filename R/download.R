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

# @title Perform an API call to the OpenML server
#
# @description The function always returns the XML file content provided by the
# server.
#
# @param url [character(1)]
#   URL to API call.
# @param file [character(1)]
#   Optional filename to write the XML content to.
# @param verbosity [logical(1)]
#   Be verbose and show info messages?
# @param post [logical(1)]
#   Use HTTP POST? Default is FALSE.
# @return [character(1)] Unparsed content of the XML file.
# FIXME: we should try to hit the cache here to avoid the repetitive if-else statements
doAPICall = function(url, file, verbosity = NULL, method = c("GET", "POST", "DELETE"), ...) {
  assertChoice(method, choices = c("GET", "POST", "DELETE"))
  showInfo(verbosity, "Downloading '%s' to '%s'", url, ifelse(is.null(file), "<mem>", file))
  api.args = append(list(...), "session.hash" = getOMLConfig()$session.hash)
  content = do.call(method, list(url = url, query = api.args))

  if (!is.null(file)) {
    con = file(file, open = "w")
    on.exit(close(con))
    writeLines(content, con = con)
  }
  return(content)
}

# Helper to do a GET request to API. See doAPICall for the signature.
doAPICallGET = function(url, file, verbosity = NULL) {
  doAPICall(url, file, verbosity, post = FALSE)
}

# Helper to do a POST request to API. See doAPICall for the signature.
doAPICallPOST = function(url, file, verbosity = NULL) {
  doAPICall(url, file, verbosity, post = TRUE)
}

# @title Download an arff file to disk.
#
# @param url [character(1)]
#   URL to arff file.
# @param file [character(1)]
#   Destination file.
# @verbosity [logical(1)]
#   Be verbose and show info messages?
# @return Parsed arff file.
# FIXME: we should try to hit the cache here to avoid the repetitive if-else statements
downloadARFF = function(url, file, verbosity = NULL) {
  showInfo(verbosity, "Downloading '%s' to '%s'", url, file)
  #FIXME: get real verbosity level here >= info
  download.file(url, file, mode = "w", quiet = TRUE)
  arff.reader(file)
}
