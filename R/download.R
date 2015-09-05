# @title Perform an API call to the OpenML server
#
# @description The function always returns the XML file content provided by the
# server.
#
# @param url [character(1)]
#   URL to API call.
# @param id [integer(1)]
#   Optional integer ID we pass to the API, like runs/list/1.
# @param url.args [list]
#   Named list of key value pairs passed as a GET parameter list, e.g.,
#   key1=value1&key2=value2 to the API call.
# @param file [character(1)]
#   Optional filename to write the XML content to.
# @param verbosity [logical(1)]
#   Be verbose and show info messages?
# @param method [character(1)]
#   HTTP request method. Currently one of GET, POST or DELETE.
# @param ... [any]
#   Another possibility to pass key-value pairs for the HTTP request query.
# @return [character(1)] Unparsed content of the returned XML file.
# FIXME: we should try to hit the cache here to avoid the repetitive if-else statements
doAPICall = function(api.call, id = NULL,
  url.args = list(), post.args = list(), file = NULL,
  verbosity = NULL, method, ...) {
  assertChoice(method, choices = c("GET", "POST", "DELETE"))

  # get config infos
  conf = getOMLConfig()

  # occasionally we need to pass a single API arg, such as the data id, additionally
  id = if (!is.null(id)) paste0("/", id) else ""

  # create HTTP GET args list
  if (method == "GET") {
    url.args = insert(url.args, list(...))
  }
  url.args$api_key = conf$apikey
  url.args = namedArgsListToHTTPQuery(url.args)

  # create url
  url = sprintf("%s/%s%s?%s", conf$server, api.call, id, url.args)
  from.url = ifelse(method == "GET", "Downloading from", 
    ifelse(method == "POST", "Uploading to", "Deleting from"))
  showInfo(verbosity, "%s '%s' to '%s'", from.url, url, ifelse(is.null(file), "<mem>", file))

  if (method == "GET") {
    content = GET(url = url)
    content = rawToChar(content$content)
  } else if (method == "POST") {
    content = POST(url = url, body = post.args)
  } else if (method == "DELETE")
    content = DELETE(url = url)

  if (!is.null(file)) {
    con = file(file, open = "w")
    on.exit(close(con))
    writeLines(content, con = con)
  }
  return(content)
}

namedArgsListToHTTPQuery = function(args) {
  collapse(paste(names(args), args, sep = "="), "&")
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
