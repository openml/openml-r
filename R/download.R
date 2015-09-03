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
doAPICall = function(api.call, id = NULL, url.args = NULL, file, verbosity = NULL,
                     method = c("GET", "POST", "DELETE"),
                     ...) {
  assertChoice(method, choices = c("GET", "POST", "DELETE"))
  # get config infos
  conf = getOMLConfig()

  # occasionally we need to pass a single API arg, such as the data id, additionally
  id = if (!is.null(id)) paste0("/", id) else ""
  # create url
  url = sprintf("%s/%s%s?%s", conf$server, api.call, id, paste0("api_key=", conf$apikey))
  showInfo(verbosity, "Downloading '%s' to '%s'", url, ifelse(is.null(file), "<mem>", file))

  if (method == "GET") {
    content = GET(url = url) #do.call(method, list(url = url, query = api.args))
  }
  content = rawToChar(content$content)

  if (!is.null(file)) {
    con = file(file, open = "w")
    on.exit(close(con))
    writeLines(content, con = con)
  }
  return(content)
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
