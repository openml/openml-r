OPENML_URL_API = "http://openml.org/api"
OPENML_URL_API_SECURE = "https://openml.org/api"
OPENML_URL_API_QUERY = "http://www.openml.org/api_query"
OPENML_URL_API_QUERY_BETA = "http://openml.liacs.nl/api_query"

# Generate a URL to download files from the server API.
#
# @param fname [\code{character(1)}]\cr
#   Name of API function to call on server.
# @param secure [\code{logical(1)}]\cr
#   Use a secure connection (https).
# @param ... [any]\cr
#   Arguments for API call.
# @return [\code{character(1)}].
getServerFunctionURL = function(fname, secure = FALSE, ...) {
  url = sprintf("%s/?f=%s",
    if(secure) OPENML_URL_API_SECURE else OPENML_URL_API,
    fname)
  args = list(...)
  if (length(args) > 0L) {
    args = collapse(paste(names(args), args, sep="="), sep="&")
    url = paste(url, args, sep="&")
  }
  return(url)
}
