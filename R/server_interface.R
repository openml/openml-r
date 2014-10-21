OPEN_ML_SERVER_API_URL = "http://openml.org/api"
OPEN_ML_SERVER_API_URL_S = "https://openml.org/api"

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
    if(secure) OPEN_ML_SERVER_API_URL_S else OPEN_ML_SERVER_API_URL,
    fname)
  args = list(...)
  if (length(args) > 0L) {
    args = collapse(paste(names(args), args, sep="="), sep="&")
    url = paste(url, args, sep="&")
  }
  return(url)
}
