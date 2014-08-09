OPEN_ML_SERVER_API_URL = "http://openml.org/api"

# Generate a URL to download files from the server API.
#
# @param fname [\code{character(1)}]\cr
#   Name of API function to call on server.
# @param ... [any]\cr
#   Arguments for API call.
# @return [\code{character(1)}].
#
# FIXME: use getForm?
getServerFunctionURL = function(fname, ...) {
  url = sprintf("%s/?f=%s", OPEN_ML_SERVER_API_URL, fname)
  args = list(...)
  if (length(args) > 0) {
    args = collapse(paste(names(args), args, sep="="), sep="&")
    url = paste(url, args, sep="&")
  }
  return(url)
}
