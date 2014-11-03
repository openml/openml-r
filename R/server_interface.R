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