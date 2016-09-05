#' @title Perform an API call to the OpenML server.
#'
#' @description
#' The function always returns the XML file content provided by the server.
#'
#' @param api.call [\code{character(1)}]\cr
#'   API endpoints listed in \href{https://github.com/openml/OpenML/wiki/API-v1}{APIv1}.
#' @param id [\code{integer(1)}]\cr
#'   Optional ID we pass to the API, like runs/list/1.
#' @param url.args [\code{list}]\cr
#'   Named list of key-value pairs passed as HTTP GET parameters, e.g.,
#'   key1=value1&key2=value2 to the API call.
#' @param post.args [\code{list}]\cr
#'   Optional. A list passed to the \code{body}-arg for \code{\link[httr]{POST}} requests.
#' @param file [\code{character(1)}]\cr
#'   Optional filename to write the XML content to.
#' @template arg_verbosity
#' @param method [\code{character(1)}]\cr
#'   HTTP request method. Currently one of GET, POST or DELETE.
#' @param ...
#'   Another possibility to pass key-value pairs for the HTTP request query.
#'   Arguments passed via ... have a higher priority.
#' @return [\code{character(1)}]\cr Unparsed content of the returned XML file.
#' @export
#' @keywords internal
doAPICall = function(api.call, id = NULL,
  url.args = list(), post.args = list(), file = NULL,
  verbosity = NULL, method, ...) {
  assertChoice(method, choices = c("GET", "POST", "DELETE"))

  # get config infos
  conf = getOMLConfig()

  # build request URL and query
  url = buildRequestURL(conf$server, api.call, id, url.args, ...)
  query = list(api_key = conf$apikey)

  if (nchar(url) > 4068)
    stopf("'%s' has %i characters, the maximum allowed url length is 4068.", url, nchar(url))

  # some nice output to the user
  if (method == "GET") {
    showInfo(verbosity, "%s '%s' to '%s'.", "Downloading from", url,
      ifelse(is.null(file), "<mem>", file))
  } else {
    from.url = ifelse(method == "POST", "Uploading to", "Deleting from")
    showInfo(verbosity, "%s '%s'.", from.url, url)
  }

  # finally to the call
  content = doHTTRCall(method,
    url = url,
    query = list(api_key = conf$apikey),
    body = if (length(post.args) > 0) post.args else NULL)

  # write response to file
  if (!is.null(file)) {
    con = file(file, open = "w")
    on.exit(close(con))
    writeLines(content, con = con)
  }

  return(content)
}

# Helper function to generate HTTP request URL to access functionality of the
# OpenML backend.
buildRequestURL = function(server, api.call, id, url.args, ...) {
  # occasionally we need to pass a single API arg, such as the data id, additionally
  id = if (!is.null(id)) paste0("/", id) else ""

  #url.args$api_key = conf$apikey
  url.args = collapseNamedList(url.args)

  # create url of form
  # http://www.openml.org/apicall/id/[?key1=value1&key2=value2&...]
  if (url.args == "")
    url = sprintf("%s/%s%s", server, api.call, id)
  else
    url = sprintf("%s/%s%s?%s", server, api.call, id, url.args)
  return(url)
}

# Helper function to transform named list to HTTP query string.
# E.g. list(x = 123, y = openml) to x=123&y=openml.
collapseNamedList = function(args, sep = "=", collapse = "&") {
  collapse(paste(names(args), args, sep = sep), collapse)
}

# Helper function to do HTTP request.
# This function checks for errors.
doHTTRCall = function(method = "GET", url, query, body = NULL) {
  # build list of args for the httr::method call
  http.args = list(url = url, body = body, query = query)
  # filter empty body
  http.args = filterNull(http.args)
  # do the request and catch potential "unreadable" curl errors
  server.response = try(do.call(method, http.args), silent = TRUE)
  if (is.error(server.response)) {
    stopf("API call failed. Maybe you are not connected to the internet.")
  }
  # if we requested a document we need to extract the actual content
  if (method == "GET")
    server.response = rawToChar(server.response$content)
  return(server.response)
}
