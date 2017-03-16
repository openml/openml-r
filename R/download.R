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
#' @keywords internal
doAPICall = function(api.call, id = NULL,
  url.args = list(), post.args = list(), file = NULL,
  verbosity = NULL, method, ...) {
  assertChoice(method, choices = c("GET", "POST", "DELETE"))
  assert(checkChoice(verbosity, choices = 0:2), checkNull(verbosity))

  # get config infos
  conf = getOMLConfig()

  # build request URL and query
  url = buildRequestURL(conf$server, api.call, id, url.args, ...)

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
  collapse(stri_paste(names(args), args, sep = sep), collapse)
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
  if (is.error(server.response) || !inherits(server.response, "response")) {
    if (has_internet()) {
      stopf("API call failed. The OpenML server '%s' is currently not available, try again later.",
        getOMLConfig()$server)
    } else {
      stopf("API call failed. Maybe you are not connected to the internet.")
    }
  }
  # handle HTTP non success status codes
  status.code = server.response$status_code
  if (!(status.code %btwn% c(200, 399))) {
    # select ERROR document parser based on response type, i.e., json, xml or html
    parseError = parseHTMLError
    if (isXMLResponse(server.response)) parseError = parseXMLError
    else if (isJSONResponse(server.response)) parseError = parseJSONError
    error = parseError(server.response)

    stopf("ERROR (code = %s) in server response: %s\n  %s\n", as.character(error$code), error$message,
      if (!is.null(error$additional.information)) error$additional.information else "")
  }

  # if we requested a document we need to extract the actual content
  if (method == "GET")
    server.response = rawToChar(server.response$content)
  return(server.response)
}

# Helper to check if HTTP call returned XML document.
#
# @param response [response]
#   Response of httr::method, e.g., httr::GET.
# @return [logical(1)]
isXMLResponse = function(response) {
  assertClass(response, "response")
  stri_detect_fixed(httr::http_type(response), "text/xml")
}

# Helper to check if HTTP call returned JSON document.
#
# @param response [response]
#   Response of httr::method, e.g., httr::GET.
# @return [logical(1)]
isJSONResponse = function(response) {
  assertClass(response, "response")
  stri_detect_fixed(httr::http_type(response), "application/json")
}

# Helpers to parse error documents.
#
# @param response [response]
#   Response of httr::method, e.g., httr::GET.
# @return [list] with components 'code', 'message' and optional 'extra'
parseHTMLError = function(response) {
  # no parsing here
  list(code = "<NA>", message = "<NA>",
    additional.information = "Server returned a HTML document!")
}

# see parseHTMLError for signature
parseXMLError = function(response) {
  content = rawToChar(response$content)
  xml.doc = try(xmlParse(content, asText = TRUE), silent = TRUE)
  if (is.error(xml.doc)) {
    return(list(code = "<NA>", message = "<NA>",
      additional.information = "Unable to parse XML error response."))
  }
  list(
    code = xmlRValI(xml.doc, "/oml:error/oml:code"),
    message = xmlOValS(xml.doc, "/oml:error/oml:message"),
    additional.information = xmlOValS(xml.doc, "/oml:error/oml:additional_information")
  )
}

# see parseHTMLError for signature
parseJSONError = function(response) {
  # parsed by httr (no need to call fromJSON by hand)
  error = content(response)$error
  names(error) = convertNamesOMLToR(names(error))
  return(error)
}
