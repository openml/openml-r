#' @title Perform an API call to the OpenML server
#'
#' @description The function always returns the XML file content provided by the
#' server.
#'
#' @param api.call [\code{character(1)}]\cr
#'   API endpoints listed in \href{https://github.com/openml/OpenML/wiki/API-v1}{APIv1}
#' @param id [\code{integer(1)}]\cr
#'   Optional ID we pass to the API, like runs/list/1.
#' @param url.args [\code{list}]\cr
#'   Named list of key value pairs passed as a GET parameter list, e.g.,
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
#' @return [\code{character(1)}]\cr Unparsed content of the returned XML file.
#' @export
#' @keywords internal
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
  #url.args$api_key = conf$apikey
  url.args = namedArgsListToHTTPQuery(url.args)

  # create url
  if (url.args == "") 
    url = sprintf("%s/%s%s", conf$server, api.call, id) 
  else
    url = sprintf("%s/%s%s?%s", conf$server, api.call, id, url.args)
  
  from.url = ifelse(method == "GET", "Downloading from", 
    ifelse(method == "POST", "Uploading to", "Deleting from"))
  showInfo(verbosity, "%s '%s' to '%s'", from.url, url, ifelse(is.null(file), "<mem>", file))

  if (method == "GET") {
    #content = GET(url = url)
    content = GET(url = url, query = list(api_key = conf$apikey))
    content = rawToChar(content$content)
  } else if (method == "POST") {
    content = POST(url = url, body = post.args, query = list(api_key = conf$apikey))
  } else if (method == "DELETE")
    content = DELETE(url = url, query = list(api_key = conf$apikey))

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
