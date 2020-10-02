.listOMLStudies = function(main.entity.type = NULL, status = "all",
  uploader.id = NULL, limit = NULL, offset = NULL, verbosity = NULL) {

  assertChoice(main.entity.type, choices = c("task", "run"), null.ok = TRUE)
  api.call = generateAPICall("xml/study/list",
    uploader.id = uploader.id, limit = limit, offset = offset,
    status = status)

  if (!is.null(main.entity.type))
    api.call = paste0(api.call, "/main_entity_type/", main.entity.type)
  content = doAPICall(api.call = api.call, file = NULL, verbosity = verbosity, method = "GET")
  if (is.null(content)) return(data.table())

  doc = xml2::read_xml(paste(content, collapse = " "))

  args_names = c("name", "id", "alias") # See https://docs.openml.org/benchmark/ , Listing the Benchmark Suites

  #   ** Might suffice to just use this commented block instead of remaining part. It suffices,
  #   if each study ALWAYS has at least the nodes/elements (which can be empty) "name", "id", "alias" **
  #
  #   args_xpQ = sprintf("//oml:%s", args_names)
  #   args = lapply(seq_along(args_names), function(i) xml_query(doc, args_xpQ[i], FALSE, TRUE, "S"))
  #   names(args) = convertNamesOMLToR(args_names)
  #
  #   res = setDT(args)
  #   res[]

  ns = xml2::xml_children(doc)
  get_text = function(node, xpQ) {
    txt = xml2::xml_text(xml2::xml_contents(xml2::xml_child(node, xpQ)))
    if (length(txt) == 0) txt = ""
    return(txt)
  }
  get_data = function(node, args_names) {
    dt = lapply(args_names, function(x) get_text(node, sprintf("oml:%s", x)))
    names(dt) = args_names
    return(dt)
  }
  studies_list = lapply(seq_along(ns), function(i) get_data(ns[[i]], args_names))
  res = rbindlist(studies_list)

  res = type.convert(res, as.is = TRUE, how = "replace")
  return(res)
}

#' @title list OpenML Studies.
#'
#' @description
#' Retrives a list of available studies.
#'
#' @template note_memoise
#'
#' @param main.entity.type [\code{character}]\cr
#'   Whether a collection of runs (study) or collection of tasks (benchmark suite) should be returned.
#'   Subsets the results according to the entity type.
#'   Possible values are  \code{{NULL, "task", "run"}}.
#'   Default is \code{NULL} which means that no subsetting is done.
#' @param uploader.id [\code{integer}]\cr
#'  a single ID or a vector of IDs of uploader profile(s).
#' @template arg_status
#' @template arg_limit
#' @template arg_offset
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family listing functions
#' @export
listOMLStudies = memoise(.listOMLStudies)
