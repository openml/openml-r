.listOMLStudies = function(alias = TRUE, status = "all", limit = NULL, offset = NULL,
  main.entity.type = NULL, uploader.id = NULL, verbosity = NULL) {

  api.call = generateAPICall("xml/study/list",
    uploader.id = uploader.id, limit = limit, offset = offset,
    status = status)

  if (!is.null(main.entity.type))
    api.call = paste0(api.call, "/main_entity_type/", main.entity.type)
  content = doAPICall(api.call = api.call, file = NULL, verbosity = verbosity, method = "GET")
  if (is.null(content)) return(data.table())

  doc = read_xml(paste(content, collapse = " "))

  args_names = c("name", "id")
  if (alias) args_names = c(args_names, "alias") # See https://docs.openml.org/benchmark/ , Listing the Benchmark Suites

  #   ** Might suffice to just use this commented block instead of remaining part. It suffices,
  #   if each study ALWAYS has at least the nodes/elements (which can be empty) "name", "id", "alias" **
  #
  #   args_xpQ = sprintf("//oml:%s", args_names)
  #   args = lapply(seq_along(args_names), function(i) xml_query(doc, args_xpQ[i], FALSE, TRUE, "S"))
  #   names(args) = convertNamesOMLToR(args_names)
  #
  #   res = setDT(args)
  #   res[]

  ns = xml_children(doc)
  get_text = function(node, xpQ) {
    txt = xml_text(xml_contents(xml_child(node, xpQ)))
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
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family listing functions
#' @export
listOMLStudies = memoise(.listOMLStudies)
