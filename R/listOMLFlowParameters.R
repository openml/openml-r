.listOMLFlowParameters = function(flow.id = NULL, tag = NULL, limit = NULL, offset = NULL, verbosity = NULL) {
  # FIXME: add filter for setup.id
  api.call = generateAPICall(api.call = "json/setup/list", flow.id = flow.id,
    tag = tag, limit = limit, offset = offset)

  content = doAPICall(api.call, file = NULL, method = "GET", verbosity = verbosity)
  if (is.null(content)) return(data.frame())

  # for each setup.id there are multiple hyperpars => returns data.frame for each setup.id
  setups = fromJSON(txt = content)$setups$setup
  setups = setNames(setups$parameter, setups$setup_id)

  # if setup.id has only one hyperparameter then we need some postprocessing
  not.df = unname(which(!vlapply(setups, is.data.frame)))
  for (i in not.df) {
    empty.cols = vnapply(setups[[i]], length) == 0
    empty.cols = names(empty.cols[empty.cols])
    for (j in empty.cols) setups[[i]][[j]] = NA
    setups[[i]] = setDF(setups[[i]])
  }

  # rbind the list
  setups = as.data.frame(rbindlist(setups, idcol = "setup_id"))
  setups$id = setups$full_name = NULL

  # some columns are data.frames, we need to postprocess this
  list.cols = vlapply(setups, is.list) #c("data.type", "default.value", "value")
  list.cols = names(list.cols[list.cols])
  setups[list.cols] = lapply(list.cols, function(i) {
    col = setups[[i]]
    ind = vlapply(col, function(x) length(x) == 0)
    col[ind] = NA
    col[col == "null"] = NA
    return(unlist(col))
  })
  setups$value[setups$value == c("null")] = NA

  #setups$data.type = as.character(lapply(setups$data.type, unlist))
  #setups$default.value = as.character(lapply(setups$default.value, unlist))

  # setups = fromJSON(txt = content, simplifyVector = FALSE)$setups$setup
  # sid = extractSubList(setups, "setup_id")
  # setups = setNames(extractSubList(setups, "parameter"), sid)

  names(setups) = convertNamesOMLToR(names(setups))
  return(setups)
}

#' @title List Hyperparameters of flows.
#'
#' @description
#' DESCRIBE HERE
#'
#' @template note_memoise
#'
#' @template arg_flow.id
#' @template arg_tag
#' @template arg_limit
#' @template arg_offset
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family listing functions
#' @export
#' @example inst/examples/listOMLFlowParameters
listOMLFlowParameters = memoise(.listOMLFlowParameters)
