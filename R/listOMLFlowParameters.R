.listOMLFlowParameters = function(flow.id = NULL, tag = NULL, limit = NULL, offset = NULL, verbosity = NULL) {
  # FIXME: add filter for setup.id?
  api.call = generateAPICall(api.call = "json/setup/list", flow.id = flow.id,
    tag = tag, limit = limit, offset = offset)

  content = doAPICall(api.call, file = NULL, method = "GET", verbosity = verbosity)
  if (is.null(content)) return(data.frame())

  # Get entries, which are grouped by setup.id
  setups = fromJSON(txt = content, simplifyVector = FALSE)$setups$setup
  setups = setNames(extractSubList(setups, "parameter"), extractSubList(setups, "setup_id"))
  # setups = lapply(names(setups), function(i) Map(c, setups[[i]], setup_id = i))

  # We need to postprocess the list
  setups = lapply(setups, function(setup) {
    # for each setup.id check if it has one or more than one hyperparameters
    if (is.null(names(setup))) {
      # if there are more than two entries (hyperparameters) create a dataframe:
      ret = rbindlist(lapply(setup, function(x) {
        replace(x, which(vlapply(x, is.list)), NA_character_)
      }))
    } else {
      # if there is only one entry (hyperparameter) do this to create a dataframe:
      setDF(replace(x, which(vlapply(x, is.list)), NA_character_))
    }
  })
  # rbind the list
  setups = as.data.frame(rbindlist(setups, idcol = "setup_id"))

  #   # We need to postprocess the list
  #   setups = lapply(names(setups), function(i) {
  #     if (is.null(names(setups[[i]]))) {
  #       ret = Map(c, setups[[i]], setup_id = i)
  #       lapply(ret, function(x) replace(x, which(vlapply(x, is.list)), NA_character_))
  #     } else {
  #       list(c(replace(x, which(vlapply(x, is.list)), NA_character_), setup_id = i))
  #     }
  #   })
  #   # rbind the list
  #   setups = rbindlist(unlist(setups, recursive = FALSE))

  setups$id = setups$full_name = NULL
  names(setups) = convertNamesOMLToR(names(setups))
  return(setups)
}

#' @title List hyperparameters of Flows.
#'
#' @description
#' Lists hyperparameter settings for flows.
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
