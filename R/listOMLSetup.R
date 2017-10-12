.listOMLSetup = function(setup.id = NULL, flow.id = NULL, 
  limit = 1000, offset = NULL, verbosity = NULL) {
  api.call = generateAPICall(api.call = "json/setup/list",
    setup.id = setup.id, flow.id = flow.id, limit = limit, offset = offset)

  content = doAPICall(api.call, file = NULL, method = "GET", verbosity = verbosity)
  if (is.null(content)) return(data.frame())

  # Get entries, which are grouped by setup.id
  setups = fromJSON(txt = content, simplifyVector = FALSE)$setups$setup
  
  if (is.null(setups$parameter)) {
    par = extractSubList(setups, "parameter")
    setups = setNames(par, extractSubList(setups, "setup_id")) 
  } else {
    setups = setNames(list(setups$parameter), setups$setup_id)
  }
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
      ret = setDF(replace(setup, which(vlapply(setup, is.list)), NA_character_))
    }
  })
  # rbind the list
  # FIXME: rbindlist does not work anymore therefore do this:
  nrows = vnapply(setups, nrow)
  setup.id = rep(names(nrows[nrows != 0]), nrows[nrows != 0])
  setups = do.call(rbind, setups)
  #setups$setup.id = setup.id
  setups = cbind(data.frame(setup.id = setup.id, stringsAsFactors = FALSE), setups)
  #setups = rbindlist(setups, idcol = "setup_id")
  setups = lapply(setups, type.convert, as.is = TRUE)
  setups = as.data.frame(setups, stringsAsFactors = FALSE)

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

listHasName = function(l, name = "parameter") {
  if (name %in% names(l)) {
    return(TRUE)
  } else {
    if (is.list(l)) {
      any(vlapply(l, foo))
    } else {
      return(FALSE)
    }
  }
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
#' @example inst/examples/listOMLSetup
listOMLSetup = memoise(.listOMLSetup)
