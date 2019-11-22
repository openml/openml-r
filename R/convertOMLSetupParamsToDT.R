# https://test.openml.org/api/v1/evaluation/setup/list/flow/2117/task/403/limit/100 works, while https://test.openml.org/api/v1/evaluation/setup/list/flow/6794 returns nothing.

# Result size limits are okay, as long as I can somehow reliably iterate with the offset, i.e.
# https://test.openml.org/api/v1/evaluation/setup/list/flow/2117/task/403/function/predictive_accuracy/limit/1/offset/2



#' @title Extract Parameters from an OpenML run into a flat structure
#'
#' @param run.evals [\code{data.frame}]\cr
#'   Result of caling listOMLRunEvaluations(..., setup = TRUE).
#' @param drop.constant [\code{logical(1)]\cr
#'  Should constant columns be dropped before returning the result?
#'
#' @return [\code{\link{data.table}}].
#' @family run-related functions
#' @export
convertOMLRunEvalsToDT = function(run.evals, drop.constant = TRUE) {
  assert_data_frame(run.evals)
  assert_true(!is.null(run.evals$setup_parameters))
  assert_flag(drop.constant)
  setup_params = run.evals$setup_parameters
  out = lapply(setup_params, function(params) {
    params[!(params$parameter_name == "verbose" & params$data_type == "boolean"), ]
    params[, convertValueByType(params$parameter_name, params$value, params$data_type)]
  })
  dt = rbindlist(out, fill = TRUE)
  if (drop.constant) dt = dt[, vlapply(dt, function(x) length(unique(x)) > 1), with = FALSE]
  run.evals$setup_parameters = NULL
  return(cbind(run.evals, dt))
}


# Convert values according to a parameter's type.
# Note that this is very unreliably.
convertValueByType = function(parameter_name, value, type) {
  value = Map(function(v, t) {
    v[v == "None" | v == "none" | v == "Null" | v == "null"] = NA
    v = gsub("&quot;", "", v)
    if (t %in% c("boolean", "bool")) v = as.logical(v)
    else if (t %in% c("float", "number")) v = as.numeric(v)
    else if (t %in% c("int", "integer", "int or None", "integer or None")) v = suppressWarnings(as.integer(v))
    return(v)
  }, value, type)
  names(value) = gsub("&quot;", "", parameter_name)
  return(as.data.table(value))
}
