#' @title List available Estimation Procedures
#'
#' @description
#' The returned \code{data.frame} contains the \code{est.id} and the corresponding
#' name of the estimation procedure.
#'
#' @template arg_hash
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @export
listOMLEstimProcs = function(session.hash = getSessionHash(), verbosity = NULL) {
  query = "SELECT DISTINCT id,name FROM estimation_procedure"
  # FIXME: remove rjson from DESCRIPTION when API is available
  parsed.doc = rjson::fromJSON(file = sprintf("http://www.openml.org/api_query/?q=%s", URLencode(query)))
  ret = do.call("rbind", parsed.doc$data)

  data.frame(est.id = as.numeric(ret[,1]),
             name = as.character(ret[,2]),
             stringsAsFactors = FALSE)
}
