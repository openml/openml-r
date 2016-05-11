.listOMLEstimationProcedures = function(verbosity = NULL) {
  content = doAPICall(api.call = "estimationprocedure/list", file = NULL,
    verbosity = verbosity, method = "GET")
  doc = parseXMLResponse(content, "Getting names of estimation procedures", "estimationprocedures",
    as.text = TRUE)
  data.frame(
    est.id = xmlValsMultNsS(doc, "/oml:estimationprocedures/oml:estimationprocedure/oml:id"),
    name = xmlValsMultNsS(doc, "/oml:estimationprocedures/oml:estimationprocedure/oml:name"),
    stringsAsFactors = TRUE
  )
}

#' @title List available estimation procedures.
#'
#' @description
#' The returned \code{data.frame} contains the \code{est.id} and the corresponding
#' name of the estimation procedure.
#'
#' @template note_memoise
#'
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family listing functions
#' @export
#' @example inst/examples/listOMLEstimationProcedures.R
listOMLEstimationProcedures = memoise(.listOMLEstimationProcedures)
