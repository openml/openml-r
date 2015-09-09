#' @title List available Estimation Procedures
#'
#' @description
#' The returned \code{data.frame} contains the \code{est.id} and the corresponding
#' name of the estimation procedure.
#'
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family listing functions
#' @export
listOMLEstimationProcedures = function(verbosity = NULL) {
  content = try(doAPICall(api.call = "estimationprocedure/list", file = NULL, 
    verbosity = verbosity, method = "GET"))
  doc = parseXMLResponse(content, "Getting names of estimation procedures", "estimationprocedures",
    as.text = TRUE)
  data.frame(
    est.id = xmlValsMultNsS(doc, "/oml:estimationprocedures/oml:estimationprocedure/oml:id"),
    name = xmlValsMultNsS(doc, "/oml:estimationprocedures/oml:estimationprocedure/oml:name"),
    stringsAsFactors = TRUE
  )
}
