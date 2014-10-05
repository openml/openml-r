#' createOpenMLImplementationForMLRLearner.
#'
#' Create an OpenML implementation description object for an mlr learner. 
#' Required if you want to upload an mlr learner.
#' 
#' @param lrn [\code{\link[mlr]{Learner}}]\cr 
#'   The mlr learner.
#' @param name [\code{character(1)}]\cr 
#'   The name of the implementation object. Default is the learner's ID.
#' @param description [\code{character(1)}]\cr
#'   An optional description of the learner. 
#'   Default is a short specification of the learner and the associated package.
#' @return [\code{\link{OpenMLImplementation}}]. 
#' @export
createOpenMLImplementationForMLRLearner = function(lrn, name = lrn$id, description) { 
  assertClass(lrn, "Learner")
  assertString(name)

  if (!missing(description))
    assertString(description)
  else
    description = sprintf("Learner %s from package %s.", name, lrn$package)

  impl = makeOpenMLImplementation(
    name = name,
    external.version = packageDescription(lrn$package)$Version,
    description = description,
    parameter = makeImplementationParameterList(lrn)
  )
  return(impl)
}