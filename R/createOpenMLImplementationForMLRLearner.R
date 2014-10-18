#' createOpenMLImplementationForMlrLearner.
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
createOpenMLImplementationForMlrLearner = function(lrn, name = lrn$id, description) { 
  assertClass(lrn, "Learner")
  assertString(name)

  if (!missing(description))
    assertString(description)
  else
    description = sprintf("Learner %s from package(s) %s.", name, collapse(lrn$package, sep = ", "))

  impl = makeOpenMLImplementation(
    name = name,
    # set package version of the basic learner's package as the flow's external.version
    external.version = packageDescription(lrn$package[length(lrn$package)])$Version,
    description = description,
    parameter = makeImplementationParameterList(lrn)
  )
  if (!is.null(lrn$next.learner)) {
    identifier = str_split(lrn$next.learner$id, '[.]')[[1]][2]
    impl$components = list(createOpenMLImplementationForMlrLearner(lrn$next.learner))
    names(impl$components) = identifier
  }
    
  return(impl)
}