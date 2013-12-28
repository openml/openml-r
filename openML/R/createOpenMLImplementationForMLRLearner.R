#' createOpenMLImplementationForMLRLearner.
#'
#' Create an OpenML implementation description object for an mlr learner. 
#' Required if you want to upload an mlr learner.
#' 
#' @param lrn [\code{\link[mlr]{Learner}}]\cr 
#'   The mlr learner.
#' @param name [\code{character(1)}]\cr 
#'   The name of the implementation object. Default is the learner's ID.
#' @param version [\code{character(1)}]\cr 
#'   The version of the implementation. Default is the version of the package.
#' @param description [\code{character(1)}]\cr
#'   An optional description of the learner. 
#'   Default is a short specification of the learner and the associated package.
#' @return [\code{\link{OpenMLImplementation}}]. 
#' @export
createOpenMLImplementationForMLRLearner <- function(
  lrn, 
  name = lrn$id, 
  version, 
  description) {
  # FIXME: Do we want to leave the chance for the user to change name and/or version?
  
  checkArg(lrn, "Learner")
  checkArg(name, "character")
  if(!missing(version))
    checkArg(version, "character")
  if(!missing(description))
    checkArg(description, "character")
  
  if(missing(description)) {
    description <- sprintf("Learner %s from package %s.", name, lrn$package)
  }
  if(missing(version)) {
    version <- packageDescription(lrn$package)$Version
  }
  impl <- OpenMLImplementation(
    name = name,
    version = version,
    description = description,
    parameter = makeImplementationParameterList(lrn)
  )
  return(impl)
  #file <- sprintf("%s/sourcefile.R", getwd())
  ## FIXME: generate a generic sourcefile?
  #content <- ""
  #save(content, file = file)
  
  #uploadOpenMLImplementation(impl, sourcefile = file, session.hash = session.hash) 
  
  #unlink(file)
}