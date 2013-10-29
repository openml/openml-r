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