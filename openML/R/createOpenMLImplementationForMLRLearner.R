createOpenMLImplementationForMLRLearner <- function(
  lrn, 
  name = lrn$id, 
  version = "1.0", 
  description) {
  
  if(missing(description)) {
    description <- sprintf("Learner %s from package %s.", name, lrn$package)
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