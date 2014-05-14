#' Upload an OpenML implementation to the server.
#' 
#' @param implementation [\code{\link{OpenMLImplementation}}]\cr 
#'   An OpenMLImplementation object. Should at least contain a name and a description.
#' @param sourcefile [\code{character(1)}]\cr
#'   The source code of the implementation. If multiple files, please zip them. 
#'   Either source or binary is required.
#' @param binaryfile [\code{character(1)}]\cr
#'   The binary of the implementation. If multiple files, please zip them. 
#'   Either source or binary is required.
#' @param session.hash [\code{character(1)}]\cr
#'   A session token returned by \code{\link{authenticateUser}}.
#' @param show.info [\code{logical(1)}]\cr
#'   Verbose output on console?
#'   Default is \code{TRUE}.
#' @export
uploadOpenMLImplementation <- function(implementation, sourcefile, binaryfile, session.hash, 
  show.info = TRUE) {
  
  # Generate a sourcefile, if user doesn't provide one. Just for now. (?)
  # FIXME: makes no sense for non-mlr implementations 
  file <- file.path(getwd(), sprintf("%s_source.R", implementation@name))
  user.prov.srcfile <- TRUE
  if (missing(sourcefile)) {
    catf(file = file, "library(mlr) \nlrn <- makeLearner(\"%s\")", implementation@name)
    sourcefile <- file
    user.prov.srcfile <- FALSE
  }
  
  file <- tempfile()
  
  writeOpenMLImplementationXML(implementation, file)
  
   if (show.info) {
     messagef("Uploading implementation to server.")
     messagef("Downloading response to: %s", file)
   }
  
  url <- getServerFunctionURL("openml.implementation.upload")
  #FIXME: handle binary
  response <- postForm(url, 
    session_hash = session.hash,
    description = fileUpload(filename = file),
    source = fileUpload(filename = sourcefile)
  )
  write(response, file = file)

  doc <- parseXMLResponse(file, "Uploading implementation", c("upload_implementation", "response"))
  
  if (show.info) {
    messagef("Implementation successfully uploaded. Implementation ID: %s", 
      xmlOValS(doc, "/oml:upload_implementation/oml:id"))
  }  

  if (!user.prov.srcfile)
    unlink(sourcefile)
}

# 
# POST description (Required)
# An XML file containing the implementation meta data
# POST source
# The source code of the implementation. If multiple files, please zip them. Either source or binary is required.
# POST binary
# The binary of the implementation. If multiple files, please zip them. Either source or binary is required.
# POST session_hash (Required)
# The session hash, provided by the server on authentication (1 hour valid)