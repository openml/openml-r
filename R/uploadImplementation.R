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
#' @param delete.source.binary [\code{logical(1)}]\cr   
#'   Should downloaded source and binary files be removed at the end? Default is \code{FALSE}.
#' @template arg_showinfo
#' @export
uploadOpenMLImplementation = function(implementation, sourcefile, binaryfile, session.hash, 
  delete.source.binary = FALSE, show.info = getOpenMLOption("show.info")) {
  
  if (missing(sourcefile) && missing(binaryfile)) {
    stopf("Please provide source and/or binary file.")
  }
  if (!missing(binaryfile)) {
    assertFile(binaryfile)
    implementation$binary.md5 = digest(file = binaryfile)
  }
  if (!missing(sourcefile)) {
    assertFile(sourcefile)
    implementation$source.md5 = digest(file = sourcefile)
  }
  on.exit({
    if (delete.source.binary) {
      if (!missing(binaryfile)) 
        unlink(binaryfile)
      if (!missing(sourcefile))
        unlink(sourcefile)
    }
  })
  assertClass(implementation, "OpenMLImplementation")
  assertString(session.hash)
  assertFlag(delete.source.binary)
  assertFlag(show.info)
  
  exist.check = checkOpenMLFlowForExistance(implementation)
  if (exist.check$exists) {
    catf("Flow already exists (ID = %i).", exist.check$id)
    return(exist.check$id)
  } 
  file = tempfile()
  on.exit(unlink(file), add = TRUE)
  writeOpenMLImplementationXML(implementation, file)
  
  if (show.info) {
    messagef("Uploading implementation to server.")
    messagef("Downloading response to: %s", file)
  }
  
  url = getServerFunctionURL("openml.implementation.upload")
  #FIXME: handle binary
  response = postForm(url, 
    session_hash = session.hash,
    description = fileUpload(filename = file),
    source = fileUpload(filename = sourcefile)
  )
  write(response, file = file)

  doc = parseXMLResponse(file, "Uploading implementation", c("upload_implementation", "response"))
  
  id = xmlOValI(doc, "/oml:upload_implementation/oml:id")
  if (show.info) {
    messagef("Implementation successfully uploaded. Implementation ID: %i", id)
  }  
  return(id)
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