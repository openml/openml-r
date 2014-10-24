#' Authenticate at server.
#'
#' Required if you want to upload anything.
#'
#' @param email [\code{character(1)}]\cr
#'   Your e-mail address at OpenMl server. Default is set via \code{\link{setOMLConfig}}.
#' @param password [\code{character(1)}]\cr
#'   Your password at OpenML server. Default is set via \code{\link{setOMLConfig}}.
#' @template arg_verbosity
#' @return [\code{character(1)}]. Session hash for further communication.
#' @export
authenticateUser = function(email, password, verbosity = NULL) {
  conf = getOMLConfig()
  if (!missing(email)) 
    assertString(email)
  else
    email = conf$username
  if (!missing(password)) {
    assertString(password)
    md5 = digest(password, algo = "md5", serialize = FALSE)
  } else {
    md5 = conf$pwdmd5
  }
  showInfo(verbosity, "Authenticating user at server: %s", email)
  # FIXME: we might want to use https for this!
  url = getAPIURL("openml.authenticate", secure = FALSE)

  content = postForm(url, verbosity, username = email, password = md5, .checkParams = FALSE)
  doc = parseXMLResponse(content, "Authenticating user", "authenticate", as.text = TRUE)
  session.hash = xmlRValS(doc, "/oml:authenticate/oml:session_hash")
  
  showInfo(verbosity, "Retrieved session hash. Valid until: %s", 
    xmlRValS(doc, "/oml:authenticate/oml:valid_until"))

  return(session.hash)
}
