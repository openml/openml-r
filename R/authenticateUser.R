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
authenticateUser = function(email = NULL, password = NULL, verbosity = NULL) {
  conf = getOMLConfig()
  if (is.null(email)) {
    email = conf$username
  } else {
    assertString(email)
  }
  if (is.null(password)) {
    md5 = conf$pwdmd5
  } else {
    assertString(password)
    md5 = digest(password, algo = "md5", serialize = FALSE)
  }
  showInfo(verbosity, "Authenticating user at server: %s", email)
  url = getAPIURL("openml.authenticate", secure = FALSE)

  # FIXME: we might want to use https for this!
  content = postForm(url, verbosity, username = email, password = md5, .checkParams = FALSE)
  doc = parseXMLResponse(content, "Authenticating user", "authenticate", as.text = TRUE)
  session.hash = xmlRValS(doc, "/oml:authenticate/oml:session_hash")
  valid.until = xmlRValS(doc, "/oml:authenticate/oml:valid_until")
  
  showInfo(verbosity, "Retrieved session hash. Valid until: %s", valid.until)

  SESSION_HASH <<- session.hash
  SESSION_HASH_EXPIRES <<- as.POSIXct(valid.until)

  return(session.hash)
}

#' Get the current session's hash
#' @return [\code{character(1)}].
#' @export
getSessionHash = function() {
  if (is.null(SESSION_HASH)) # ... session expire, renew authenticaton, etc.
    stop("Please authenticate first.")
  if (Sys.time() > SESSION_HASH_EXPIRES)
    stop("Session hash expired. Please refresh your authentication.")
  SESSION_HASH
}
