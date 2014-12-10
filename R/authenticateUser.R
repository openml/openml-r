#' @title Authenticate at server.
#'
#' @description
#' Needs to be done once, before you are allowed to do any operation.
#' Defaults are set via the OpenML config.
#'
#' The session hash is stored in your config, so it will be automatically used for all
#' subsequent package operations.
#'
#' @param email [\code{character(1)}]\cr
#'   Your e-mail address at OpenML server.
#' @param password [\code{character(1)}]\cr
#'   Your password at OpenML server.
#' @template arg_verbosity
#' @return [\code{character(1)}]. Invisibly returns session hash.
#' @export
#' @family config
authenticateUser = function(email = NULL, password = NULL, verbosity = NULL) {
  conf = getOMLConfig()
  if (is.null(email))
    email = conf$username
  assertString(email)
  if (is.null(password)) {
    md5 = conf$pwdmd5
    if (is.na(md5))
      stop("Please provide the password.")
  } else {
    assertString(password)
    md5 = digest(password, algo = "md5", serialize = FALSE)
  }
  showInfo(verbosity, "Authenticating user at server: %s", email)
  url = getAPIURL("openml.authenticate", secure = FALSE)
  # FIXME: we might want to use https for this!
  content = try(postForm(url, .params = list(username = email, password = md5), .checkParams = FALSE), silent = TRUE)

  if (is.error(content))
    stop("Username/password combination invalid.")

  doc = parseXMLResponse(content, "Authenticating user", "authenticate", as.text = TRUE)
  session.hash = xmlRValS(doc, "/oml:authenticate/oml:session_hash")
  valid.until = xmlRValS(doc, "/oml:authenticate/oml:valid_until")

  showInfo(verbosity, "Retrieved session hash. Valid until: %s", valid.until)

  # set auth data in config
  conf$session.hash = session.hash
  conf$session.hash.expires = as.POSIXct(valid.until)

  invisible(session.hash)
}

#' Get the current session's hash.
#' @return [\code{character(1)}].
#' @export
#' @family config
getSessionHash = function() {
  conf = getOMLConfig()
  hash = conf$session.hash
  expires = conf$session.hash.expires
  if (is.null(hash)) # ... session expire, renew authenticaton, etc
    stop("Please authenticate first.")
  if (expires < Sys.time() + 60)
    stop("Session hash expired. Please refresh your authentication.")
  return(hash)
}
