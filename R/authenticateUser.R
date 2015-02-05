#' @title Authenticate at server.
#'
#' @description
#' Needs to be done once, before you are allowed to do any operation.
#' Defaults are set via the OpenML config.
#'
#' The session hash is stored in your config, so it will be automatically used for all
#' subsequent package operations.
#'
#' @param username [\code{character(1)}]\cr
#'   The e-mail address you used to register at the OpenML server.
#' @param password [\code{character(1)}]\cr
#'   Your password at OpenML server.
#' @template arg_verbosity
#' @return [\code{character(1)}]. Invisibly returns session hash.
#' @export
#' @family config
authenticateUser = function(username = NULL, password = NULL, verbosity = NULL) {
  conf = getOMLConfig()
  if (is.null(username))
    username = conf$username
  assertString(username)
  if (is.null(password)) {
    md5 = conf$pwdmd5
    if (is.na(md5))
      stop("Please provide the password.")
  } else {
    assertString(password)
    md5 = digest(password, algo = "md5", serialize = FALSE)
  }
  showInfo(verbosity, "Authenticating user at server: %s", username)
  url = getAPIURL("openml.authenticate", secure = FALSE)
  # FIXME: we might want to use https for this!
  content = try(postForm(url, .params = list(username = username, password = md5), 
    .checkParams = FALSE), silent = TRUE)

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
  expired = conf$session.hash.expires < Sys.time() + 60L
  if (is.null(hash) || expired) {
    auto.login = conf$is.user.config && !is.null(conf$username) && !is.null(conf$pwdmd5)
    if (is.null(hash)) {
      if (auto.login)
        hash = authenticateUser()
      else
        stop("Please authenticate first.")
    } else if (expired) {
      if (auto.login)
        hash = authenticateUser()
      else
        stop("Session hash expired. Please refresh your authentication.")
    }
  }
  return(hash)
}
