#' Authenticate at server.
#'
#' Required if you want to upload anything.
#'
#' @param email [\code{character(1)}]\cr
#'   Your E-mail address at OpenMl server.
#' @param password [\code{character(1)}]\cr
#'   Your password at OpenML server.
#' @template arg_showinfo
#' @return [\code{character(1)}]. Session hash for further communication.
#' @export
authenticateUser = function(email, password, show.info = getOpenMLOption("show.info")) {
  assertString(email)
  assertString(password)
  assertFlag(show.info)
  # FIXME: we might want to use https for this!
  file = tempfile()
  on.exit(unlink(file))
  if (show.info) {
    messagef("Authenticating user at server: %s", email)
  }
  url = getServerFunctionURL("openml.authenticate")
  md5 = digest(password, algo = "md5", serialize = FALSE)
  params = list(username = email, password = md5)
  content = postForm(url, .params = params, .checkParams = FALSE)
  write(content, file = file)
  parseAuthenticateUserResponse(file, show.info)
}


parseAuthenticateUserResponse = function(file, show.info) {
  assertFile(file)
  doc = parseXMLResponse(file, "Authenticating user", "authenticate")
  session.hash = xmlRValS(doc, "/oml:authenticate/oml:session_hash")
  valid.until = xmlRValS(doc, "/oml:authenticate/oml:valid_until")
  if (show.info)
    messagef("Retrieved session hash. Valid until: %s", valid.until)
  return(session.hash)
}
