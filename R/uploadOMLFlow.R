#' Upload an OpenML flow to the server.
#'
#' Upload an OpenML flow to the server.
#'
#' @param x [\code{\link{OMLFlow}}|\code{\link{Learner}}]\cr
#'   The flow that should be uploaded.
#' @template arg_verbosity
#' @param sourcefile [\code{character(1)}]\cr
#'   The file path to the flow (not needed for \code{\link{Learner}})
#' @param binaryfile [\code{character(1)}]\cr
#'   The file path to the flow (not needed for \code{\link{Learner}})
#' @return [\code{invisible(numeric(1))}].
#'   The id of the flow (\code{flow.id}).
#' @family uploading functions
#' @export

uploadOMLFlow = function(x, verbosity, sourcefile, binaryfile) {
  UseMethod("uploadOMLFlow")
}

#' @export
uploadOMLFlow.OMLFlow = function(x, verbosity = NULL, sourcefile = NULL, binaryfile = NULL) {
  if (is.null(sourcefile) && is.null(binaryfile)) {
    stopf("Please provide source and/or binary file.")
  }
  if (!is.null(binaryfile)) {
    assertFile(binaryfile)
    x$binary.md5 = digest(file = binaryfile)
  }
  if (!is.null(sourcefile)) {
    assertFile(sourcefile)
    x$source.md5 = digest(file = sourcefile)
  }
  check = checkOMLFlow(x, verbosity = FALSE)
  doc = check$doc

  if (check$exists) {
    flow.id = xmlOValI(doc, "/oml:flow_exists/oml:id")
    showInfo(verbosity, "Flow already exists (Flow ID = %i).", flow.id)
    return(flow.id)
  }
  file = tempfile()
  on.exit(unlink(file))
  writeOMLFlowXML(x, file)

  showInfo(verbosity, "Uploading flow to server.")
  showInfo(verbosity, "Downloading response to: %s", file)

  #url = getAPIURL("flow/")
  params = list(description = upload_file(path = file))
  if (!is.null(sourcefile))
    params$source = upload_file(path = sourcefile)
  if (!is.null(binaryfile))
    params$binary = upload_file(path = binaryfile)

  response = doAPICall(api.call = "flow", method = "POST", file = NULL,
    verbosity = verbosity, post.args = params)

  # response = postForm(url, .params = params, .checkParams = FALSE)
  doc = parseXMLResponse(response, "Uploading flow", c("upload_flow", "response"), as.text = TRUE)
  flow.id = xmlOValI(doc, "/oml:upload_flow/oml:id")
  showInfo(verbosity, "Flow successfully uploaded. Flow ID: %i", flow.id)
  return(flow.id)
}

#' @export
uploadOMLFlow.Learner = function(x,
  verbosity = NULL, sourcefile = NULL, binaryfile = NULL) {
  flow = createOMLFlowForMlrLearner(x)

  # create sourcefile
  sourcefile = createLearnerSourcefile(x)
  on.exit(unlink(sourcefile))

  flow.id = uploadOMLFlow(flow, sourcefile = sourcefile, verbosity)
  return(flow.id)
}

# FIXME: remove this when uploading flows without sourcefile are possible (and use setup.string instead)
createLearnerSourcefile = function(x){
  sourcefile = file.path(tempdir(), sprintf("%s_source.R", x$id))
  xx = base64Encode(rawToChar(serialize(x, connection = NULL, ascii = TRUE)))
  writeLines(sprintf("
sourcedFlow = function(task.id) {
  library(RCurl)
  library(mlr)
  task = getOMLTask(task.id)
  x = unserialize(charToRaw(base64Decode('%s')))
  runTaskMlr(task, x)
}", xx), sourcefile)
  return(sourcefile)
}

checkOMLFlow = function(x, verbosity = NULL){
  if(inherits(x, "Learner")) x = createOMLFlowForMlrLearner(x)

  content = doAPICall(api.call = paste0("flow/exists/", x$name, "/", x$external.version),
                      method = "GET", file = NULL, verbosity = verbosity)

  doc = parseXMLResponse(content, "Checking existence of flow", "flow_exists", as.text = TRUE)

  return(list(exists = as.logical(xmlRValS(doc, "/oml:flow_exists/oml:exists")),
    doc = doc))
}

#' @title createOMLFlowForMlrLearner.
#'
#' @description Creates an OMLFlow for an mlr learner.
#'   Required if you want to upload an mlr learner.
#'
#' @param lrn [\code{\link[mlr]{Learner}}]\cr
#'   The mlr learner.
#' @param name [\code{character(1)}]\cr
#'   The name of the flow object. Default is the learner's ID.
#' @param description [\code{character(1)}]\cr
#'   An optional description of the learner.
#'   Default is a short specification of the learner and the associated package.
#' @param ... [\code{any}]\cr
#'   Further optional parameters that are passed to \code{\link{makeOMLFlow}}.
#' @return [\code{\link{OMLFlow}}].
createOMLFlowForMlrLearner = function(lrn, name = lrn$id, description = NULL, ...) {
  assertClass(lrn, "Learner")
  assertString(name)

  if (!is.null(description))
    assertString(description)
  else
    description = sprintf("Learner %s from package(s) %s.", name, collapse(lrn$package, sep = ", "))

  # dependencies
  pkges = c("mlr", lrn$package)
  pkges = sapply(pkges, function(x) sprintf("%s_%s", x, packageVersion(x)))
  pkges = collapse(pkges, sep = ", ")

  # create sourcefile
  sourcefile = createLearnerSourcefile(lrn)
  #external.version = paste0("R_", digest(file = sourcefile)) #digest(file = sourcefile)
  on.exit(unlink(sourcefile))

  # FIXME: currently we only want to allow mlr learners as flows, later we might want switch using sourcefiles again
  external.version = paste0("R_", collapse(R.Version()[c("major", "minor")], "."), "-", digest(pkges, "crc32"))

  flow = makeOMLFlow(
    name = name,
    description = description,
    parameters = makeFlowParameterList(lrn),
    dependencies = pkges,
    external.version = external.version,
    ...
  )
  if (!is.null(lrn$next.learner)) {
    identifier = stri_split_fixed(lrn$next.learner$id, ".")[[1L]][2L]
    flow$components = list(createOMLFlowForMlrLearner(lrn$next.learner))
    names(flow$components) = identifier
  }
  return(flow)
}

# Generate a list of OpenML flow parameters for a given mlr learner.
#
# @param mlr.lrn [\code{\link[mlr]{Learner}}]\cr
#   The mlr learner.
# @return A list of \code{\link{OpenMLFlowParameter}s}.
# @examples
# library(mlr)
# lrn = makeLearner("classif.randomForest")
# pars = makeFlowParameterList(lrn)
# pars
makeFlowParameterList = function(mlr.lrn) {
  pars = mlr.lrn$par.set$pars
  par.list = vector("list", length = length(pars))
  for(i in seq_along(pars)){
    name = pars[[i]]$id
    data.type = pars[[i]]$type
    # FIXME: data.type Should be either integer, numeric, string, vector, matrix, object.
    # if(data.type == "discrete") data.type = "string"      ?
    # if(data.type == "numericvector") data.type = "vector" ?
    # ...
    # FIXME: For now, we don't want to store default values on the server.
#     if (pars[[i]]$has.default & length(pars[[i]]$default) != 0)
#       default.value = as.character(pars[[i]]$default)
#     else
   default.value = NA_character_
    flow.par = makeOMLFlowParameter(
      name = name,
      data.type = data.type,
      default.value = default.value)
    par.list[[i]] = flow.par
  }
  seed.pars = setNames(c(1, RNGkind()), c("seed", "kind", "normal.kind"))
  par.list = append(par.list, lapply(seq_along(seed.pars), function(x) {
    makeOMLFlowParameter(
      name = names(seed.pars[x]),
      data.type = ifelse(is.numeric(seed.pars[x]), "integer", "discrete"),
      default.value = seed.pars[x]
    )}))
  return(par.list)
}
