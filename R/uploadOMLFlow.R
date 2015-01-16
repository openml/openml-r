uploadOMLFlow = function(x, session.hash, verbosity, sourcefile, binaryfile) {
  UseMethod("uploadOMLFlow")
}

uploadOMLFlow.OMLFlow = function(x, session.hash = getSessionHash(), verbosity = NULL, sourcefile = NULL, binaryfile = NULL) {
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
  url = getAPIURL("openml.implementation.exists", name = x$name, external_version = x$external.version)
  content = downloadXML(url, NULL, verbosity, session_hash = session.hash)
  doc = parseXMLResponse(content, "Checking existence of flow", "implementation_exists", as.text = TRUE)

  if (as.logical(xmlRValS(doc, "/oml:implementation_exists/oml:exists"))) {
    id = xmlOValI(doc, "/oml:implementation_exists/oml:id")
    showInfo(verbosity, "Flow already exists (ID = %i).", id)
    return(id)
  }
  file = tempfile()
  on.exit(unlink(file))
  writeOMLFlowXML(x, file)

  showInfo(verbosity, "Uploading implementation to server.")
  showInfo(verbosity, "Downloading response to: %s", file)

  url = getAPIURL("openml.implementation.upload")
  params = list(session_hash = session.hash, description = fileUpload(filename = file))
  if (!is.null(sourcefile))
    params$source = fileUpload(filename = sourcefile)
  if (!is.null(binaryfile))
    params$binary = fileUpload(filename = binaryfile)
  response = postForm(url, .params = params, .checkParams = FALSE)
  doc = parseXMLResponse(response, "Uploading implementation", c("upload_implementation", "response"), as.text = TRUE)
  id = xmlOValI(doc, "/oml:upload_implementation/oml:id")
  showInfo(verbosity, "Implementation successfully uploaded. Implementation ID: %i", id)
  return(id)
}

uploadOMLFlow.Learner = function(x, session.hash = getSessionHash(), verbosity = NULL) {
  flow = createOMLFlowForMlrLearner(x)

  # create sourcefile
  sourcefile = file.path(tempdir(), sprintf("%s_source.R", x$id))
  xx = base64Encode(serialize(x, ascii = TRUE, connection = NULL))
  writeLines(sprintf("
sourcedFlow = function(task.id) {
  library(RCurl)
  library(mlr)
  task = getOMLTask(task.id)
  x = unserialize(base64Decode('%s'))
  runTaskMlr(task, x)
}", xx), sourcefile)
  on.exit(unlink(sourcefile))

  flow.id = uploadOMLFlow(flow, sourcefile = sourcefile, session.hash = session.hash, verbosity)
  return(flow.id)
}

# createOMLFlowForMlrLearner.
#
# Create an OMLFlow for an mlr learner.
# Required if you want to upload an mlr learner.
#
# @param lrn [\code{\link[mlr]{Learner}}]\cr
#   The mlr learner.
# @param name [\code{character(1)}]\cr
#   The name of the implementation object. Default is the learner's ID.
# @param description [\code{character(1)}]\cr
#   An optional description of the learner.
#   Default is a short specification of the learner and the associated package.
# @param ... [\code{any}]\cr
#   Further optional parameters that are passed to \code{\link{makeOMLFlow}}.
# @return [\code{\link{OMLFlow}}].
createOMLFlowForMlrLearner = function(lrn, name = lrn$id, description = NULL, ...) {
  assertClass(lrn, "Learner")
  assertString(name)

  if (!is.null(description))
    assertString(description)
  else
    description = sprintf("Learner %s from package(s) %s.", name, collapse(lrn$package, sep = ", "))

  pkges = c("mlr", lrn$package)
  pkges = sapply(pkges, function(x) sprintf("%s_%s", x, packageVersion(x)))
  pkges = collapse(pkges, sep = ", ")
  flow = makeOMLFlow(
    name = name,
    # set package version of the "last" learner as the flow's external.version
    external.version = packageDescription(lrn$package[1L])$Version,
    description = description,
    parameter = makeFlowParameterList(lrn),
    dependencies = pkges,
    ...
  )
  if (!is.null(lrn$next.learner)) {
    identifier = str_split(lrn$next.learner$id, '[.]')[[1L]][2L]
    flow$components = list(createOMLFlowForMlrLearner(lrn$next.learner))
    names(flow$components) = identifier
  }
  return(flow)
}

# Generate a list of OpenML implementation parameters for a given mlr learner.
#
# @param mlr.lrn [\code{\link[mlr]{Learner}}]\cr
#   The mlr learner.
# @return A list of \code{\link{OpenMLImplementationParameter}s}.
# @examples
# library(mlr)
# lrn = makeLearner("classif.randomForest")
# pars = makeImplementationParameterList(lrn)
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
    if (pars[[i]]$has.default)
      default.value = as.character(pars[[i]]$default)
    else
      default.value = NA_character_
    flow.par = makeOMLFlowParameter(
      name = name,
      data.type = data.type,
      default.value = default.value)
    par.list[[i]] = flow.par
  }
  return(par.list)
}
