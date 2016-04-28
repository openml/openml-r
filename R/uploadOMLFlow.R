#' @title Upload an OpenML.
#'
#' @description
#' Share a flow by uploading it to the OpenML server.
#'
#' @note
#' This function will reset the cache of \code{\link{listOMLFlows}} on success.
#'
#' @param x [\code{\link{OMLFlow}}|\code{\link{Learner}}]\cr
#'   The flow that should be uploaded.
#' @template arg_upload_tags
#' @template arg_verbosity
#' @param sourcefile [\code{character(1)}]\cr
#'   The file path to the flow (not needed for \code{\link{Learner}}).
#' @param binaryfile [\code{character(1)}]\cr
#'   The file path to the flow (not needed for \code{\link{Learner}}).
#' @return [\code{invisible(numeric)}].
#'   The ID of the flow (\code{flow.id}). If there are more componets in the flow, than a vector of IDs.
#' @family uploading functions
#' @export
uploadOMLFlow = function(x, tags = NULL, verbosity = NULL, sourcefile, binaryfile) {
  UseMethod("uploadOMLFlow")
}

#' @export
uploadOMLFlow.OMLFlow = function(x, tags = NULL, verbosity = NULL, sourcefile = NULL, binaryfile = NULL) {
#   if (is.null(sourcefile) && is.null(binaryfile)) {
#     stopf("Please provide source and/or binary file.")
#   }
  if (length(x$components) > 0) {
    tmp = uploadOMLFlow(x$components[[1]])
    if (length(x$components) > 1) {
      for(i in 2:length(x$components)) tmp = c(uploadOMLFlow(x$components[[i]]), tmp)
    }
  } else tmp = NULL
  
  check = checkOMLFlow(x, verbosity = FALSE)
  doc = check$doc
  if (check$exists) {
    flow.id = xmlOValI(doc, "/oml:flow_exists/oml:id")
    showInfo(verbosity, "Flow already exists (Flow ID = %i).", flow.id)
    return(c(flow.id, tmp))
  }
  file = tempfile(fileext = ".xml")
  on.exit(unlink(file))

  if (!checkUserConfirmation(type = "flow")) {
    return(invisible())
  }

  writeOMLFlowXML(x, file)

  showInfo(verbosity, "Uploading flow to server.")
  showInfo(verbosity, "Downloading response to: %s", file)

  #url = getAPIURL("flow/")
  params = list(description = upload_file(path = file))

  # if binary.path is given (and binaryfile is empty), upload binary.path, otherwise upload binaryfile
  if (testFile(x$binary.path) & !testFile(binaryfile)) binaryfile = x$binary.path
  if (testFile(binaryfile)) {
    x$binary.md5 = digest(file = binaryfile)
    params$binary = upload_file(path = binaryfile)
  }
  if (testFile(x$source.path) & !testFile(sourcefile)) sourcefile = x$source.path
  if (testFile(sourcefile)) {
    x$source.md5 = digest(file = sourcefile)
    params$source = upload_file(path = sourcefile)
  }
#   sourcefile.exists = !(is.null(sourcefile) || is.na(sourcefile))
#   if (!(is.null(x$source.path) || is.na(x$source.path)) & !sourcefile.exists)
#     sourcefile = x$source.path
#   if (!(is.null(sourcefile) || is.na(sourcefile))){
#     assertFile(sourcefile)
#     x$source.md5 = digest(file = sourcefile)
#     params$source = upload_file(path = sourcefile)
#   }

  response = doAPICall(api.call = "flow", method = "POST", file = NULL,
    verbosity = verbosity, post.args = params)

  # response = postForm(url, .params = params, .checkParams = FALSE)
  doc = parseXMLResponse(response, "Uploading flow", c("upload_flow", "response"), as.text = TRUE)
  flow.id = xmlOValI(doc, "/oml:upload_flow/oml:id")
  showInfo(verbosity, "Flow successfully uploaded. Flow ID: %i", flow.id)
  if (!is.null(tags)) tagOMLObject(flow.id, object = "flow", tags = tags)
  forget(listOMLFlows)
  return(invisible(c(flow.id, tmp)))
}

#' @export
uploadOMLFlow.Learner = function(x, tags = NULL,
  verbosity = NULL, sourcefile = NULL, binaryfile = NULL) {
  flow = createOMLFlowForMlrLearner(x)

  # create sourcefile
  #sourcefile = createLearnerSourcefile(x)
  #on.exit(unlink(sourcefile))

  flow.id = uploadOMLFlow(flow, sourcefile = sourcefile, binaryfile = binaryfile, verbosity = verbosity)
  return(flow.id)
}

# FIXME: remove this when uploading flows without sourcefile are possible (and use setup.string instead)
# createLearnerSourcefile = function(x){
#   sourcefile = file.path(tempdir(), sprintf("%s_source.R", x$id))
#   xx = base64Encode(rawToChar(serialize(x, connection = NULL, ascii = TRUE)))
#   writeLines(sprintf("
# sourcedFlow = function(task.id) {
#   library(RCurl)
#   library(mlr)
#   task = getOMLTask(task.id)
#   x = unserialize(charToRaw(base64Decode('%s')))
#   runTaskMlr(task, x)
# }", xx), sourcefile)
#   return(sourcefile)
# }

checkOMLFlow = function(x, verbosity = NULL){
  if (inherits(x, "Learner")) x = createOMLFlowForMlrLearner(x)

  content = doAPICall(api.call = paste0("flow/exists/", x$name, "/", x$external.version),
    method = "GET", file = NULL, verbosity = verbosity)

  doc = parseXMLResponse(content, "Checking existence of flow", "flow_exists", as.text = TRUE)

  return(
    list(
      exists = as.logical(xmlRValS(doc, "/oml:flow_exists/oml:exists")),
      doc = doc
    )
  )
}

#' @title createOMLFlowForMlrLearner.
#'
#' @description
#' Creates an OMLFlow for an mlr learner.
#' Required if you want to upload an mlr learner.
#'
#' @param lrn [\code{\link[mlr]{Learner}}]\cr
#'   The mlr learner.
#' @param name [\code{character(1)}]\cr
#'   The name of the flow object. Default is the learner ID.
#' @param description [\code{character(1)}]\cr
#'   An optional description of the learner.
#'   Default is a short specification of the learner and the associated package.
#' @param ... [\code{any}]\cr
#'   Further optional parameters that are passed to \code{\link{makeOMLFlow}}.
#' @return [\code{\link{OMLFlow}}].
createOMLFlowForMlrLearner = function(lrn, name = paste0("mlr.", lrn$id), description = NULL, ...) {
  assertClass(lrn, "Learner")
  assertString(name)

  lrn = removeDefaultsFromParamValues(lrn)
  
  if (!is.null(description))
    assertString(description)
  else
    description = sprintf("Learner %s from package(s) %s.", name, collapse(lrn$package, sep = ", "))

  # dependencies
  lrn.package = ifelse(grepl("^!", lrn$package), gsub("^!", "", lrn$package), lrn$package)
  if ("mlr"%in%lrn.package) pkges = lrn.package else pkges = c("mlr", lrn.package)
  #pkges = c("mlr", lrn.package)
  pkges = sapply(pkges, function(x) sprintf("%s_%s", x, packageVersion(x)))
  pkges = collapse(pkges, sep = ", ")

  # create sourcefile
  #sourcefile = createLearnerSourcefile(lrn)
  #external.version = paste0("R_", digest(file = sourcefile)) #digest(file = sourcefile)
  #on.exit(unlink(sourcefile))

  # FIXME: currently we only want to allow mlr learners as flows, later we might want switch using sourcefiles again
  binary.path = file.path(tempdir(), sprintf("%s_binary.Rds", lrn$id))
  saveRDS(lrn, file = binary.path)
  external.version = paste0("R_", collapse(R.Version()[c("major", "minor")], "."), 
    "-v2.", digest(algo = "crc32", file = binary.path))

  flow = makeOMLFlow(
    name = name,
    description = description,
    parameters = makeFlowParameterList(lrn),
    dependencies = pkges,
    external.version = external.version,
    binary.path = binary.path,
    ...
  )
  if (!is.null(lrn$next.learner)) {
    identifier = gsub(".*[.]", "", lrn$next.learner$id) #stri_split_fixed(lrn$next.learner$id, ".")[[1L]][2L]
    flow$components = list(createOMLFlowForMlrLearner(lrn$next.learner))
    names(flow$components) = identifier
  }
  return(flow)
}

removeDefaultsFromParamValues = function(mlr.lrn) {
  par.defaults = getDefaults(getParamSet(mlr.lrn))
  par.vals = mlr.lrn$par.vals
  par.ind = vlapply(names(par.vals), function(x) !isTRUE(all.equal(par.defaults[[x]] , par.vals[[x]])))
  mlr.lrn$par.vals = par.vals[par.ind]
  
  if (!is.null(mlr.lrn$next.learner))
    mlr.lrn$next.learner = removeDefaultsFromParamValues(mlr.lrn$next.learner)
  
  return(mlr.lrn)
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
  pars = getParamSet(mlr.lrn)$pars #mlr.lrn$par.set$pars
  par.list = vector("list", length = length(pars))
  for(i in seq_along(pars)){
    name = pars[[i]]$id
    data.type = pars[[i]]$type
    # FIXME: data.type Should be either integer, numeric, string, vector, matrix, object.
    # if(data.type == "discrete") data.type = "string"      ?
    # if(data.type == "numericvector") data.type = "vector" ?
    # ...
    # For now, we don't want to store default values on the server.
    default.value = NA_character_
    flow.par = makeOMLFlowParameter(
      name = name,
      data.type = data.type,
      default.value = default.value
    )
    par.list[[i]] = flow.par
  }
  seed.pars = setNames(c(1, RNGkind()), c("openml.seed", "openml.kind", "openml.normal.kind"))
  par.list = append(par.list, lapply(seq_along(seed.pars), function(x) {
    makeOMLFlowParameter(
      name = names(seed.pars[x]),
      data.type = ifelse(is.numeric(seed.pars[x]), "integer", "discrete"),
      default.value = seed.pars[x]
    )}))
  return(par.list)
}
