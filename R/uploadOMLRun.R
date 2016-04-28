#' @title Upload an OpenML run.
#'
#' @description
#' Share a run of a flow on a given OpenML task by uploading it to the OpenML server.
#'
#' @note
#' This function will reset the cache of \code{\link{listOMLRuns}} and
#' \code{\link{listOMLRunEvaluations}} on success.
#'
#' @param run [\code{\link{OMLRun}}|\code{\link{runTaskMlr}}]\cr
#'   The run that should be uploaded. Either a \code{\link{OMLRun}} or a run created with \code{\link{runTaskMlr}}.
#' @template arg_upload_tags
#' @template arg_verbosity
#' @param ...
#'   Not used.
#' @return [\code{invisible(numeric(1))}].
#'   The run ID.
#' @family uploading functions
#' @family run-related functions
#' @export
uploadOMLRun = function(run, tags = NULL, verbosity = NULL, ...) {
  UseMethod("uploadOMLRun")
}

#' @export
uploadOMLRun.runTaskMlr = function(run, tags = NULL, verbosity = NULL, ...) {
  assertClass(run, "runTaskMlr")
  assertClass(run$bmr, "BenchmarkResult")
  assertClass(run$flow, "OMLFlow")
  uploadOMLRun.OMLRun(run = run$run, bmr = run$bmr, flow = run$flow)
}

#' @export
uploadOMLRun.OMLRun = function(run, tags = NULL, verbosity = NULL, ...) {
  assertClass(run, "OMLRun")
  bmr = list(...)$bmr
  flow = list(...)$flow
  
  if (!checkUserConfirmation(type = "run")) {
    return(invisible())
  }

  # if no flow.id, try to upload flow (if it exists) and assign its id to flow.id slot
  if (is.na(run$flow.id)) {
    if (!is.null(flow)){
      run$flow.id = uploadOMLFlow(flow) 
      #flow.ids = setNames(flow.ids, rev(unlist(strsplit(flow$name, "[.]")))[1:length(flow.ids)])
    } else stop("Please provide a 'flow'")
  } # else flow.ids = run$flow.id
  
  if (is.na(run$error.message)) {
    assertDataFrame(run$predictions)
  } else {
    assertString(run$error.message)
  }
  
  # modify parameter.setting and add component flow.id
  #parameter.setting = unclass(getOMLRunParList(run))
  #seed.setting = unclass(getOMLSeedParList(run))
  parameter.setting = run$parameter.setting
  #flow.ids[1] = NA_character_
  if (length(parameter.setting) > 0) {
    for(i in 1:length(parameter.setting)) {
      ind = parameter.setting[[i]]$component
      parameter.setting[[i]]$component = NA_character_ #flow.ids[ind]
    }
  }
  run$parameter.setting = parameter.setting #append(parameter.setting, seed.setting)
  
  description = tempfile(fileext = ".xml")
  on.exit(unlink(description))
  writeOMLRunXML(run, description, bmr = bmr)

  if (!is.null(run$predictions)) {
    output = tempfile(fileext = ".arff")
    on.exit(unlink(output), add = TRUE)
    arff.writer(run$predictions, file = output)
    
    content = doAPICall(api.call = "run", method = "POST", file = NULL, verbosity = verbosity,
      post.args = list(description = upload_file(path = description),
                       predictions = upload_file(path = output)))
  } else {
    content = doAPICall(api.call = "run", method = "POST", file = NULL, verbosity = verbosity,
      post.args = list(description = upload_file(path = description)) )
  }
  # was uploading successful?
  doc = parseXMLResponse(content, "Uploading run", as.text = TRUE)
  run.id = xmlRValI(doc, "/oml:upload_run/oml:run_id")
  # else, return the run.id invisibly
  showInfo(verbosity, "Run successfully uploaded. Run ID: %i", run.id)
  if (!is.null(tags)) tagOMLObject(run.id, object = "run", tags = tags)
  forget(listOMLRuns)
  forget(listOMLRunEvaluations)

  return(invisible(run.id))
}
