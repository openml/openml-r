#' @title Upload an OpenML run.
#'
#' @description
#' Share a run of a flow on a given OpenML task by uploading it to the OpenML server.
#'
#' @note
#' This function will reset the cache of \code{\link{listOMLRuns}} and
#' \code{\link{listOMLRunEvaluations}} on success.
#'
#' By default you will be asked to confirm the upload. You can deactivate the
#' need for confirmation by setting \dQuote{confirm.upload = TRUE} via
#' \link{setOMLConfig} or set the corresponding argument each time you call
#' the function.
#'
#' @param run [\code{\link{OMLRun}}|\code{\link{OMLMlrRun}}]\cr
#'   The run that should be uploaded. Either a \code{\link{OMLRun}} or a run created with \code{\link{OMLMlrRun}}.
#' @param upload.bmr [\code{logical(1)}]\cr
#'   Should the Benchmark result created by \code{\link[mlr]{benchmark}} function be uploaded?
#'   If set to \code{TRUE} and the flow is created via \link[mlr]{makeTuneWrapper}, an arff file that contains the hyperparameter optimization trace is also uploaded.
#' @template arg_upload_tags
#' @template arg_confirm.upload
#' @template arg_verbosity
#' @param ...
#'   Not used.
#' @return [\code{invisible(numeric(1))}].
#'   The run ID.
#' @family uploading functions
#' @family run-related functions
#' @export
uploadOMLRun = function(run, upload.bmr = FALSE, tags = NULL, confirm.upload = NULL, verbosity = NULL, ...) {
  UseMethod("uploadOMLRun")
}

# For reverse support
#' @export
uploadOMLRun.runTaskMlr = function(run, upload.bmr = FALSE, tags = NULL, confirm.upload = NULL, verbosity = NULL, ...) {
  class(run) = "OMLMlrRun"
  uploadOMLRun(run = run, upload.bmr = upload.bmr, tags = tags, confirm.upload = NULL, verbosity = verbosity, ...)
}

#' @export
uploadOMLRun.OMLMlrRun = function(run, upload.bmr = FALSE, tags = NULL, confirm.upload = NULL, verbosity = NULL, ...) {
  assertClass(run, "OMLMlrRun")
  assertClass(run$bmr, "BenchmarkResult")
  assertClass(run$flow, "OMLFlow")
  assertFlag(upload.bmr)
  uploadOMLRun.OMLRun(run = run$run, upload.bmr = upload.bmr, bmr = run$bmr, flow = run$flow)
}

#' @export
uploadOMLRun.OMLRun = function(run, upload.bmr = FALSE, tags = NULL, confirm.upload = NULL, verbosity = NULL, ...) {
  assertClass(run, "OMLRun")
  assertFlag(upload.bmr)

  bmr = list(...)$bmr
  flow = list(...)$flow

  if (!checkUserConfirmation(type = "run", confirm.upload = confirm.upload)) {
    return(invisible())
  }

  # if no flow.id, try to upload flow (if it exists) and assign its id to flow.id slot
  if (is.na(run$flow.id)) {
    if (!is.null(flow)){
      run$flow.id = uploadOMLFlow(flow, tags = tags, verbosity = verbosity)
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

  description = tempfile(pattern = "description", fileext = ".xml")
  on.exit(unlink(description))
  writeOMLRunXML(run, description, bmr = bmr)
  post.args = list(description = upload_file(path = description))

  if (!is.null(run$predictions)) {
    predictions.file = tempfile(pattern = "predictions", fileext = ".arff")
    on.exit(unlink(predictions.file), add = TRUE)
    arff.writer(run$predictions, file = predictions.file)
    post.args$predictions = upload_file(path = predictions.file)
  }

  if (!is.null(bmr)) {
    # FIXME: See https://github.com/openml/OpenML/issues/276 do we always want to upload this? Or only for TuneWrapper?
    if (grepl("[.]tuned", flow$name)) {
      trace.file = tempfile(pattern = "optimization_trace", fileext = ".arff")
      on.exit(unlink(trace.file), add = TRUE)
      arff.writer(getBMRTuneTrace(bmr), file = trace.file)
      post.args$Trace = upload_file(path = trace.file)
    }
    if (upload.bmr) {
      bmr.file = tempfile(pattern = "bmr", fileext = ".rds")
      on.exit(unlink(bmr.file), add = TRUE)
      saveRDS(bmr, file = bmr.file)
      post.args$BenchmarkResult = upload_file(path = bmr.file)
    }
  }

  content = doAPICall(api.call = "run", method = "POST", file = NULL, verbosity = verbosity,
    post.args = post.args)

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



# helpers for tune trace
getResampInfo = function(resample.desc) {
  assertClass(resample.desc, "ResampleDesc")
  reps = ifelse(!is.null(resample.desc$reps), resample.desc$reps, 1)
  folds = ifelse(!is.null(resample.desc$folds), resample.desc$folds, resample.desc$iters)
  return(list(reps = reps, folds = folds))
}

getBMRTuneTrace = function(bmr) {
  res = bmr$results[[1]][[1]]
  tune.res = mlr::getNestedTuneResultsOptPathDf(res)
  tune.x = mlr::getNestedTuneResultsX(res)
  tune.par = colnames(tune.x)
  resample.info = getResampInfo(res$pred$instance$desc)
  evaluation = names(mlr::getBMRTuneResults(bmr)[[1]][[1]][[1]]$y)

  cv.iter = tune.res$iter
  folds = resample.info$folds
  reps = resample.info$reps
  rep = rep(seq_len(reps), each = length(cv.iter)/reps)
  fold = cv.iter %% folds
  fold[fold == 0L] = folds

  # Note: The columns rep, fold and row_id must be 0-based to be accepted by the server.
  tune.trace = data.frame(
    rep = rep - 1L,
    fold = fold - 1L,
    iteration = as.numeric(tune.res$dob) - 1L,
    tune.res[,tune.par, drop = FALSE],
    evaluation = tune.res[evaluation]
  )

  par = apply(tune.res[,tune.par, drop = FALSE], 1, function(x) collapse(x))
  par = chunk(par, chunk.size = length(unique(tune.res$dob)))
  tune.x.vec = apply(tune.x, 1, function(x) collapse(x))
  tune.trace$selected = unlist(lapply(seq_along(par), function(i) par[[i]]%in%tune.x.vec[i]))

  # tune.x2 = data.frame(unique(tune.trace[,c("rep", "fold")]), tune.x)
  # tune.trace = plyr::ddply(tune.trace, .variables = c("rep", "fold"), function(d) {
  #   dat = tune.x2[tune.x2$rep == unique(d$rep) & tune.x2$fold == unique(d$fold),, drop = FALSE]
  #   dat = subset(dat, select = -c(rep, fold))
  #   d$select = (apply(d[, colnames(dat), drop = FALSE], 1, collapse))%in%collapse(dat)
  #   return(d)
  # })

  tune.trace = tune.trace[order(tune.trace$rep, tune.trace$fold, tune.trace$iteration),]
  return(tune.trace)
}
