#' OMLRun
#'
#' @title Construct OMLRun.
#'
#' @description 
#' More details about the elements of a \code{OMLRun} can be found in the
#' \href{https://github.com/openml/website/blob/master/openml_OS/views/pages/api_new/v1/xsd/openml.run.upload.xsd}{XSD scheme}.
#'
#' @param run.id [\code{numeric(1)}]\cr
#'   ID of the run. Added by server. Ignored when uploading a run.
#' @param uploader [\code{numeric(1)}]\cr
#'   ID of the user that uploaded the run. Added by server. Ignored when uploading a run.
#' @param uploader.name [\code{character(1)}]\cr
#'   Name of the user that uploaded the run. Ignored when uploading a run.
#' @param task.id [\code{numeric(1)}]\cr
#'   ID of the task that is solved in this run. This ID is given in the task description.
#' @param task.type [\code{character(1)}]\cr
#'   Task type of the run. See \code{\link{listOMLTaskTypes}} for all possible types.
#' @param task.evaluation.measure [\code{character(1)}]\cr
#'   Evaluation measure used in the run.
#' @param flow.id [\code{character(1)}]\cr
#'   ID of the flow used to solve the task. Returned by the API when you upload the
#'   flow, or given in the flow description when you download an existing flow.
#' @param flow.name [\code{character(1)}]\cr
#'   Name of the flow.
#' @param setup.id [\code{numeric(1)}]\cr
#'   Unique ID of the used setup. Ignored when uploading a run (i.e., it will be
#'   searched based on the parameter settings).
#' @param setup.string [\code{character(1)}]\cr
#'   The CLI string that can invoke the learner with the correct parameter settings.
#'   This argument is optional.
#' @param error.message [\code{character(1)}]\cr
#'   Whenever an error occurs during the run, this can be reported here.
#' @param parameter.setting [\code{list}]\cr
#'   A list of \code{OMLRunParameter}s containing information on the parameter
#'   settings.
#' @param tags [\code{character}]\cr
#'   Optional tags describing the run.
#' @param predictions [\code{data.frame}]\cr
#'   The predictions of the run. 
#' @param input.data [\code{\link{OMLIOData}}]\cr
#'   All data that served as input for the run. Added by server. Ignored when uploading.
#' @param output.data [\code{\link{OMLIOData}}]\cr
#'   All data that was the output of this run, i.e.,
#'   predictions, evaluation scores. Most of this will be added by the server, but users can also
#'   provide evaluation scores for their own evaluation measures.
#' @export
#' @aliases OMLRun
#' @family run related functions
makeOMLRun = function(run.id = NA_integer_, uploader = NA_integer_, uploader.name = NA_character_,
  task.id, task.type = NA_character_,
  task.evaluation.measure = NA_character_, flow.id = NA_integer_, flow.name = NA_character_,
  setup.id = NA_integer_, setup.string = NA_character_, error.message = NA_character_,
  parameter.setting = list(), tags = NA_character_, predictions = NULL, input.data = makeOMLIOData(),
  output.data = makeOMLIOData()) {

  run.id = asCount(run.id, na.ok = TRUE)
  uploader = asCount(uploader, na.ok = TRUE)
  uploader.name = as.character(uploader.name)
  task.id = asCount(task.id)
  task.type = as.character(task.type)
  task.evaluation.measure = as.character(task.evaluation.measure)
  flow.id = asCount(flow.id, na.ok = TRUE)
  flow.name = as.character(flow.name)
  setup.id = asCount(setup.id, na.ok = TRUE)
  assertString(setup.string, na.ok = TRUE)
  assertString(error.message, na.ok = TRUE)
  assertList(parameter.setting)
  assertCharacter(tags, all.missing = TRUE)
  if (!is.null(predictions))
    assertDataFrame(predictions)
  assertClass(input.data, "OMLIOData")
  assertClass(output.data, "OMLIOData")

  makeS3Obj("OMLRun",
      run.id = run.id,
      uploader = uploader,
      uploader.name = uploader.name,
      task.id = task.id,
      task.type = task.type,
      task.evaluation.measure = task.evaluation.measure,
      flow.id = flow.id,
      flow.name = flow.name,
      setup.id = setup.id,
      setup.string = setup.string,
      error.message = error.message,
      parameter.setting = parameter.setting,
      tags = tags,
      predictions = predictions,
      input.data = input.data,
      output.data = output.data
  )
}

# ***** Methods *****

# show
#' @export
print.OMLRun = function(x, printMetrics = FALSE, ...)  {
  catNotNA = function(s, val) {
    if (!all(is.na(val)))
      catf("%s %s", s, collapse(val, sep = ", "))
  }

  ## General info
  catf('\nOpenML Run %i :: (Task ID = %i, Flow ID = %i)', x$run.id, x$task.id, x$flow.id)
  catNotNA('\tUser ID:', x$uploader)
  catNotNA('\tTags   :', x$tags)

  if (!is.null(x$mlr.benchmark.result$results[[1]][[1]])) {
    cat('\n')
    print(x$mlr.benchmark.result$results[[1]][[1]])
  }

  if (printMetrics) {
    cat('\n\tMetrics:\n\n')
    m = x$output.data$evaluation
    print(m[, colnames(m) != "array.data"])
  }
}
