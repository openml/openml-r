#' OMLRun
#'
#' @title Construct OMLRun.
#'
#' @param run.id [\code{numeric(1)}]\cr ID of the run. Added by server. Ignored when uploading a
#'   run.
#' @param uploader [\code{numeric(1)}]\cr ID of the user that uploaded the run. Added by server.
#'   Ignored when uploading a run.
#' @param task.id [\code{numeric(1)}]\cr ID of the task that is solved in this run. This ID is given
#'   in the task description.
#' @param implementation.id [\code{character(1)}]\cr ID of the implementation used to solve the
#'   task. Returned by the API when you upload the implementation, or given in the implementation
#'   description when you download an existing implementation.
#' @param setup.id [\code{numeric(1)}]\cr Unique ID of the used setup. Ignored when uploading a run
#'   (i.e., it will be searched based on the parameter settings).
#' @param setup.string [\code{character(1)}]\cr The CLI string that can invoke the learner with the
#'   correct parameter settings. Optional.
#' @param error.message [\code{character(1)}]\cr Whenever an error occurs during the run, this can
#'   be reported here.
#' @param parameter.setting [\code{list}]\cr A list of \code{\link{OMLRunParameter}s} containing
#'   information on the parameter settings.
#' @param tags [\code{character}]\cr Optional tags describing the run.
#' @param predictions [\code{data.frame}]\cr The predictions of the run. These are NOT downloaded
#'   by \code{\link{getOMLRun}}. To retrieve predictions of an uploaded run, please use
#'   \code{\link{getOMLPredictions}}.
#' @param input.data [\code{\link{OMLIOData}}]\cr All data that served as input for the run. Added
#'   by server. Ignored when uploading.
#' @param output.data [\code{\link{OMLIOData}}]\cr All data that was the output of this run, i.e.,
#'   predictions, evaluation scores. Most of this will be added by the server, but users can also
#'   provide evaluation scores for their own evaluation measures.
#' @export
#' @aliases OMLRun
#' @seealso \code{\link{getOMLRun}}, \code{\link{listOMLRunResults}}
makeOMLRun = function(run.id, uploader, task.id, implementation.id, setup.id,
  setup.string = NA_character_, error.message = NA_character_, parameter.setting = list(),
  tags = NA_character_, predictions = NULL, input.data = makeOMLIOData(), output.data = makeOMLIOData()) {

  run.id = asCount(run.id)
  uploader = asCount(uploader)
  task.id = asCount(task.id)
  assertString(implementation.id)
  setup.id = asCount(setup.id)
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
      task.id = task.id,
      implementation.id = implementation.id,
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
    if (!is.na(val))
      catf("%s %s", s, val)
  }

  ## General info
  catf('\nRun Results :: (Run ID = %i, Task ID = %i)', x$run.id, x$task.id)
  catNotNA('\tFlow ID: ', x$implementation.id)
  catNotNA('\tUser ID: ', x$uploader)

  if (printMetrics) {
    cat('\n\tMetrics:\n\n')
    m = x$output.data$evaluation
    print(m[, colnames(m) != "array.data"])
  }
}
