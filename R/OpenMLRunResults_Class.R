#' @title Construct OpenMLRunResults.
#' 
#' @param run.id [\code{numeric(1)}]\cr
#'       ID of the run. Added by server. Ignored when uploading a run.
#' @param uploader [\code{numeric(1)}]\cr   
#'       ID of the user that uploaded the run. Added by server. Ignored when uploading a run.
#' @param task.id [\code{numeric(1)}]\cr
#'       ID of the task that is solved in this run. This ID is given in the task description.
#' @param implementation.id [\code{character(1)}]\cr
#'       ID of the implementation used to solve the task. Returned by the API when you first upload the 
#'       implementation, or given in the implementation description when you download an existing 
#'       implementation.
#' @param setup.id [\code{numeric(1)}]\cr
#'       Unique ID of the used setup. Ignored when uploading a run (i.e., it will be searched based 
#'       on the parameter settings).
#' @param error.message [\code{character(1)}]\cr
#'       Whenever an error occurs during the run, this can be reported here.
#' @param parameter.setting [\code{list}]\cr
#'       A list of \code{\link{OpenMLRunParameter}s} containing information on the parameter settings.
#' @param input.data [\code{\link{OpenMLData}}]\cr
#'       All data that served as input for the run. Added by server. Ignored when uploading.
#' @param output.data [\code{\link{OpenMLData}}]\cr
#'       All data that was the output of this run, i.e., predictions, evaluation scores. 
#'       Most of this will be added by the server, but users can also provide evaluation scores for their 
#'       own evaluation measures.
#' @export 
#' @aliases OpenMLRunResults

# FIXME: add setup.string
makeOpenMLRunResults = function(run.id, uploader, task.id, implementation.id, setup.id,
  error.message = NA_character_, parameter.setting = list(), input.data = OpenMLData(),
  output.data = OpenMLData()
) {
  
  assertInt(run.id)
  assertInt(uploader)
  assertInt(task.id)
  assertString(implementation.id)
  assertInt(setup.id)
  assertString(error.message, na.ok = TRUE)
  assertList(parameter.setting)
  assertClass(input.data, "OpenMLData")
  assertClass(output.data, "OpenMLData")
  
  makeS3Obj("OpenMLRunResults",
      run.id = run.id,
      uploader = uploader,
      task.id = task.id,
      implementation.id = implementation.id,
      setup.id = setup.id,
      error.message = error.message,
      parameter.setting = parameter.setting,
      input.data = input.data,
      output.data = output.data
  )
}

# ***** Methods *****

# show
#' @export
print.OpenMLRunResults = function(x, ...)  {
  catNotEmpty = function(s, val) {
    if (val != "") 
      catf("%s %s", s, val)
  }
  
  ## General info
  catf('\n** Run Information **')
  
  catNotEmpty('Run ID            :: ', x$run.id)
  catNotEmpty('Task ID           :: ', x$task.id)
  catNotEmpty('User ID           :: ', x$uploader)
  catNotEmpty('Implementation ID :: ', x$implementation.id)
  
  ## FIXME: Add parameters.
  
  ## Metrics
  catf('\n** Metrics **')
  
  #if(length(x$metrics) > 0) {
  #  for(i in seq_along(x$metrics)) {
  #    catNotEmpty('Name              :: ', x$metrics[[i]]$name)
  #    catNotEmpty('Value             :: ', x$metrics[[i]]$value)
  #    catNotEmpty('Label             :: ', x$metrics[[i]]$label)
  #    cat("\n")
  #  }
  #}
  print(x$output.data)
}

