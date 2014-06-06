# FIXME @method show \code{signature(object = "OpenMLTask")}: method used to show the contents of a OpenMLTask object. 

#' OpenMLTask
#'
#' This class of objects contains the information describing an OpenML task.
#' 
#' Objects can be created by calls of the form \code{OpenMLTask(...)}.
#' The objects contain information on ... .
#'
#' @section Slots: 
#'  \describe{
#'    \item{\code{task.id}}{[\code{integer(1)}]\cr
#'    The task's OpenML ID.}
#'    \item{\code{task.type}}{[\code{character}]\cr 
#'    The task's type.}
#'    \item{\code{task.pars}}{[\code{list}]\cr 
#'    A set of parameters specific to the task type.}
#'    \item{\code{task.target.features}}{[\code{character}]\cr 
#'    The name(s) of the target feature(s)}
#'    \item{\code{task.data.desc.id}}{[\code{integer(1)}]\cr 
#'    The OpenML ID of the data set associated with the task.}
#'    \item{\code{task.data.desc}}{[\code{\link{OptionalOpenMLDataSetDescription}}]\cr 
#'    Information on the data set.}
#'    \item{\code{task.estimation.procedure}}{[\code{\link{OptionalOpenMLEstimationProcedure}}]\cr 
#'    Information on the task's estimation method and the asoociated data splits.}
#'    \item{\code{task.preds}}{[\code{list}]\cr 
#'    A list that contains information on the format of the predictions for the particular task.}
#'    \item{\code{task.evaluation.measures}}{[\code{character}]\cr 
#'    The evaluation measure(s) that should be used for optimization.}
#'  }
#'
#' @seealso \code{\linkS4class{OpenMLDataSetDescription}}
#' @examples
#' showClass("OpenMLTask")
#' @name OpenMLTask
#' @rdname OpenMLTask
#' @aliases OpenMLTask-class
#' @exportClass OpenMLTask

setClass("OpenMLTask", representation(
  task.id = "integer",
  task.type = "character",
  task.pars = "list",
  task.target.features = "character",
  task.data.desc.id = "integer",
  task.data.desc = "OptionalOpenMLDataSetDescription",
  task.estimation.procedure = "OptionalOpenMLEstimationProcedure",
  task.preds = "list",
  task.evaluation.measures = "character"
))


# ***** Constructor *****
OpenMLTask = function(task.id, task.type, task.pars, task.target.features,
                       task.data.desc.id, task.data.desc,
                       task.estimation.procedure,
                       task.preds, task.evaluation.measures) {
  makeS3Obj("OpenMLTask",
      task.id = task.id, task.type = task.type, task.pars = task.pars,
      task.target.features = task.target.features,
      task.data.desc.id = task.data.desc.id, task.data.desc = task.data.desc,
      task.estimation.procedure = task.estimation.procedure,
      task.preds = task.preds, task.evaluation.measures = task.evaluation.measures
  )
}


# ***** Methods *****

# show
# Note: The data splits and the predictions are not shown
#' @export
print.OpenMLTask = function(x, ...) {
  ## Task general info
  catf('\nTask ID ::  %i \n\nTask Type ::  %s', x$task.id, x$task.type)
  if (length(x$task.pars)) 
    cat(collapse(paste("\t", names(x$task.pars), " = ", x$task.pars), sep = "\n"))
  
  ## Target variables info
  catf('\nTask Target Feature :: %s', collapse(x$task.target.features, "\t"))
  
  ## Data set info
<<<<<<< HEAD
  if (!is.null(x$task.data.desc)) {
    catf('\nDataset ::  %s  (openML ID =  %i, version = %s)', 
         x$task.data.desc$name, x$task.data.desc$id, x$task.data.desc$version)
=======
  if (!is.null(object$task.data.desc)) {
    catf('\nDataset ::  %s  (OpenML ID =  %i, version = %s)', 
         object$task.data.desc$name, object$task.data.desc$id, object$task.data.desc$version)
>>>>>>> e8b0b6241ec03f17913429863d4bfdfe85807fab
    catf('\tData frame with %i rows and %i columns', 
         nrow(x$task.data.desc$data.set), ncol(x$task.data.desc$data.set))
  }
  
  ## Estimation procedure info
  if (!is.null(x$task.estimation.procedure)) {
    catf('\nEstimation Procedure :: %s', x$task.estimation.procedure$type)
    catf('\tData splits for estimation %s.',
         ifelse(all(dim(x$task.estimation.procedure$data.splits) == 0), 'not available', 'available'))
    if (length(x$task.estimation.procedure$parameters)) {
      
      cat('\tParameters of the estimation procedure:\n')
      
      cat(collapse(paste("\t", names(x$task.estimation.procedure$parameters), " = ", 
                         x$task.estimation.procedure$parameters), sep = "\n"))
    }
  }
  cat('\nPredictions ::\n')
  catf('\tFormat = %s', x$task.preds$format)
  cat('\tColumns:\n')
  cat(collapse(paste("\t\t", names(x$task.preds$features), " = ", x$task.preds$features), sep = "\n"))
  cat('\nEvaluation Measures ::\n')
  catf('%s', collapse(x$task.evaluation.measures, '\n'))
}

