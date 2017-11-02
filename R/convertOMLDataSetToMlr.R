#' @title Convert an OpenML data set to mlr task.
#'
#' @description
#' Converts an \code{\link{OMLDataSet}} to a \code{\link[mlr]{Task}}.
#'
#' @param obj [\code{\link{OMLDataSet}}]\cr
#'   The object that should be converted.
#' @param mlr.task.id [\code{character(1)}]\cr
#'   Id string for \code{\link[mlr]{Task}} object.
#'   The strings \code{<oml.data.name>}, \code{<oml.data.id>} and \code{<oml.data.version>}
#'   will be replaced by their respective values contained in the \code{\link{OMLDataSet}} object.
#'   Default is \code{<oml.data.name>}.
#' @param task.type [\code{character(1)}]\cr
#'   As we only pass the data set, we need to define the task type manually.
#'   Possible are: \dQuote{Supervised Classification}, \dQuote{Supervised Regression},
#'   \dQuote{Survival Analysis}.
#'   Default is \code{NULL} which means to guess it from the target column in the
#'   data set. If that is a factor or a logical, we choose classification.
#'   If it is numeric we choose regression. In all other cases an error is thrown.
#' @param target [\code{character}]\cr
#'   The target for the classification/regression task.
#'   Default is the \code{default.target.attribute} of the \code{\link{OMLDataSetDescription}}.
#' @param ignore.flagged.attributes [\code{logical(1)}]\cr
#'   Should those features that are listed in the data set description slot \dQuote{ignore.attribute}
#'   be removed?
#'   Default is \code{TRUE}.
#' @param drop.levels [\code{logical(1)}]\cr
#'   Should empty factor levels be dropped in the data?
#'   Default is \code{TRUE}.
#' @param fix.colnames [\code{logical(1)}]\cr
#'   Should colnames of the data be fixed using \code{\link[base]{make.names}}?
#'   Default is \code{TRUE}.
#' @template arg_verbosity
#' @return [\code{\link[mlr]{Task}}].
#' @family data set-related functions
#' @example /inst/examples/convertOMLDataSetToMlr.R
#' @export
convertOMLDataSetToMlr = function(
  obj,
  mlr.task.id = "<oml.data.name>",
  task.type = NULL,
  target = obj$desc$default.target.attribute,
  ignore.flagged.attributes = TRUE,
  drop.levels = TRUE,
  fix.colnames = TRUE,
  verbosity = NULL) {

  assertClass(obj, "OMLDataSet")
  assertSubset(target, obj$colnames.new)
  assertFlag(ignore.flagged.attributes)
  assertFlag(drop.levels)

  data = obj$data
  desc = obj$desc

  # no task type? guess it by looking at target
  if (is.null(task.type))
    task.type = guessTaskType(data[, target])
  assertChoice(task.type, getValidTaskTypes())

  #  remove ignored attributes from data
  if (!is.na(desc$ignore.attribute) && ignore.flagged.attributes) {
    inds = which(obj$colnames.old %in% desc$ignore.attribute)
    data = data[, -inds]
  }

  # drop levels
  if (drop.levels)
    data = droplevels(data)

  # fix colnames using make.names
  if (fix.colnames) {
    colnames(data) = make.names(colnames(data), unique = TRUE)
    target = make.names(target, unique = TRUE)
  }

  # get fixup verbose setting for mlr
  if (is.null(verbosity))
    verbosity = getOMLConfig()$verbosity
  fixup = ifelse(verbosity == 0L, "quiet", "warn")

  mlr.task = switch(task.type,
    "Supervised Classification" = mlr::makeClassifTask(data = data, target = target, fixup.data = fixup),
    "Supervised Regression" = mlr::makeRegrTask(data = data, target = target, fixup.data = fixup),
    "Survival Analysis" = mlr::makeSurvTask(data = data, target = target, fixup.data = fixup),
    "Multilabel" = mlr::makeMultilabelTask(data = data, target = target, fixup.data = fixup),
    stopf("Encountered currently unsupported task type: %s", task.type)
  )

  if (!is.null(mlr.task.id))
    mlr.task$task.desc$id = replaceOMLDataSetString(mlr.task.id, obj)

  #  remove constant featues
  mlr.task = mlr::removeConstantFeatures(mlr.task)
  return(mlr.task)
}

replaceOMLDataSetString = function(string, data.set) {
  string = stri_replace_all_fixed(string, "<oml.data.id>", data.set$desc$id)
  string = stri_replace_all_fixed(string, "<oml.data.name>", data.set$desc$name)
  stri_replace_all_fixed(string, "<oml.data.version>", data.set$desc$version)
}

# @title Helper to guess task type from target column format.
#
# @param target [vector]
#   Vector of target values.
# @return [character(1)]
guessTaskType = function(target) {
  if (inherits(target, "data.frame")) {
    assertDataFrame(target, types = "logical")
    return("Multilabel")
  } else {
    if (is.factor(target) | is.logical(target))
      return("Supervised Classification")
    if (is.numeric(target))
      return("Supervised Regression")
  }

  stopf("Cannot guess task.type from data!")
}

getValidTaskTypes = function() {
  c("Supervised Classification", "Supervised Regression", "Survival Analysis", "Multilabel")
}
