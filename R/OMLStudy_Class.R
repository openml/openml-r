
#' @title OMLStudy.
#'
#' @description
#' If you create a study through the website \url{https://www.openml.org/new/study}, you can also specify an alias which can be used to access the study.
#' To see a full list of all elements, please see the
#' \href{https://www.openml.org/api/v1/xsd/openml.study.upload}{XSD}.
#'
#' @param alias [\code{character}]\cr
#'   The alias of the study.
#' @param main.entity.type [\code{character}]\cr
#'   Whether it is a collection of runs (study) or collection of tasks (benchmark suite).
#'   Possible values are  \code{{"task", "run"}}.
#' @param name [\code{character}]\cr
#'   The name of the study.
#' @param description [\code{character}]\cr
#'   The description of the study.
#' @param data.id [\code{integer}]\cr
#'   A vector of IDs of the data sets to be included in the study.
#' @param task.id [\code{integer}]\cr
#'   A vector of IDs of the tasks to be included in the study.
#' @param flow.id [\code{integer}]\cr
#'   A vector of IDs of the flows to be included in the study.
#' @param run.id [\code{integer}]\cr
#'   A vector of IDs of the runs to be included in the study.
#' @return [\code{OMLStudy}].
#' @family uploading functions
#' @export
makeOMLStudy = function(alias, main.entity.type, name, description,
  data.id = NULL, task.id = NULL, flow.id = NULL, run.id = NULL) {
  assertString(alias)
  assertChoice(main.entity.type, choices = c("task", "run"), null.ok = TRUE)
  assertString(name)
  assertString(description)
  assertIntegerish(data.id, null.ok = TRUE)
  assertIntegerish(task.id, null.ok = TRUE)
  assertIntegerish(flow.id, null.ok = TRUE)
  assertIntegerish(run.id, null.ok = TRUE)

  makeS3Obj("OMLStudy",
    alias = alias,
    main.entity.type = main.entity.type,
    name = name,
    description = description,
    data = list(data.id = data.id),
    tasks = list(task.id = task.id),
    flows = list(flow.id = flow.id),
    runs = list(run.id = run.id)
  )
}

#' @export
print.OMLStudy = function(x, ...) {
  catf("\n Study '%s' (Study ID %i)", x$name, x$id)
  catf("  Description           : %s", BBmisc::clipString(x$description, 80))
  catf("  Creation Date         : %s", x$creation.date)
  if (!is.null(x$tag))
    catf("  Tag(s)                : %s", stri_paste(x$tag$name, collapse = ","))
  catf("  Number of Data Sets   : %s", length(x$data$data.id))
  catf("  Number of Tasks       : %s", length(x$tasks$task.id))
  catf("  Number of Flows       : %s", length(x$flows$flow.id))
  # catf("  Number of Setups      : %s", length(x$setups$setup.id))
  catf("  Number of Runs        : %s", length(x$runs$run.id))
  # catf("  Data IDs         : %s", BBmisc::clipString(BBmisc::collapse(x$data$data.id), 80))
  # catf("  Task IDs         : %s", BBmisc::clipString(BBmisc::collapse(x$tasks$task.id), 80))
  # catf("  Flow IDs         : %s", BBmisc::clipString(BBmisc::collapse(x$flows$flow.id), 80))
  # catf("  Setup IDs        : %s", BBmisc::clipString(BBmisc::collapse(x$setups$setup.id), 80))
  # catf("  Run IDs          : %s", BBmisc::clipString(BBmisc::collapse(x$runs$run.id), 80))
}
