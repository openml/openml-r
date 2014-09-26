#' Get task IDs for a certain data set
#'
#' @param name [\code{character(1)}]\cr
#'   The name of the data set.
#' @return [\code{integer}]. A vector containing the task IDs.
#' @export

getTasksForDataSet = function(name) {
  assertString(name)
  query = paste0("SELECT DISTINCT t.task_id FROM task_type_inout ttio, ",
    "task_inputs ti, task t, dataset d WHERE ttio.type='Dataset' and ttio.name = ti.input and d.name='", 
    name, "' and ti.value=d.did and ti.task_id=t.task_id")
  return(runSQLQuery(query))
}