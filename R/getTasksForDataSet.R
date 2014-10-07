#' Get task IDs for a certain data set
#'
#' @param id [\code{integer}]\cr
#'   One or more data set IDs.
#' @return [\code{integer}]. A vector containing the task IDs.
#' @export

getTasksForDataSet = function(did) {
  assertIntegerish(did)
  did = paste0("(", collapse(paste0('did=', did), sep = ' or '), ")") 
  query = paste0("SELECT DISTINCT t.task_id FROM task_type_inout ttio, ",
    "task_inputs ti, task t, dataset d WHERE ttio.type='Dataset' and ttio.name = ti.input and ", 
    did, "and ti.value=d.did and ti.task_id=t.task_id")
  return(runSQLQuery(query))
}