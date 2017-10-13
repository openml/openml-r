chunkOMLlist = function(listfun, ..., total.limit = 100000, chunk.limit = 1000) {
  assertSubset(as.character(substitute(listfun)), 
    choices = c("listOMLFlows", "listOMLRuns", "listOMLRunEvaluations",
      "listOMLDataSets", "listOMLTasks", "listOMLSetup"))
  
  offset = seq(0, total.limit - 1, by = chunk.limit)
  res = vector("list", length(offset))
  for (i in seq_along(offset)) {
    tmp = try(listfun(..., offset = offset[i], limit = chunk.limit))
    if (!(is.error(tmp) || nrow(tmp) == 0)) {
      res[[i]] = tmp
      if (i == length(offset))
        return(as.data.frame(rbindlist(filterNull(res), fill = TRUE), stringsAsFactors = FALSE))
    } else {
      return(as.data.frame(rbindlist(filterNull(res), fill = TRUE), stringsAsFactors = FALSE))
    }
  }
}
