chunkOMLlist = function(listfun, ..., limit = 100000, server.limit = 10000) {
  offset = seq(1, limit, by = server.limit)
  res = vector("list", length(offset))
  for (i in seq_along(offset)) {
    tmp = try(listfun(..., offset = offset[i], limit = server.limit))
    if (!(is.error(tmp) || nrow(tmp) == 0)) {
      res[[i]] = tmp
      if (i == length(offset))
        return(rbindlist(filterNull(res), fill = TRUE))
    } else {
      return(rbindlist(filterNull(res), fill = TRUE))
    }
  }
}
