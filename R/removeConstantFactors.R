removeConstantFactors <- function(df, perc = 0) {
  factor.inds <- which(unlist(lapply(df, is.factor)))
  remove.inds <- c()
  for(i in factor.inds) {
    if(nlevels(df[, i]) == 1) {
      remove.inds <- c(remove.inds, i)
    } else {
      if (sum(sort(table(df[, i]), decreasing = TRUE)[-1])/length(df[, i]) < perc/100) {
        remove.inds <- c(remove.inds, i)
      }
    }  
  }
  if(length(remove.inds) > 0) {
    df <- df[-remove.inds]
  }
  return(df)
}