library(BatchExperiments)
library(mlr)
library(stringr)
devtools::load_all("openML")

reg <- makeExperimentRegistry(id="openML_with_mlr", packages=c("mlr", "RCurl", "XML", "stringr"))

all.res <- reduceResults(reg, 
  fun = function(aggr, job, res) rbind(aggr, c(job$algo.pars, job$prob.pars, res)), init = c())

sep.res <- list()
for(i in seq_along(all.lrners)) {
  inds <- which(unlist(all.res[, 1]) == all.lrners[i])
  sep.res[[i]] <- unlist(all.res[inds, 3])
}

# get the error messages
error.inds <- which(is.na(as.numeric(all.res[, 3])))
error.msges <- unlist(all.res[error.inds, 3])

multiclass <- which(!is.na(str_match(error.msges, "multiclass-problem")))  # ok.
factor.inputs <- which(!is.na(str_match(error.msges, "factor inputs")))  # ok. (only fnn)
missing.values <- which(!is.na(str_match(error.msges, "missing values")))  # ok.
factor.mismatch <- which(!is.na(str_match(error.msges, "factors of new data do not match")))  # only blackboost
special.chars <- which(!is.na(str_match(error.msges, "special characters")))  # Issue-tracker.
pseudo.factor <- which(!is.na(str_match(error.msges, "factors with 2 or more")))  # Issue-tracker.  
missing.arg <- which(!is.na(str_match(error.msges, "argument of length 0")))  # only boosting
error.code.1 <- which(!is.na(str_match(error.msges, "error code 1")))  # only ctree
ds.small.ssrate.large <- which(!is.na(str_match(error.msges, "dataset size is too small")))  # only gbm
scale.default <- which(!is.na(str_match(error.msges, "length of 'center' must equal")))  # only glmboost and lssvm
na.nan.inf <- which(!is.na(str_match(error.msges, "NA/NaN/Inf in foreign function call")))  # only lvq1
na.in.assignment <- which(!is.na(str_match(error.msges, "NAs are not allowed")))  # only mda
null.in.assignment <- which(!is.na(str_match(error.msges, "attempt to set an attribute on NULL")))  # only naiveBayes
constant.variable <- which(!is.na(str_match(error.msges, "to be constant")))  # only lda
java.out.of.bounds <- which(!is.na(str_match(error.msges, "IndexOutOfBounds")))  # only JRip
too.many.weights <- which(!is.na(str_match(error.msges, "too many")))  # only multinom and nnet
rank.deficiency <- which(!is.na(str_match(error.msges, "rank deficiency")))  # only qda
group.too.small <- which(!is.na(str_match(error.msges, "some group is too small")))  # only qda
init.centers <- which(!is.na(str_match(error.msges, "initial centers")))  # only mda
non.conformable.args <- which(!is.na(str_match(error.msges, "non-conformable arguments")))  # only rda

too.many.lvls <- which(!is.na(str_match(error.msges, "predictors with more than")))  # only randomForest
empty.classes <- which(!is.na(str_match(error.msges, "empty classes")))  # only randomForest

replacement.mismatch <- which(!is.na(str_match(error.msges, "number of items to replace")))  # only mda

rest <- seq_along(error.msges)[-c(multiclass, factor.inputs, missing.values, factor.mismatch, special.chars,
  pseudo.factor, missing.arg, error.code.1, ds.small.ssrate.large, scale.default, na.nan.inf,
  na.in.assignment, null.in.assignment, constant.variable, java.out.of.bounds, too.many.weights,
  replacement.mismatch, empty.classes, rank.deficiency, group.too.small, too.many.lvls,
  init.centers, non.conformable.args)]


# all.res[error.inds[pseudo.factor], 1:2]

# summarizeExperiments(reg, special.chars, show = c("lrn", "id"))
