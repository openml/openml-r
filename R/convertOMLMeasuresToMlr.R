# FIXME: add more metrics/measures.
# FIXME: task.type not used at the moment

# @title Convert OML measure to mlr objects.
#
# @description Simple mapping from OML measures to
# mlr measure functions.
#
# @param measures [character]
#   Measure names.
# @return [list]

lookupMeasures = function() {
  list(
    "mean_absolute_error" = mlr::mae,
    "root_mean_squared_error" = mlr::rmse,
    "area_under_roc_curve" = mlr::auc,
    #"build_cpu_time" = mlr::timetrain,
    "f_measure" = mlr::f1,
    "matthews_correlation_coefficient" = mlr::mcc,
    "precision" = mlr::ppv,
    "predictive_accuracy" = mlr::acc,
    "recall" = mlr::tpr,
    "c_index" = mlr::cindex,
    "usercpu_time_millis" = mlr::timeboth,
    "usercpu_time_millis_testing" = mlr::timepredict,
    "usercpu_time_millis_training" = mlr::timetrain
  )
}

convertOMLMeasuresToMlr = function(measures) {
  lookup = lookupMeasures()
  assertSubset(measures, names(lookup))
  mlr.measures = lookup[measures]
  mlr.measures = lapply(mlr.measures, mlr::setAggregation, aggr = mlr::test.join)
  return(mlr.measures)
}

# convertMlrMeasuresToOMLMeasures = function(mlr.measures) {
#   lookup = lookupMeasures()
#   lookup.ids = vcapply(lookup, function(x) x$id)
#   if (inherits(mlr.measures, "Measure"))
#     mlr.measures = mlr.measures$id else {
#       assertList(mlr.measures)
#       mlr.measures = vcapply(mlr.measures, function(x) x$id)
#     }
#
#   assertSubset(mlr.measures, lookup.ids)
#   return(names(lookup.ids[lookup.ids%in%mlr.measures]))
# }
