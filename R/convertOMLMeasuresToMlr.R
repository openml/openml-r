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
    "mean_absolute_error" = mae,
    "root_mean_squared_error" = rmse,
    "area_under_roc_curve" = auc,
    #"build_cpu_time" = timetrain,
    "f_measure" = f1,
    "matthews_correlation_coefficient" = mcc,
    "precision" = ppv,
    "predictive_accuracy" = acc,
    "recall" = tpr,
    "c_index" = cindex,
    "usercpu_time_millis" = timeboth,
    "usercpu_time_millis_testing" = timepredict,
    "usercpu_time_millis_training" = timetrain
  )
}

convertOMLMeasuresToMlr = function(measures) {
  lookup = lookupMeasures()
  assertSubset(measures, names(lookup))
  return(lookup[measures])
}

convertMlrMeasuresToOMLMeasures = function(mlr.measures) {
  lookup = lookupMeasures()
  lookup.ids = vcapply(lookup, function(x) x$id)
  if (inherits(mlr.measures, "Measure"))
    mlr.measures = mlr.measures$id else {
      assertList(mlr.measures)
      mlr.measures = vcapply(mlr.measures, function(x) x$id)
    }
      
  assertSubset(mlr.measures, lookup.ids)
  return(names(lookup.ids[lookup.ids%in%mlr.measures]))
}