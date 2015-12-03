# FIXME: add more metrics/measures.
# FIXME: task.type not used at the moment

# @title Convert OML measure to mlr objects.
#
# @description Simple mapping from OML measures to
# mlr measure functions.
#
# @param measures [character]
#   Measure names.
# @param task.type [character]
#   Task type.
# @return [list]
convertOMLMeasuresToMlr = function(measures, task.type) {
  lookup = list(
    "mean_absolute_error" = mae,
    "root_mean_squared_error" = rmse,
    "area_under_roc_curve" = auc,
    "build_cpu_time" = timetrain,
    "f_measure" = f1,
    "matthews_correlation_coefficient" = mcc,
    "precision" = ppv,
    "predictive_accuracy" = acc,
    "recall" = tpr,
    "c_index" = cindex
  )

  assertSubset(measures, names(lookup))
  return(lookup[measures])
}
