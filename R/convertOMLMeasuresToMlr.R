# FIXME: add more metrics/measures.

# convert OML measure to mlr objects
convertOMLMeasuresToMlr = function(measures, task.type) {
  assertCharacter(measures, any.missing = FALSE)

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

  lookup[measures]
}

