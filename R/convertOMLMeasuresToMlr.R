# FIXME: add more metrics/measures.

# convert OML measure to mlr objects
convertOMLMeasuresToMlr = function(measures, task.type) {
  assertCharacter(measures, any.missing = FALSE)


  lookup = list(
    "meanabsoluteerror" = mmce,
    "rootmeansquarederror" = rmse,
    "areaunderroccurve" = auc,
    "buildcputime" = timetrain,
    "fmeasure" = f1,
    "matthewscorrelationcoefficient" = mcc,
    "precision" = ppv,
    "predictiveaccuracy" = acc,
    "recall" = tpr,
    "cindex" = cindex
  )
  lookup[measures]

}

