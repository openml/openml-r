library(mlr)
library(BBmisc)
library(RCurl)
library(devtools)
load_all("OpenML/")


# IMPLEMENTATIONEN: upload, download

lrn <- makeLearner("classif.JRip")

OpenML_impl <- createOpenMLImplementationForMLRLearner(lrn)

hash <- authenticateUser("dominik.kirchhoff@tu-dortmund.de", "testpasswort")

uploadOpenMLImplementation(OpenML_impl, session.hash=hash)

impl_dl <- downloadOpenMLImplementation("classif.JRip(0.4-18)")

# TASKS: download

task <- downloadOpenMLTask(4)

# RUNS: Anwendung einer geeigneten Implementation (mit konkreten Parametersettings) auf einen Task.

run_preds <- runTask(task, lrn)

#run.desc <- OpenMLRun(
#  task.id = "4", 
#  implementation.id = "classif.JRip(0.4-18)", 
#  parameter.settings = makeRunParameterList(lrn))

run_ul <- uploadOpenMLRun(task, lrn, OpenML_impl, run_preds, hash)

# RUN RESULTS: download

run_results <- downloadOpenMLRunResults(run_ul)

run_results@metrics

# TASK RESULTS: download

task_results <- downloadOpenMLTaskResults(4)