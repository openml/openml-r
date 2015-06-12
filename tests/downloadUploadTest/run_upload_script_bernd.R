library(OpenML)
library(mlr)

# download open ml task via ID
# actually this is iris, with 10CV and 2 reps
omltask = downloadOpenMLTask(id = 1)
print(omltask)

# lets create a simple decision tree from mlr
learner = makeLearner("classif.rpart")

# we resample it on the task via the given splits
# and measure the associated performance
# as we use a "standard algorithm"  the openml package
# basically does all the work for us, no futher coding necessary
result = runTask(omltask, learner)

# we want to upload our results so "knock, knock" at openml server
#hash = authenticateUser(username = "bernd_bischl@gmx.net", password = "not_your_business")
hash <- authenticateUser(username = "dominik.kirchhoff@tu-dortmund.de", password = "testpasswort")

# upload results and info about our experiment
# the follwoing 2 functions are only 90% finished 90% and will soon work
impl = createOpenMLImplementationForMLRLearner(learner)
uploadOpenMLImplementation(impl, session.hash = hash)

run.desc <- OpenMLRun(
  task.id = as.character(omltask@task.id),
  implementation.id = sprintf("%s(%s)", impl@name, impl@version),
  parameter.settings = makeRunParameterList(learner))

uploadOpenMLRun(run.desc = run.desc, predictions = result, session.hash = hash)
