library(mlr)

lrn <- makeLearner("classif.rpart")
bagging <- makeBaggingWrapper(lrn, bag.iters = 500)
impl <- OpenMLImplementation(
  name = "twoComponents",
  version = "1.0",
  description = "This is just a test to try parsing an implementation with two components.",
  parameter = makeImplementationParameterList(bagging),
  components = list(OpenMLImplementation(
    name = "rpart", 
    version = "1.0", 
    description = "rpart decision tree from package rpart",
    parameter = makeImplementationParameterList(lrn)),
    OpenMLImplementation(name = "J48", version = "1.0"))
)

implXML <- writeOpenMLImplementationXML(impl, file="downloadUploadTest/twoComponents.xml")
session.hash <- authenticateUser("dominik.kirchhoff@tu-dortmund.de", "testpasswort")

uploadOpenMLImplementation("downloadUploadTest/twoComponents.xml", 
                           sourcefile = "downloadUploadTest/rpartforest_sourcefile.R", 
                           session.hash = session.hash) 
# Uploading implementation to server.
# Downloading response to: C:\Users\Dom\AppData\Local\Temp\RtmpodPygH\file52c7c6a31e1
# Implementation successfully uploaded. Implementation ID: J48-forest(1.0)

# Download the implementation again:
impl_dl <- downloadOpenMLImplementation("twoComponents(1.0)")
