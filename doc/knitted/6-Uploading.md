Uploading
=========





(**??? Fixme: We need to make the results from section 5 available here. Is there an easy and good way to source all chunks from sec. 5 here but do not include the output? http://yihui.name/knitr/demo/child/ seems not to work.**)

### Upload a flow using mlr

(**??? Fixme: Shoudln't we explain first, why we need to upload the implementation before uploading the results? And isn't that needed only once?**)

A flows is an implementation of single algorithms or scripts. To create an flow, we can use the `mlr` package. Each `mlr` learner can be considered as an implementation of a flow that can be uploaded to the server with the function `uploadOMLFlow`.
If the flow has already been uploaded to the server, we get a message that the flow already exists and the `flow.id` is returned from the function. Otherwise, the not existing flow is uploaded and a new `flow.id` is assigned to it.


```r
library(mlr)
lrn = makeLearner("classif.randomForest")
flow.id = uploadOMLFlow(lrn)
flow.id
```

```
## [1] 2393
```
(**??? Fixme: What happens if a flow already exists on the server? And what happens here if we use the read-only API key? The tutorial states that we get a message that the flow already exists. This is not the case!**)

### Upload a flow without using mlr

(**??? Fixme: We don't want to do this with the sourcefile anymore. Should we provide a link how to create a mlr-learner as alternative?**)

In the previous section, we explained how to create an `OMLFlow` manually and created the object `oml.flow`, which reflects the description object of the flow. Before you can upload this flow to the server, you have to write an R-script containing the algorithm you want to use as flow. Let's assume you have done this and have a string `sourcefile` containing the path to your R-script. Your flow can now be uploaded as follows:


```r
oml.flow.id = uploadOMLFlow(oml.flow, sourcefile = sourcefile)
oml.flow.id
```

### Upload an OpenML run to the server

Runs that have been created using `mlr` can be uploaded by:


```r
run.id = uploadOMLRun(run.mlr)
```

Before the run is uploaded, `uploadOMLRun` is looking if the flow that created this run is available on the server. If the flow is not available on the server, it is automatically uploaded.
(**??? Fixme: Cannot be executed with read-only API key. I still would like to execute this in order to check the code...**)

----------------------------------------------------------------------------------------------------
Jump to:
- [Introduction](1-Introduction.md)
- [Configuration](2-Configuration.md)
- [Listing](3-Listing.md)
- [Downloading](4-Downloading.md)
- [Running models on tasks](5-Running.md)
- Uploading
- [Example workflow with mlr](7-Example-workflow-with-mlr.md)
