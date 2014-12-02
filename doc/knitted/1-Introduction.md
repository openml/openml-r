Introduction
============

The R package OpenML is an interface to make interactions with the OpenML server as comfortable as possible. Users can download and upload files, run their implementations on specific tasks, get
predictions in the correct form, run SQL queries, etc. directly via R commands. In this tutorial, we
will show you the most important functions of this package and give you examples on standard
workflows.

For general information on what [OpenML](http://openml.org/) is, please have a look at the
[Readme file](https://github.com/openml/OpenML/blob/master/README.md).

There are different stages when using this package:
* Stage 0: Listing
    * function names begin with `listOML`
    * result is always a data.frame
    * available for DataSets, Tasks, Flows, Runs, RunResults, EvaluationMeasures and TaskTypes
* Stage 1: Downloading
    * function names begin with `getOML`
    * result is an object of a specific OpenML class
    * available for DataSets, Tasks, Runs, Predictions and Flows
* Stage 2: Running Models on Tasks
    * runTaskMlr, runTaskCustom
    * input: task and model
    * output: OMLMlrRunResult, OMLRunResult
* Stage 3: Uploading
    * uploadRunResult

----------------------------------------------------------------------------------------------------
Jump to:   
1 Introduction  
[2 Configuration](2-Configuration.md)  
[3 Download a task](3-Download-a-task.md)  
[4 Upload an implementation](4-Upload-an-implementation.md)  
[5 Upload predictions](5-Upload-predictions.md)  
[6 Download performance measures](6-Download-performance-measures.md)  
[7 Browse the database](7-Browse-the-database.md)  
[8 Example workflow with mlr](8-Example-workflow-with-mlr.md)
