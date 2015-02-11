Upload an implementation
========================

If you want to upload predictions of an algorithm for a certain task, you have to know the algorithm's OpenML implementation ID. If the implementation is not yet registered, you need to upload it. Here, we give you some advice to help you get started.

### Get a valid session hash
In order to upload anything to the server, you have to authenticate your identity first. Therefore, you need to be registered at openml.org. Use the following function to get an authentication hash that is valid for one hour:


```r
hash = authenticateUser(username = "openml.rteam@gmail.com", password = "testpassword")
```

```
## Error in authenticateUser(username = "openml.rteam@gmail.com", password = "testpassword"): unbenutztes Argument (username = "openml.rteam@gmail.com")
```
### Upload an mlr learner
There are some helper functions in case you are using the package [mlr](https://github.com/berndbischl/mlr). Uploading an mlr learner is easy and quickly done. It doesn't matter if the learner has been
uploaded already by you or some other user; `uploadMlrLearner` checks this first and returns the 
flow ID in any case:


```r
library(mlr)
learner = makeLearner("classif.JRip")
flow.id = uploadMlrLearner(learner, hash)
```

### Upload an implementation without using mlr
If you are not using mlr, you will have to invest quite a bit more time to get things done. So -- unless you have good reasons to do otherwise -- we strongly encourage you to use mlr. 

The following example shows how to create an OpenML implementation description object manually.

First, create an implementation parameter list. This is a list that contains an `OpenMLImplementationParameter` for each parameter of your implementation. Let's assume we have written an algorithm that has two parameters called "a" (numeric, default: 500) and "b" (logical, default: TRUE). 

```r
impl.par.a = OpenMLImplementationParameter(
  name = "a", 
  data.type = "numeric", 
  default.value = "500",  # Yes, all defaults must be passed as strings.
  description = "An optional description of parameter a.")  

impl.par.b = OpenMLImplementationParameter(
  name = "b", 
  data.type = "logical", 
  default.value = "TRUE",  
  description = "An optional description of parameter b.")  

impl.pars = list(impl.par.a, impl.par.b)
```
Now we can create the whole description object. Try to find a good name for your algorithm that gives other users an idea of what is happening. 

```r
oml.flow = OpenMLImplementation(
  name = "good_name",
  version = "1.0",
  description = "Please take some time and write a description of your algorithm/changes compared with the previous version/etc. here.",
  parameter = impl.pars)
```
Now we finally have the implementation description object. The last step is to write an R-script with your flow. Let's assume you have done this and have a string `sourcefile` containing the path to your script. Your flow can now be uploaded as follows:

```r
flow.id.2 = uploadOpenMLImplementation(oml.flow, sourcefile = sourcefile, session.hash = hash)
```

----------------------------------------------------------------------------------------------------------------------
Jump to:    
[1 Introduction](1-Introduction.md)    
[2 Configuration](2-Configuration.md)  
[3 Download a task](3-Download-a-task.md)  
4 Upload an implementation  
[5 Upload predictions](5-Upload-predictions.md)  
[6 Download performance measures](6-Download-performance-measures.md)  
[7 Browse the database](7-Browse-the-database.md)  
[8 Example workflow with mlr](8-Example-workflow-with-mlr.md)
