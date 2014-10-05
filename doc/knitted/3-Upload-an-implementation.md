Upload an implementation
========================

If you want to upload predictions of an algorithm for a certain task, you have to know the algorithm's OpenML implementation ID. If the implementation is not yet registered, you need to upload it. Here, we give you some advice to help you get started.

### Get a valid session hash
In order to upload anything to the server, you have to authenticate your identity first. Therefore, you need to be registered at openml.org. Use the following function to get an authentication hash that is valid for one hour:


```splus
hash = authenticateUser(email = "your@email.com", password = "your_password")
```
### Upload an mlr learner
There are some helper functions in case you are using the package [mlr](https://github.com/berndbischl/mlr) (Machine Learning in R). To upload an mlr learner you have to convert it into an OpenML implementation description object. This can be done by the function `createOpenMLImplementationForMLRLearner`:


```splus
library(mlr)
learner = makeLearner("classif.rpart")
oml.flow = createOpenMLImplementationForMLRLearner(learner)
```
This description object can be uploaded as follows: First, we need to generate a sourcefile. This is an .R-file with code that shows other users how you obtain results with your flow. Using mlr, we can generate a small sourcefile then and upload it together with the flow:

```splus
sourcefile = generateSourcefileForMlrLearner(oml.flow)
flow.id = uploadOpenMLImplementation(oml.flow, sourcefile = sourcefile, session.hash = hash)
```

### Upload an implementation without using mlr
If you are not using mlr, you will have to invest quite a bit more time to get things done. So -- unless you have good reasons to do otherwise -- we strongly encourage you to use mlr. 

The following example shows how to create an OpenML implementation description object manually.

First, create an implementation parameter list. This is a list that contains an `OpenMLImplementationParameter` for each parameter of your implementation. Let's assume we have written an algorithm that has two parameters called "a" (numeric, default: 500) and "b" (logical, default: TRUE). 

```splus
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

```splus
oml.flow = OpenMLImplementation(
  name = "good_name",
  version = "1.0",
  description = "Please take some time and write a description of your algorithm/changes compared with the previous version/etc. here.",
  parameter = impl.pars)
```
Now we finally have the implementation description object. The last step is to write an R-script with your flow. Let's assume you have done this and have a string `sourcefile` containing the path to your script. Your flow can now be uploaded as we have seen in the section above:

```splus
flow.id = uploadOpenMLImplementation(oml.flow, sourcefile = sourcefile, session.hash = hash)
```

----------------------------------------------------------------------------------------------------------------------
Jump to:    
[1 Introduction](1-Introduction.md)    
[2 Download a task](2-Download-a-task.md)  
3 Upload an implementation  
[4 Upload predictions](4-Upload-predictions.md)  
[5 Download performance measures](5-Download-performance-measures.md)  
[6 Browse the database](6-Browse-the-database.md)
