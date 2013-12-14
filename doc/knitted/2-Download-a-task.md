Download a task
===============

An OpenML task is an object that contains information on the data set, the task type (classification/regression) and the estimation procedure. A user can download a task from the OpenML server, compute predictions with an algorithm and upload this algorithm as well as the predictions. The server will then calculate many different measures and add them to the data base.

To download a certain task from the OpenML server, you need to know the task's ID. You can look for appropriate task IDs on  
http://openml.org/search. 

The following call returns an OpenML task object: 


```r
task <- downloadOpenMLTask(id = 4)
print(task)
```


Note: You do not have to be registered at http://openml.org/ to download a task. 

----------------------------------------------------------------------------------------------------------------------
Jump to:    
[1 Introduction](1-Introduction.md)    
2 Download a task  
[3 Upload an implementation](3-Upload-an-implementation.md)  
[4 Upload predictions](4-Upload-predictions.md)  
[5 Download performance measures](5-Download-performance-measures.md)
