Upload predictions
==================

The predictions data.frame has to be in a certain form in order to be accepted by the server. These columns have to be present:
* repeat 
* fold 
* row_id     
* prediction  

and additionally, in case of a classification task:
* confidence.*classname_1* 
* confidence.*classname_2* 
* ... (one column for each level of the target variable).

**Note: The columns "repeat", "fold" and "row_id" have to be zero-based!** 

Example: An excerpt of predictions (Iris data set, 10-fold CV, 2 repeats).

        repeat fold row_id      prediction confidence.Iris-setosa confidence.Iris-versicolor confidence.Iris-virginica  
    1        0    0    140  Iris-virginica                      0                          0                         1  
    ...    ...  ...    ...             ...                    ...                        ...                       ...  
    51       0    3     37     Iris-setosa                      1                          0                         0  
    ...    ...  ...    ...             ...                    ...                        ...                       ...  
    150      0    9     76  Iris-virginica                      0                          0                         1  
    151      1    0    110  Iris-virginica                      0                          0                         1  
    ...    ...  ...    ...             ...                    ...                        ...                       ...  
    300      1    9     58 Iris-versicolor                      0                          1                         0  

### Compute predictions of an mlr learner for an OpenML task
If you are working with [mlr](https://github.com/berndbischl/mlr), you can use the OpenML function `runTask` that returns a data.frame of predictions in the desired form:


```r
predictions <- runTask(task, learner)
```


If the prediction type of the learner is set to "response" instead of "prob", the confidence-columns will contain only 0s and 1s like in the example above. Else, the predicted class probabilities will be used.

### Upload predictions to the server

```r
run_ul <- uploadOpenMLRun(task, learner, openML.impl, predictions, hash)
```


----------------------------------------------------------------------------------------------------------------------
Jump to:    
[1 Introduction](1-Introduction.md)    
[2 Download a task](2-Download-a-task.md)  
[3 Upload an implementation](3-Upload-an-implementation.md)  
4 Upload predictions  
[5 Download performance measures](5-Download-performance-measures.md)
