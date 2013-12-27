Browse the database
===================

Data sets have different characteristics and users might want to work on data sets that meet certain conditions. 

### Get basic data characteristics

The easiest way to retrieve a data.frame containing basic data characteristics like the number of features/instances/classes/missing values etc. of all stored data sets is the following:


```r
data.chars <- getDataCharacteristics()
```


### Get meta learning features
With the function `getMetaLearningFeatures` you can obtain all data qualities of all stored data sets. This includes meta learning features as well as the basic characteristics that can be retrieved by `getDataCharacteristics`. If you only want a few of these data qualities, you can pass them in form of a character vector to the function. A list of all data quality names can be obtained by the function `getDataQualitiyNames`.


```r
ml.feats <- getMetaLearningFeatures()

dq.names <- getDataQualitiyNames()
ml.feats <- getMetaLearningFeatures(only = dq.names[1:5])
```


### Make an arbitrary SQL-query
The most free way to browse the OpenML database is by SQL-queries. The function `openMLSQLQuery` is an interface for any arbitrary SQL-query. The query is passed as a string to the function as you can see in the following example:


```r
openMLSQLQuery(SQL = "SELECT id FROM implementation WHERE name = 'classif.rpart'")
```


----------------------------------------------------------------------------------------------------------------------
Jump to:   
[1 Introduction](1-Introduction.md) 
[2 Download a task](2-Download-a-task.md)  
[3 Upload an implementation](3-Upload-an-implementation.md)  
[4 Upload predictions](4-Upload-predictions.md)  
[5 Download performance measures](5-Download-performance-measures.md)
6 Browse the database
