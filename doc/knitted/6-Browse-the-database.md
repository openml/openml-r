Browse the database
===================

To browse the data base for appropriate data sets, you can use the so called data qualities. Data qualities contain basic data characteristics (number of features/instances/classes/missing values etc.) as well as values of more abstract meta learning features. 

### Get data qualities
With the function `getDataQualities` you can obtain all data qualities of all stored data sets. 
By default, only the basic data characteristics are retrieved:


```splus
dq = getDataQualities()
```

If you want to retrieve not only the basic data qualities but also meta learning features, 
please use the argument "set":


```splus
dq = getDataQualities(set = "all")
```

Now, you have a data.frame with some data qualities. You can use them to find data sets that meet certain conditions. Let's assume we wanted to work on data sets that have only numeric features and not a single missing value. Therefore, we simply subset the data.frame:


```splus
filtered.dq = subset(dq, NumberOfNumericFeatures == NumberOfFeatures & NumberOfMissingValues == 0)
filtered.dq$dataset
```

Now we obtained the names of all appropriate data sets that are stored on the OpenML server. Finally, we could go ahead and download them as seen in [section 2](2-Download-a-task.md).

### Make an arbitrary SQL-query
The freest way to browse the OpenML database is by SQL-queries. The function `runSQLQuery` is 
an interface for any arbitrary SQL-query. The query is passed as a string to the function as 
you can see in the following example:


```splus
runSQLQuery(query = "SELECT id FROM implementation WHERE name = 'classif.rpart'")
```

Note that most users should not need this function.

----------------------------------------------------------------------------------------------------------------------
Jump to:   
[1 Introduction](1-Introduction.md)  
[2 Download a task](2-Download-a-task.md)  
[3 Upload an implementation](3-Upload-an-implementation.md)  
[4 Upload predictions](4-Upload-predictions.md)  
[5 Download performance measures](5-Download-performance-measures.md)  
6 Browse the database
