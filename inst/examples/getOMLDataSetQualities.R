\dontrun{
a <- getOMLDataSetQualities(did = 9L)
a[a$name == "NumberOfMissingValues", ]
getOMLDataSetQualities(did = 9L, 
		       name = "NumberOfMissingValues")
}
