\dontrun{
	dat <- getOMLDataSet(did = 9L)

	# this object contains the data ($data) 
	# and meta information
	str(dat, 1)
	summary(dat$data)
}
