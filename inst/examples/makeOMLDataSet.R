data("airquality")
dsc <- "aily air quality measurements in New York, May to September 1973.
This data is taken from R and can be loaded via data('airquality')"
cit <- "Chambers, J. M., Cleveland, W. S., Kleiner, B. and Tukey, P. A. (1983) Graphical 
Methods for Data Analysis. Belmont, CA: Wadsworth."
desc_airquality <- makeOMLDataSetDescription(name = "airquality", 
    description = dsc, 
    creator = "New York State Department of Conservation (ozone data) and the National 
    Weather Service (meteorological data)",
    collection.date = "May 1, 1973 to September 30, 1973",
    language = "English",
    licence = "GPL-2", 
    url = "https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/00Index.html",
    default.target.attribute = "Ozone",
    citation = cit, 
    tags = "R")

airquality_oml <- makeOMLDataSet(desc = desc_airquality, 
                            data = airquality, 
                            colnames.old = colnames(airquality), 
                            colnames.new = colnames(airquality),
                            target.features = "Ozone")
