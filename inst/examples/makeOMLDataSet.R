data(CHFLS, package = "HSAUR3")
dsc <- "The Chinese Health and Family Life Survey sampled $60$ villages and urban 
neighborhoods chosen in such a way as to represent the full geographical and 
socioeconomic range of contemporary China.
This data set is taken from R package HSAUR3 (version 1.0-5)"
cit <- "William L. Parish, Edward O. Laumann, Myron S. Cohen, Suiming Pan, Heyi Zheng, 
Irving Hoffman, Tianfu Wang, and Kwai Hang Ng. (2003), Population-Based Study of 
Chlamydial Infection in China: A Hidden Epidemic. Journal of the American Medican 
Association, 289(10), 1265–1273."
desc_chfls <- makeOMLDataSetDescription(name = "CHFLS", 
    description = dsc, 
    creator = "Chinese Health and Family Life Survey",
    contributor = "Torsten Hothorn and Brian S. Everitt",
    collection.date = as.POSIXct("2015-07-28"),
    language = "English",
    licence = "GPL-2", 
    url = "https://cran.r-project.org/web/packages/HSAUR3/",
    default.target.attribute = NA_character_,
    citation = cit, 
    original.data.url = "http://www.spc.uchicago.edu/prc/chfls.php",
    tags = "R")

chfls_oml <- makeOMLDataSet(desc = desc_chfls, 
                            data = CHFLS, 
                            colnames.old = colnames(CHFLS), 
                            colnames.new = colnames(CHFLS),
                            target.features = c("R_income", "R_happy", "A_income"))
