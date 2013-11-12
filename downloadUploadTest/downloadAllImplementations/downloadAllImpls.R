library(devtools)
load_all("openML/")
library(BBmisc)

impls <- read.csv(file = "downloadUploadTest/downloadAllImplementations/implementations.csv", sep = ",")
impls <- sprintf("%s(%s)", impls[, 1], impls[, 2])

error_inds <- error_ids <- error_msges <- c()

for(i in seq_along(impls)) {
  dl <- try(downloadOpenMLImplementation(
    impls[i], 
    dir = "downloadUploadTest/downloadAllImplementations/", 
    show.info = FALSE))
  
  if(is.error(dl)) {
    error_inds <- c(error_inds, i)
    error_ids <- c(error_ids, impls[i])
    error_msges <- c(error_msges, dl[[1]])
  } else {
    print(sprintf("%s successfully downloaded.", impls[i]))
  }
}

errors <- data.frame(error_inds, error_ids, error_msges)

# some if-clauses were wrong in downloadOpenMLImplementation.R. fixed. 

# id 15: bad characters ("<") in the xml. "Training set size/(Dataset size - 1) < num < 1.0"

# id 20: "weka.CfsSubsetEval(1.28)" is an unknown implementation.

# id 67: bad character ("<") in the xml. "default: <1: use log(nb inputs) +1"

# id 68: bad character ("<") in the xml. "default: <1: use log(nb attributes)+1"

# id 102: bad character (">", "<") in the xml. 
#   "Add Bias term with the given value if >= 0; if < 0, no bias term added (default: 1)"

# id 143: bad character ("<") in the xml.
#   "Number of attributes to randomly investigate (<0 = int(log_2(#attributes)+1))."

# id 144: bad character (">", "<") in the xml.
#   "< 1: percentage of the number of attributes >=1: absolute number of attributes"

# id 192: bad character ("<") in the xml.
#   "Number of features to consider (<1=int(logM+1))"

# id 193: bad character ("<") in the xml.
#   "(<0 = int(log_2(#attributes)+1))"

# id 202 - 205: 
#   Error in curlPerform(curl = curl, .opts = opts, .encoding = .encoding) : 
#   embedded nul in string: '\037‹\b\0\0\0\0\0\0\006\vr‰0âŠàb```b`f`d`b\0062Y˜€\004#\003\v\003'fOÎÏ+IÍ+\0012\005„Aà\037\0jv¦ò>\0\0\0'
# This is an error regarding the downloading of a sourcefile. In the sourcefile there's only the string
# "Useless sourcefile", so we might wanna ignore this at the moment.