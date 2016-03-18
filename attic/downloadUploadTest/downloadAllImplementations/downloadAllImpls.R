library(devtools)
load_all("openML/")
library(BBmisc)

impls <- read.csv(file = "downloadUploadTest/downloadAllImplementations/implementations.csv", sep = ",")
impls <- sprintf("%s(%s)", impls[, 1], impls[, 2])

error_inds <- error_ids <- error_msges <- c()

for(i in seq_along(impls)) {
  dl <- try(downloadOpenMLImplementation(
    impls[i], 
    dir = "downloadUploadTest/downloadAllImplementations/Downloads", 
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

# id 202 - 205 (classif.J48(1.0), ..., classif.J48(4.0)): 
#   Error in curlPerform(curl = curl, .opts = opts, .encoding = .encoding) : 
#   embedded nul in string: '\037‹\b\0\0\0\0\0\0\006\vr‰0âŠàb```b`f`d`b\0062Y˜€\004#\003\v\003'fOÎÏ+IÍ+\0012\005„Aà\037\0jv¦ò>\0\0\0'
# This is an error regarding the downloading of a sourcefile. In the sourcefile there's only the string
# "Useless sourcefile", so we might wanna ignore this at the moment.