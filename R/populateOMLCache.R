#' @title Download a bunch of OpenML objects to cache.
#'
#' @description
#' Given a set of OML object ids,  the function populates the cache directory by downloading the
#' corresponding objects. This can avoid network access in later experiments, as you can retrieve
#' all objects from the cache on disk.
#' This is of particular interest in highly parallel computations on
#' a cluster with a shared file system.
#'
#' @param dids [\code{integer}]\cr
#'   Dataset ids.
#'   Default is none.
#' @param task.ids [\code{integer}]\cr
#'   Task ids.
#'   Default is none.
#' @param flow.ids [\code{integer}]\cr
#'   Flow ids.
#'   Default is none.
#' @param run.ids [\code{integer}]\cr
#'   Run ids.
#'   Default is none.
#' @template arg_verbosity
#' @param overwrite [\code{integer(1)}]\cr
#'   Should files that are already in cache be overwritten?
#' @return [\code{invisible(NULL)}]
#' @export
populateOMLCache = function(dids = integer(0L), task.ids = integer(0L),
  flow.ids = integer(0L), run.ids = integer(0L), verbosity = NULL, overwrite = FALSE) {

  # sanity check passed stuff
  task.ids = asInteger(task.ids, lower = 1L, any.missing = FALSE, unique = TRUE)
  flow.ids = asInteger(flow.ids, lower = 1L, any.missing = FALSE, unique = TRUE)
  run.ids = asInteger(run.ids, lower = 1L, any.missing = FALSE, unique = TRUE)
  dids = asInteger(dids, lower = 1L, any.missing = FALSE, unique = TRUE)

  # Helper function to dispatch to the download function
  downloadStuff = function(type, fun, ids, ...) {
    if (length(ids) > 0L) {
      showInfo(verbosity, "Downloading '%s' to cache.", type)
      lapply(ids, fun, verbosity = verbosity, overwrite = overwrite, ...)
    }
  }

  downloadStuff("datsets", downloadOMLObject, dids, object = "data")
  downloadStuff("tasks", downloadOMLObject, task.ids, object = "task")
  downloadStuff("flows", downloadOMLObject, flow.ids, object = "flow")
  downloadStuff("runs", downloadOMLObject, run.ids, object = "run")

  return(invisible(NULL))
}

# Downloads all files that refer to data, task, flow or run and returns the corresponding parsed xml
downloadOMLObject = function(id, object = c("data", "task", "flow", "run"), 
  overwrite = FALSE, cache.only = FALSE, verbosity = NULL) {
  
  id = asCount(id)
  assertChoice(object, choices = c("data", "task", "flow", "run"))
  
  ## look if desired file(s) are already in cache
  cap.obj = object
  substr(cap.obj, 1,1) = toupper(substr(object, 1,1))
  f = do.call(paste0("findCached", cap.obj), list(id))
  
  if (all(vlapply(f, function(X) !X$found)) & cache.only)
    stopf("%s '%i' not found in cache with option 'cache.only'", cap.obj, id)
  
  ## download and write xml if not found in cache (and if overwrite is true)
  xml.ind = grep("xml", names(f))
  file.ind = seq_along(f)[!seq_along(f)%in%xml.ind]
  if (f[[xml.ind]]$found & !overwrite) {
    # parse info
    showInfo(verbosity, paste0(cap.obj, " description found in cache."))
    content = readLines(f[[xml.ind]]$path)
  } else {
    content = doAPICall(api.call = object, id = id, file = f[[xml.ind]]$path,
      verbosity = verbosity, method = "GET")
  }
  # look for an error in xml and stop if there is one
  xml.type = ifelse(object == "data", "data_set_description", object)
  doc = parseXMLResponse(content, msg = paste0("Getting ", gsub("_", " ", xml.type)), type = xml.type, as.text = TRUE)
  
  ## now download files
  # get url of files
  if (object == "data") url = xmlRValS(doc, "/oml:data_set_description/oml:url") else
    if (object == "task") url = xmlOValS(doc, "/oml:task/oml:input/oml:estimation_procedure/oml:data_splits_url") else
      if (object == "flow") {
        # flows have either a source.url, a binary.url or nothing to be downloaded
        source.url = xmlOValS(doc, "/oml:flow/oml:source_url")
        binary.url = xmlOValS(doc, "/oml:flow/oml:binary_url")
        # get url if there is either a source.url or a binary.url
        url = if (is.null(binary.url)) source.url else binary.url
        # FIXME: can there be a source AND a binary url?
        if(!is.null(source.url) & !is.null(binary.url)) stop("source.url and binary.url found")
        # if there is a url
        if (!is.null(url)) {
          # get name of the file that has to be downloaded 
          file.name = basename(url)
          # is it in the cache?
          f = findCachedFlow(id, file.name)
          f[[file.name]]$binary = ifelse(!is.null(binary.url), TRUE, FALSE)
          file.ind = which(names(f) == file.name)
        }
      } else if (object == "run") {
        # get url of runs
        url = vcapply(getNodeSet(doc, "/oml:run/oml:output_data/oml:file"),
          function(X) {
            child = getChildrenStrings(X)
            if (child["name"] == "predictions") return(child["url"]) else ""
          })
        url = url[url!=""]
      }

  # download files if there is an url
  if (!is.null(url) & length(url) != 0) {
    if (f[[file.ind]]$found & !overwrite) {
      showInfo(verbosity, paste0(cap.obj, " file found in cache."))
    } else {
      url = stri_trim_both(url)
      showInfo(verbosity, "Downloading from '%s' to '%s'", url, f[[file.ind]]$path)
      download.file(url, destfile = f[[file.ind]]$path, 
        mode = ifelse(!is.null(f[[file.ind]]$binary), ifelse(f[[file.ind]]$binary, "wb", "w"), "w"),
        #FIXME: do we want to get real verbosity level here >= info ?
        quiet = TRUE) #!as.logical(getOMLConfig()$verbosity))
    }
  }
  return(list(doc = doc, files = f))
}

