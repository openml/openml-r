#' @title Download an OpenML Object.
#'
#' @description
#' Download all files that refer to data, task, flow or run and
#' returns the corresponding parsed XML.
#'
#' @param id [\code{integer(1)}]\cr
#'   The ID of the OpenML Object.
#' @param object [\code{character(1)}]\cr
#'   The data, task, flow or run that should be downloaded.
#'   Default is none.
#' @param overwrite [\code{logical(1)}]\cr
#'   Should files that are already in cache be overwritten?
#'   Default is \code{FALSE}.
#' @template arg_cache_only
#' @template arg_verbosity
#' @keywords internal
#' @return [list]
downloadOMLObject = function(id, object = c("data", "task", "flow", "run"), overwrite = FALSE, cache.only = FALSE, verbosity = NULL) {
  id = asCount(id)
  assertChoice(object, choices = c("data", "task", "flow", "run"))

  # look if desired file(s) are already in cache
  cap.obj = object
  substr(cap.obj, 1, 1) = toupper(substr(object, 1, 1))
  f = do.call(paste0("findCached", cap.obj), list(id))

  # if cache.only option is active and there is a file which is not in cache, stop with an error
  # FIXME: This does not work if there is flow.xml but the corresponding source or binary files were removed;
  #   can be fixed if findCachedFlow outputs the file from source_url or binary_url if there is one
  if (all(vlapply(f, function(X) !X$found)) & cache.only)
    stopf("%s '%i' files not found in cache with option 'cache.only'.", cap.obj, id)

  # download and write xml if not found in cache (and if overwrite is true)
  xml.ind = stri_endswith_fixed(names(f), "xml")
  file.ind = which(!xml.ind)
  xml.ind = which(xml.ind)
  if (f[[xml.ind]]$found & !overwrite) {
    # parse info
    showInfo(verbosity, sprintf("%s '%i' file '%s' found in cache.", cap.obj, id, basename(f[[xml.ind]]$path)))
    content = readLines(f[[xml.ind]]$path, warn = FALSE)
  } else {
    content = doAPICall(api.call = object, id = id, file = f[[xml.ind]]$path,
      verbosity = verbosity, method = "GET")
    # set found = TRUE if downloaded file is in cache
    if (file.exists(f[[xml.ind]]$path)) f[[xml.ind]]$found = TRUE
  }
  # look for an error in xml and stop if there is one
  xml.type = ifelse(object == "data", "data_set_description", object)
  doc = tryCatch(parseXMLResponse(content, msg = paste0("Getting ", stri_replace_all_fixed(xml.type, "_", " ")), type = xml.type, as.text = TRUE),
    error = function(e) {
      unlink(f[[xml.ind]]$path, recursive = TRUE, force = TRUE)
      stop(e)
    })
  # if (is.error(doc)) {
  #   unlink(f[[xml.ind]]$path, recursive = TRUE, force = TRUE)
  #   stop(doc)
  # }

  ## now download files
  # get url of files
  if (object == "data") {
    url = xmlRValS(doc, "/oml:data_set_description/oml:url")
  } else if (object == "task") {
    url = xmlOValS(doc, "/oml:task/oml:input/oml:estimation_procedure/oml:data_splits_url")
  } else if (object == "flow") {
    #FIXME: Do this in findCachedFlow to find the filename of the flow (and if its binary or source)?
    # flows have either a source.url, a binary.url or nothing to be downloaded
    source.url = xmlOValS(doc, "/oml:flow/oml:source_url")
    binary.url = xmlOValS(doc, "/oml:flow/oml:binary_url")
    if (!is.null(source.url) & !is.null(binary.url)) {
      source.file = readLines(source.url, warn = FALSE)
      binary.file = readLines(binary.url, warn = FALSE)
      # if they are identical, we can go on and download just one of them
      if (!identical(source.file, binary.file))
        stopf("Flow '%i' contains both source.url and binary.url, which is currently not supported.", id)
    }
    # get url if there is either a source.url or a binary.url
    url = if (is.null(binary.url)) source.url else binary.url
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
    url = url[url != ""]
  }

  # download files if there is an url
  if (!is.null(url) & length(url) != 0) {
    if (f[[file.ind]]$found & !overwrite) {
      showInfo(verbosity, sprintf("%s '%i' file '%s' found in cache.", cap.obj, id, basename(f[[file.ind]]$path)))
    } else {
      url = stri_trim_both(url)
      showInfo(verbosity, "Downloading from '%s' to '%s'", url, f[[file.ind]]$path)
      download.file(url, destfile = f[[file.ind]]$path,
        mode = ifelse(!is.null(f[[file.ind]]$binary), ifelse(f[[file.ind]]$binary, "wb", "w"), "w"),
        #FIXME: do we want to get real verbosity level here >= info ?
        quiet = TRUE,
        method = getOMLConfig()$download.method
      ) #!as.logical(getOMLConfig()$verbosity))
      # set found = TRUE if downloaded file is in cache
      if (file.exists(f[[file.ind]]$path)) f[[file.ind]]$found = TRUE
    }
  }
  return(list(doc = doc, files = f))
}

