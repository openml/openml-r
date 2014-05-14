#' Download an OpenML implementation from the OpenML server through a server API call.
#' 
#' Retrieves an implementation for a given id. 
#' 
#' @param id [\code{character(1)}]\cr
#'   The implementation id.
#' @param dir [\code{character(1)}]\cr 
#'   Directory where downloaded files from the repository are stored. 
#'   Default is current directory.
#' @param download.source.binary [\code{logical(1)}]\cr    
#'   Should source / binary files of the implementation also be downloaded?
#'   They will also be stored in \code{dir}.
#'   Default is \code{TRUE}.
#' @param show.info [logical(1)]\cr
#'   Verbose output on console? 
#'   Default is \code{TRUE}.
#' @return [\code{\link{OpenMLImplementation}}].
#' @export

downloadOpenMLImplementation <- function(id, dir = getwd(), download.source.binary = TRUE, show.info = TRUE) {
  checkArg(id, "character", len = 1L, na.ok = FALSE)
  checkArg(dir, "character", len = 1L, na.ok = FALSE)
  checkArg(download.source.binary, "logical", len = 1L, na.ok = FALSE)
  fn.impl.xml <- file.path(dir, sprintf("%s.xml", id))  
  downloadAPICallFile(api.fun = "openml.implementation.get", file = fn.impl.xml, implementation_id = id, show.info = show.info)  
  impl <- parseOpenMLImplementation(fn.impl.xml)
  if (download.source.binary) {
    if (length(impl@source.url) > 0 && impl@source.url != "") {
      if (show.info)
        messagef("Downloading implementation source file from URL:\n%s.", impl@source.url)
      
      format <- rev(strsplit(rev(strsplit(impl@source.url, "/")[[1]])[1], "[.]")[[1]])[1]
      fn.impl.src <- sprintf("%s(%s)_source.%s", impl@name, impl@version, format)
      fn.impl.src <- file.path(dir, fn.impl.src) 
      downloadBinaryFile(url = impl@source.url, file = fn.impl.src, show.info = show.info)
    }
    if (length(impl@binary.url) > 0 && impl@source.url != "") {
      if (show.info)
        messagef("Downloading implementation binary file.")

      fn.impl.src <- sprintf("%s(%s)_binary", impl@name, impl@version)
      fn.impl.bin <- file.path(dir, fn.impl.bin)  
      downloadBinaryFile(url = impl@binary.url, file = fn.impl.bin, show.info = show.info)
    }
  }
  return(impl)
}

parseOpenMLImplementation <- function(file) {
  checkArg(file, "character", len = 1L, na.ok = FALSE)
  doc <- parseXMLResponse(file, "Getting implementation", "implementation")
  args <- list()
  args[["id"]] <- xmlRValS(doc, "/oml:implementation/oml:id")
  args[["name"]] <- xmlRValS(doc, "/oml:implementation/oml:name")
  args[["version"]] <- xmlRValS(doc, "/oml:implementation/oml:version")
  args[["external.version"]] <- xmlRValS(doc, "/oml:implementation/oml:external_version")
  args[["description"]] <- xmlRValS(doc, "/oml:implementation/oml:description")
  args[["creator"]] <- xmlValsMultNsS(doc, "/oml:implementation/oml:creator")
  args[["contributor"]] <- xmlValsMultNsS(doc, "/oml:implementation/oml:contributor")
  args[["upload.date"]] <- xmlRValS(doc, "/oml:implementation/oml:upload_date")
  args[["licence"]] <- xmlOValS(doc, "/oml:implementation/oml:licence")
  args[["language"]] <- xmlOValS(doc, "/oml:implementation/oml:language")
  args[["full.description"]] <- xmlOValS(doc, "/oml:implementation/oml:full_description")
  args[["installation.notes"]] <- xmlOValS(doc, "/oml:implementation/oml:installation_notes")
  args[["dependencies"]] <- xmlOValS(doc, "/oml:implementation/oml:dependencies")
  args[["bibliographical.reference"]] <- parseOpenMLBibRef(doc)
  #args[["implements"]] <- xmlOValS(doc, "/oml:implementation/oml:implements")
  args[["parameter"]] <- parseOpenMLParameters(doc)  
  
  ## components section
  comp_ns <- getNodeSet(doc, "/oml:implementation/oml:components/oml:implementation")
  comp <- list()
  for(i in seq_along(comp_ns)){
    file2 <- sprintf("%s/downloadUploadTest/comp.xml", getwd())
    saveXML(comp_ns[[i]], file = file2)
    comp <- c(comp, parseOpenMLImplementation(file2))
    unlink(file2)
  }
  args[["components"]] <- comp


  args[["collection.date"]] <- xmlOValS(doc, "/oml:implementation/oml:collection_date")
  args[["source.url"]] <- xmlOValS(doc, "/oml:implementation/oml:source_url")
  args[["binary.url"]] <- xmlOValS(doc, "/oml:implementation/oml:binary_url")
  args[["source.format"]] <- xmlOValS(doc, "/oml:implementation/oml:source_format")
  args[["binary.format"]] <- xmlOValS(doc, "/oml:implementation/oml:binary_format")
  args[["source.md5"]] <- xmlOValS(doc, "/oml:implementation/oml:source_md5")
  args[["binary.md5"]] <- xmlOValS(doc, "/oml:implementation/oml:binary_md5")

  impl <- do.call(OpenMLImplementation, args)

  return(impl)
}

parseOpenMLParameters <- function(doc) {  
  path <- "/oml:implementation/oml:parameter"
  
  ns <- getNodeSet(doc, path)
  
  par.names <- xmlValsMultNsS(doc, sprintf("%s/oml:name", path))
  par.types <- xmlValsMultNsS(doc, sprintf("%s/oml:data_type", path))
  par.defs <- xmlValsMultNsS(doc, sprintf("%s/oml:default_value", path))
  par.descs <- xmlValsMultNsS(doc, sprintf("%s/oml:description", path))
  
  par <- list()
  for (i in 1:length(par.names)) {
    par <- c(par, 
      OpenMLImplementationParameter(par.names[i], par.types[i], par.defs[i], par.descs[i]))
  }
  return(par)
}

parseOpenMLBibRef <- function(doc) {  
  path <- "/oml:implementation/oml:bibliographical_reference"
  
  ns <- getNodeSet(doc, path)
  
  bib.citation <- xmlValsMultNsS(doc, sprintf("%s/oml:citation", path))
  bib.url <- xmlValsMultNsS(doc, sprintf("%s/oml:url", path))
  
  bib <- list()
  for (i in seq_along(bib.citation)) {
    bib <- c(bib, 
      OpenMLBibRef(bib.citation[i], bib.url[i]))
  }
  return(bib)
}
