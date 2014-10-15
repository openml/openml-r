#' Download an OpenML implementation from the OpenML server through a server API call.
#' 
#' Retrieves an implementation for a given id. 
#' 
#' @param id [\code{integer(1)}]\cr
#'   The implementation id.
#' @param dir [\code{character(1)}]\cr 
#'   Directory where downloaded files from the repository are stored. 
#'   Default is current directory.
#' @param download.source.binary [\code{logical(1)}]\cr    
#'   Should source / binary files of the implementation also be downloaded?
#'   They will also be stored in \code{dir}.
#'   Default is \code{TRUE}.
#' @template arg_showinfo
#' @param clean.up [logical(1)]\cr
#'   Delete implementation xml file at the end?
#'   Default is \code{TRUE}.
#' @return [\code{\link{OpenMLImplementation}}].
#' @export

downloadOpenMLImplementation = function(id, dir = getwd(), download.source.binary = TRUE, 
  show.info = getOpenMLOption("show.info"), clean.up = TRUE) {
  
  id = asInt(id)
  assertDirectory(dir)
  assertFlag(download.source.binary)
  assertFlag(show.info)
  assertFlag(clean.up)
  
  fn.impl.xml = file.path(dir, sprintf("flow_%i.xml", id))  
  
  on.exit({
    if (clean.up)
      unlink(fn.impl.xml)
    if (show.info)
      messagef("Intermediate Flow XML has been removed.")
  })
  
  downloadAPICallFile(api.fun = "openml.implementation.get", file = fn.impl.xml, implementation_id = id, 
    show.info = show.info)  
  impl = parseOpenMLImplementation(fn.impl.xml)
  
  if (download.source.binary) {
    if (!is.na(impl$source.url)) {
      if (show.info)
        messagef("Downloading implementation source file from URL:\n%s.", impl$source.url)
      format = rev(strsplit(rev(strsplit(impl$source.url, "/")[[1]])[1], "[.]")[[1]])[1]
      fn.impl.src = sprintf("%s(%s)_source.%s", impl$name, impl$version, format)
      fn.impl.src = file.path(dir, fn.impl.src)
      download.file(url = impl$source.url, destfile = fn.impl.src, quiet = !show.info, mode = "wb")
    } 
    if (!is.na(impl$binary.url)) {
      if (show.info)
        messagef("Downloading implementation binary file.")
      fn.impl.bin = sprintf("%s(%s)_binary", impl$name, impl$version)
      fn.impl.bin = file.path(dir, fn.impl.bin)
      download.file(url = impl$binary.url, destfile = fn.impl.bin, mode = "wb")
    }
  }
  return(impl)
}

parseOpenMLImplementation = function(file) {
  assertFile(file)
  doc = parseXMLResponse(file, "Getting implementation", "implementation")
  args = list()
  args[["id"]] = xmlRValI(doc, "/oml:implementation/oml:id")
  args[["name"]] = xmlRValS(doc, "/oml:implementation/oml:name")
  args[["version"]] = xmlRValS(doc, "/oml:implementation/oml:version")
  args[["external.version"]] = xmlOValS(doc, "/oml:implementation/oml:external_version")
  args[["description"]] = xmlRValS(doc, "/oml:implementation/oml:description")
  args[["creator"]] = xmlOValsMultNsS(doc, "/oml:implementation/oml:creator")
  args[["contributor"]] = xmlOValsMultNsS(doc, "/oml:implementation/oml:contributor")
  args[["upload.date"]] = xmlRValS(doc, "/oml:implementation/oml:upload_date")
  args[["licence"]] = xmlOValS(doc, "/oml:implementation/oml:licence")
  args[["language"]] = xmlOValS(doc, "/oml:implementation/oml:language")
  args[["full.description"]] = xmlOValS(doc, "/oml:implementation/oml:full_description")
  args[["installation.notes"]] = xmlOValS(doc, "/oml:implementation/oml:installation_notes")
  args[["dependencies"]] = xmlOValS(doc, "/oml:implementation/oml:dependencies")
  args[["bibliographical.reference"]] = parseOpenMLBibRef(doc)
  #args[["implements"]] = xmlOValS(doc, "/oml:implementation/oml:implements")
  args[["parameter"]] = parseOpenMLParameters(doc)  
  
  ## components section
  comp_ns = getNodeSet(doc, "/oml:implementation/oml:component/oml:implementation")
  
  comp = vector("list", length = length(comp_ns))
  for (i in seq_along(comp_ns)){
    file2 = tempfile()
    saveXML(comp_ns[[i]], file = file2)
    comp[[i]] = parseOpenMLImplementation(file2)
    unlink(file2)
  }
  args[["components"]] = comp

  args[["collection.date"]] = xmlOValS(doc, "/oml:implementation/oml:collection_date")
  args[["source.url"]] = xmlOValS(doc, "/oml:implementation/oml:source_url")
  args[["binary.url"]] = xmlOValS(doc, "/oml:implementation/oml:binary_url")
  args[["source.format"]] = xmlOValS(doc, "/oml:implementation/oml:source_format")
  args[["binary.format"]] = xmlOValS(doc, "/oml:implementation/oml:binary_format")
  args[["source.md5"]] = xmlOValS(doc, "/oml:implementation/oml:source_md5")
  args[["binary.md5"]] = xmlOValS(doc, "/oml:implementation/oml:binary_md5")

  impl = do.call(makeOpenMLImplementation, args)

  return(impl)
}

parseOpenMLParameters = function(doc) {  
  path = "/oml:implementation/oml:parameter"
  
  ns = getNodeSet(doc, path)
  nr.pars = length(ns)
  
  par.names = xmlValsMultNsS(doc, sprintf("%s/oml:name", path))
  par.types = xmlOValsMultNsSPara(doc, sprintf("%s/oml:data_type", path), exp.length = nr.pars)
  par.defs = xmlOValsMultNsSPara(doc, sprintf("%s/oml:default_value", path), exp.length = nr.pars)
  par.descs = xmlOValsMultNsSPara(doc, sprintf("%s/oml:description", path), exp.length = nr.pars)
  par.rec.range = xmlOValsMultNsSPara(doc, sprintf("%s/oml:recommendedRange", path), exp.length = nr.pars)
  
  par = vector("list", length(par.names))
  for (i in seq_along(par)) {
    par[[i]] = makeOpenMLImplementationParameter(name = par.names[i], data.type = par.types[i], 
      default.value = par.defs[i], description = par.descs[i], recommended.range = par.rec.range[i])
  }
  return(par)
}

parseOpenMLBibRef = function(doc) {  
  path = "/oml:implementation/oml:bibliographical_reference"
  
  ns = getNodeSet(doc, path)
  
  bib.citation = xmlValsMultNsS(doc, sprintf("%s/oml:citation", path))
  bib.url = xmlValsMultNsS(doc, sprintf("%s/oml:url", path))
  
  bib = vector("list", length(bib.citation))
  for (i in seq_along(bib.citation)) {
    bib[[i]] = makeOpenMLBibRef(bib.citation[i], bib.url[i])
  }
  if (length(bib) > 0)
    return(bib)
  else
    return(NULL)
}
