#' Download an OpenML implementation from the OpenML server through a server API call.
#'
#' Retrieves an implementation for a given id.
#'
#' @param id [\code{integer(1)}]\cr
#'   The implementation id.
#' @param download.source.binary [\code{logical(1)}]\cr
#'   Should source / binary files of the implementation also be downloaded?
#'   They will also be stored in \code{dir}.
#'   Default is \code{TRUE}.
#' @template arg_ignore.cache
#' @template arg_showinfo
#' @return [\code{\link{OpenMLImplementation}}].
#' @export
downloadOpenMLImplementation = function(id, download.source.binary = TRUE, ignore.cache = FALSE, show.info = getOpenMLOption("show.info")) {
  id = asInt(id)
  assertFlag(download.source.binary)
  assertFlag(show.info)

  url = getAPIURL("openml.implementation.get", implementation_id = id)
  doc = parseXMLResponse(url, "Getting implementation", "implementation")
  impl = parseOpenMLImplementation(doc)

  if (download.source.binary) {
    if (!is.na(impl$source.url)) {
      # FIXME: discuss this
      # if (show.info)
      #   messagef("Downloading implementation source file from URL:\n%s.", impl$source.url)
      # format = rev(strsplit(rev(strsplit(impl$source.url, "/")[[1L]])[1L], "[.]")[[1L]])[1L]
      # fn.impl.src = sprintf("%s(%s)_source.%s", impl$name, impl$version, format)
      # fn.impl.src = file.path(dir, fn.impl.src)
      # download.file(url = impl$source.url, destfile = fn.impl.src, quiet = !show.info, mode = "wb")
    }
    if (!is.na(impl$binary.url)) {
      # if (show.info)
      #   messagef("Downloading implementation binary file.")
      # fn.impl.bin = sprintf("%s(%s)_binary", impl$name, impl$version)
      # fn.impl.bin = file.path(dir, fn.impl.bin)
      # download.file(url = impl$binary.url, destfile = fn.impl.bin, mode = "wb")
    }
  }
  return(impl)
}

parseOpenMLImplementation = function(doc) {
  args = filterNull(list(
    id = xmlRValI(doc, "/oml:implementation/oml:id"),
    name = xmlRValS(doc, "/oml:implementation/oml:name"),
    version = xmlRValS(doc, "/oml:implementation/oml:version"),
    external.version = xmlOValS(doc, "/oml:implementation/oml:external_version"),
    description = xmlRValS(doc, "/oml:implementation/oml:description"),
    creator = xmlOValsMultNsS(doc, "/oml:implementation/oml:creator"),
    contributor = xmlOValsMultNsS(doc, "/oml:implementation/oml:contributor"),
    upload.date = xmlRValS(doc, "/oml:implementation/oml:upload_date"),
    licence = xmlOValS(doc, "/oml:implementation/oml:licence"),
    language = xmlOValS(doc, "/oml:implementation/oml:language"),
    full.description = xmlOValS(doc, "/oml:implementation/oml:full_description"),
    installation.notes = xmlOValS(doc, "/oml:implementation/oml:installation_notes"),
    dependencies = xmlOValS(doc, "/oml:implementation/oml:dependencies"),
    bibliographical.reference = parseOMLBibRef(doc),
    parameter = parseOpenMLParameters(doc),
    collection.date = xmlOValS(doc, "/oml:implementation/oml:collection_date"),
    source.url = xmlOValS(doc, "/oml:implementation/oml:source_url"),
    binary.url = xmlOValS(doc, "/oml:implementation/oml:binary_url"),
    source.format = xmlOValS(doc, "/oml:implementation/oml:source_format"),
    binary.format = xmlOValS(doc, "/oml:implementation/oml:binary_format"),
    source.md5 = xmlOValS(doc, "/oml:implementation/oml:source_md5"),
    binary.md5 = xmlOValS(doc, "/oml:implementation/oml:binary_md5"),
    components = list()
  ))

  ## components section
  comp_ns = getNodeSet(doc, "/oml:implementation/oml:component/oml:implementation")
  comp = vector("list", length = length(comp_ns))
  for (i in seq_along(comp_ns)) {
    file2 = tempfile("components")
    saveXML(comp_ns[[i]], file = file2)
    comp[[i]] = parseOpenMLImplementation(file2)
    names(comp)[i] = xmlRValS(doc, paste0("/oml:implementation/oml:component[", i, "]/oml:identifier"))
    unlink(file2)
  }
  args[["components"]] = comp

  do.call(makeOpenMLImplementation, args)
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

parseOMLBibRef = function(doc) {
  path = "/oml:implementation/oml:bibliographical_reference"

  ns = getNodeSet(doc, path)

  bib.citation = xmlValsMultNsS(doc, sprintf("%s/oml:citation", path))
  bib.url = xmlValsMultNsS(doc, sprintf("%s/oml:url", path))

  # FIXME: Map()
  bib = vector("list", length(bib.citation))
  for (i in seq_along(bib.citation)) {
    bib[[i]] = makeOMLBibRef(bib.citation[i], bib.url[i])
  }
  if (length(bib) > 0L)
    return(bib)
  return(NULL)
}
