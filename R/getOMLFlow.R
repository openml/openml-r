#' @title Download an OpenML flow.
#'
#' @description
#' Given an flow id, the corresponding \code{\link{OMLFlow}} is downloaded if
#' not already available in cache.
#'
#' @template arg_flow.id
#' @template arg_cache_only
#' @template arg_verbosity
#' @return [\code{\link{OMLFlow}}].
#' @export
getOMLFlow = function(flow.id, cache.only = FALSE, verbosity = NULL) {
  flow.id = asCount(flow.id)
  assertFlag(cache.only)

  f = findCachedFlow(flow.id)

  if (!f$flow.xml$found) {
    if (cache.only)
      stopf("Data set '%i' not found in cache with option 'cache.only'", did)
    flow.desc.contents = doAPICall(api.call = "flow", id = flow.id, file = f$flow.xml$path,
      verbosity = verbosity, method = "GET")
  } else {
    showInfo(verbosity, "Flow description found in cache.")
    flow.desc.contents = readLines(f$flow.xml$path)
  }

  doc = parseXMLResponse(flow.desc.contents, "Getting flow", "flow", as.text = TRUE)
  flow = parseOMLFlow(doc)

  # download source and/or binary files:
  downloadFileAndSavePath = function(flow, binOrSource, mode, verbosity) {
    url = flow[[sprintf("%s.url", binOrSource)]]
    if (!is.na(url)) {
      file = basename(url)
      f = findCachedFlow(flow$flow.id, file)[[1L]]
      flow[[sprintf("%s.path", binOrSource)]] = f$path
      if (f$found) {
        showInfo(verbosity, "File %s found in cache.", file)
      } else {
        showInfo(verbosity, "Downloading '%s' to '%s'", url, file)
        download.file(url, f$path, mode = mode, quiet = TRUE)
      }
    }
    return(flow)
  }

  flow = downloadFileAndSavePath(flow, "binary", "wb", verbosity)
  flow = downloadFileAndSavePath(flow, "source", "w", verbosity)

  # fix unserialize in downloaded R file (issue #49)
#   if(!is.na(flow$source.format) && flow$source.format == "R") {
#     source.file = readLines(flow$source.path)
#     fix.line = grepl("unserialize", source.file)
#     if(!grepl("charToRaw", source.file[fix.line])) {
#       source.file[fix.line] = gsub("unserialize", "unserialize(charToRaw", source.file[fix.line])
#       source.file[fix.line] = gsub("))", ")))", source.file[fix.line])
#       writeLines(source.file, flow$source.path)
#     }
#   }

  return(flow)
}

parseOMLFlow = function(doc) {
  args = filterNull(list(
    flow.id = xmlRValI(doc, "/oml:flow/oml:id"),
    uploader = xmlOValI(doc, "/oml:flow/oml:uploader"),
    name = xmlRValS(doc, "/oml:flow/oml:name"),
    version = xmlRValS(doc, "/oml:flow/oml:version"),
    external.version = xmlOValS(doc, "/oml:flow/oml:external_version"),
    description = xmlRValS(doc, "/oml:flow/oml:description"),
    creator = xmlOValsMultNsS(doc, "/oml:flow/oml:creator"),
    contributor = xmlOValsMultNsS(doc, "/oml:flow/oml:contributor"),
    upload.date = xmlRValS(doc, "/oml:flow/oml:upload_date"),
    licence = xmlOValS(doc, "/oml:flow/oml:licence"),
    language = xmlOValS(doc, "/oml:flow/oml:language"),
    full.description = xmlOValS(doc, "/oml:flow/oml:full_description"),
    installation.notes = xmlOValS(doc, "/oml:flow/oml:installation_notes"),
    dependencies = xmlOValS(doc, "/oml:flow/oml:dependencies"),
    bibliographical.reference = parseOMLBibRef(doc),
    implements = xmlOValS(doc, "/oml:flow/oml:implements"),
    parameters = parseOMLParameters(doc),
    qualities = parseOMLQualities(doc),
    tags = xmlOValsMultNsS(doc, "/oml:flow/oml:tag"),
    source.url = xmlOValS(doc, "/oml:flow/oml:source_url"),
    binary.url = xmlOValS(doc, "/oml:flow/oml:binary_url"),
    source.format = xmlOValS(doc, "/oml:flow/oml:source_format"),
    binary.format = xmlOValS(doc, "/oml:flow/oml:binary_format"),
    source.md5 = xmlOValS(doc, "/oml:flow/oml:source_md5"),
    binary.md5 = xmlOValS(doc, "/oml:flow/oml:binary_md5"),
    components = list()
  ))

  ## components section
  comp_ns = getNodeSet(doc, "/oml:flow/oml:component/oml:flow")
  comp = vector("list", length = length(comp_ns))
  for (i in seq_along(comp_ns)) {
    # save subcomponent temporarily on disk
    # FIXME: this is not very elegant
    file2 = tempfile("components")
    saveXML(comp_ns[[i]], file = file2)
    comp[[i]] = parseOMLFlow(parseXMLResponse(file2, type = "flow"))
    names(comp)[i] = xmlRValS(doc, paste0("/oml:flow/oml:component[", i, "]/oml:identifier"))
    unlink(file2)
  }
  args[["components"]] = comp

  do.call(makeOMLFlow, args)
}

parseOMLParameters = function(doc) {
  path = "/oml:flow/oml:parameter"

  ns = getNodeSet(doc, path)
  nr.pars = length(ns)

  par.names = xmlValsMultNsS(doc, sprintf("%s/oml:name", path))
  par.types = xmlOValsMultNsSPara(doc, sprintf("%s/oml:data_type", path), exp.length = nr.pars)
  par.defs = xmlOValsMultNsSPara(doc, sprintf("%s/oml:default_value", path), exp.length = nr.pars)
  par.descs = xmlOValsMultNsSPara(doc, sprintf("%s/oml:description", path), exp.length = nr.pars)
  par.rec.range = xmlOValsMultNsSPara(doc, sprintf("%s/oml:recommendedRange", path), exp.length = nr.pars)

  par = vector("list", length(par.names))
  for (i in seq_along(par)) {
    par[[i]] = makeOMLFlowParameter(name = par.names[i], data.type = par.types[i],
      default.value = par.defs[i], description = par.descs[i], recommended.range = par.rec.range[i])
  }
  return(par)
}

parseOMLBibRef = function(doc) {
  path = "/oml:flow/oml:bibliographical_reference"

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

parseOMLQualities = function(doc) {
  path = "/oml:flow/oml:quality"

  ns = getNodeSet(doc, path)

  name = xmlValsMultNsS(doc, sprintf("%s/oml:name", path))
  value = xmlValsMultNsS(doc, sprintf("%s/oml:value", path))

  # FIXME: Map()
  qualities = vector("list", length(name))
  for (i in seq_along(qualities)) {
    qualities[[i]] = makeOMLFlowQuality(name[i], value[i])
  }
  if (length(qualities) > 0L)
    return(qualities)
  return(NULL)
}
