downloadXML = function(url, file, ignore.cache = FALSE, show.info = getOpenMLOption("show.info")) {
  file = file.path(getOpenMLOption("cache.dir"), file)
  if (!ignore.cache && file.exists(file)) {
    if (show.info)
      messagef("Retrieving file '%s' from cache", url)
    content = readLines(file)
  } else {
    if (show.info)
      messagef("Downloading '%s'", url)
    content = getURL(url)
    dn = dirname(file)
    if (!isDirectory(dn))
      dir.create(dn, recursive = TRUE)
    # con = gzfile(file, open = "w")
    con = file(file, open = "w")
    on.exit(close(con))
    writeLines(content, con = con)
  }

  content
}

downloadARFF = function(url, file, ignore.cache = FALSE, show.info = getOpenMLOption("show.info")) {
  file = file.path(getOpenMLOption("cache.dir"), file)
  if (!ignore.cache && file.exists(file)) {
    if (show.info)
      messagef("Retrieving file '%s' from cache", url)
  } else {
    dn = dirname(file)
    if (!isDirectory(dn))
      dir.create(dn, recursive = TRUE)
    if (show.info)
      messagef("Downloading '%s' -> %s", url, file)
    download.file(url, file, mode = "w", quiet = !show.info)
  }

  read.arff(file)
}
