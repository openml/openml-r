ImplementationParameter = function(name, data.type, default.value = NA_character_, 
  description = NA_character_) {
  
  assertString(name)
  assertString(data.type)
  assertString(default.value, na.ok = TRUE)
  assertString(description, na.ok = TRUE)
  
  makeS3Obj("ImplementationParameter", 
    name = name,
    data.type = data.type,
    default.value = default.value,
    description = description
 )
}

#' @export
print.ImplementationParameter = function(x, ...) {
  cat(x$name)
  if (!is.na(x$data.type)) cat(' : ', x$data.type)
  if (!is.na(x$default.value)) cat(' (default value = ', x$default.value, ' )')
  if (!is.na(x$description)) cat('\n   Description : ', x$description)
  cat('\n')
}
