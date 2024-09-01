the_cache <- new.env(parent = emptyenv())

the_cache$info <- tibble::tribble(
  ~type,           ~key,
  "centerHistory", "",
  "sessionLength", "",
  "suppression",   "Student",
  "template",      "",
  "vacation",      "Student",
  "test",          ""
)

createCache <- function(type) {
  type <- match.cacheType(type)

  message("createCache(): processCache(empty) called for ", type)
  #processCache(empty, type) %>% saveRDS(cachePath(type))
  invisible(NULL)
}

readCache <- function(type) {
  type <- match.cacheType(type)
  path <- cachePath(type)

  if (!file.exists(path)) {
    message("readCache(): createCache() called for ", type)
    createCache(type)
    return("cache to be created")
  }

  message("readCache(): cleanCacheData() called for ", type)
  readRDS(path) %>%
  #cleanCache(type) %>%
  return()
}

saveCache <- function(data, type) {
  type <- match.cacheType(type)

  cache <- readCache(type)
  if (!identical(data, cache)) {
    saveRDS(data, cachePath(type))
    return(TRUE)
  }
  return(FALSE)
}

cachePath <- function(type) {
  type <- match.cacheType(type)

  the_cache$info %>%
    filter(.data$type == .env$type) %>%
    dplyr::pull("type") %>%
    paste0(".rds") %>%
    file.path(cacheDir(), .) %>%
    return()
}

cacheKey <- function(type) {
  type <- match.cacheType(type)

  the_cache$info %>%
    filter(.data$type == .env$type) %>%
    dplyr::pull("key") %>%
    return()
}

match.cacheType <- function(type) {
  return(match.arg(type, the_cache$info$type))
}
