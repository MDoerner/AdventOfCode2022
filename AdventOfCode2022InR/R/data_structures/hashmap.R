
## Hash Map implementation using environments


modules::export("empty_hashmap")
empty_hashmap <- function() {
  rlang::new_environment()
}

modules::export("set")
set <- function(hashmap, key, value) {
  rlang::env_poke(
    env = hashmap,
    nm = key,
    value = value
  )
  hashmap
}

modules::export("has")
has <- function(hashmap, key) {
  rlang::env_has(
    env = hashmap,
    nms = key
  )
}

modules::export("get_or_default")
get_or_default <- function(hashmap, key, default) {
  rlang::env_get(
    env = hashmap,
    nm = key,
    default = default
  )
}

modules::export("get")
get <- function(hashmap, key) {
  if (!has(hashmap, key)) {
    cli::cli_abort(c("x" = "There is no key '{key}' in the hashmap."))
  }
  get_or_default(hashmap, key, NA)
}

modules::export("remove")
remove <- function(hashmap, key) {
  if (!has(hashmap, key)) {
    cli::cli_abort(c("x" = "There is no key '{key}' in the hashmap to remove."))
  } 
  remove_if_present(hashmap, key)
}

modules::export("remove_if_present")
remove_if_present <- function(hashmap, key) {
  rlang::env_unbind(
    env = hashmap, 
    nms = key
  )
  hashmap
}

modules::export("keys")
keys <- function(hashmap) {
  rlang::env_names(hashmap)
}

modules::export("as_list")
as_list <- function(hashmap) {
  keys <- sort(keys(hashmap))
  values <- purrr::map(
    .x = keys, 
    .f = function(key) get(hashmap, key)
  )
  names(values) <- keys
  values
}






