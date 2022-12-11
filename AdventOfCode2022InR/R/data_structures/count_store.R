
modules::export("empty_count_store")
empty_count_store <- function() {
  rlang::new_environment()
}


modules::export("count")
count <- function(count_store, item) {
  rlang::env_get(
    env = count_store,
    nm = item,
    default = 0
  )
}


set_count <- function(count_store, item, value) {
  if (value == 0) {
    rlang::env_unbind(
      env = count_store,
      nms = item
    )
  } else {
    rlang::env_poke(
      env = count_store,
      nm = item,
      value = value
    )
  }
  count_store
}


modules::export("add")
add <- function(count_store, item) {
  set_count(
    count_store = count_store,
    item = item,
    value = count(count_store, item) + 1
  )
}


modules::export("remove")
remove <- function(count_store, item) {
  if (!rlang::env_has(env = count_store, nms = item)) {
    cli::cli_abort(c("X" = "There is no item {item} in the count store that could be removed."))
  } else {
    set_count(
      count_store = count_store,
      item = item,
      value = count(count_store, item) - 1
    )
  }
  count_store
}


modules::export("max_count")
max_count <- function(count_store) {
  bound_items <- rlang::env_names(count_store)
  bound_values <- rlang::env_get_list(
    env = count_store,
    nms = bound_items,
    default = 0
  )
  max(unlist(bound_values))
}


modules::export("items_with_non_zero_count")
items_with_non_zero_count <- function(count_store) {
  rlang::env_names(count_store)
} 
