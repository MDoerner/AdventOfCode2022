

get_empty_count_store <- function() {
  rlang::new_environment()
}

get_count <- function(count_store, item) {
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

add_to_count_store <- function(count_store, item) {
  set_count(
    count_store = count_store,
    item = item,
    value = get_count(count_store, item) + 1
  )
}

remove_from_count_store <- function(count_store, item) {
  if (!rlang::env_has(env = count_store, nms = item)) {
    cli::cli_abort(c("X" = "There is no item {item} in the count store that could be removed."))
  } else {
    set_count(
      count_store = count_store,
      item = item,
      value = get_count(count_store, item) - 1
    )
  }
  count_store
}

max_count <- function(count_store) {
  bound_items <- rlang::env_names(count_store)
  bound_values <- rlang::env_get_list(
    env = count_store,
    nms = bound_items,
    default = 0
  )
  max(unlist(bound_values))
}


items_with_non_zero_count <- function(count_store) {
  rlang::env_names(count_store)
} 
