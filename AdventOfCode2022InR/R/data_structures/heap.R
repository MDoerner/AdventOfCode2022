
## Simple Heap data structure

# We use a hash map implementation to store the heap because R
# copies lists on modify, which leads to too high complexity. 
hashmap <- modules::use("R/data_structures/hashmap.R")


modules::export("empty_heap")
empty_heap <- function() {
  list(
    min = NA,
    heap = hashmap$empty_hashmap()
  )
}

modules::export("get_min")
get_min <- function(heap) {
  if (is_empty(heap)) {
    NA
  } else {
    heap$min$key
  }
}

modules::export("is_empty")
is_empty <- function(heap) {
  length(heap$min) == 1L && is.na(heap$min)
}

modules::export("is_heap")
is_heap <- function(heap) {
  root_key <- heap$min$key
  is_heap_node(heap, root_key)
}

is_heap_node <- function(heap, key) {
  min_key <- min_node_key(heap, key)
  if (min_key != key) {
    return(key)
  }
  
  node <- get_node(heap, key)
  if (!is.na(node$left_key)) {
    left_ok <- is_heap_node(heap, node$left_key)
    if (!is.logical(left_ok) || is.na(left_ok) || !left_ok) {
      return(left_ok)
    }
  }
  if (!is.na(node$right_key)) {
    right_ok <- is_heap_node(heap, node$right_key)
    if (!is.logical(right_ok) || is.na(right_ok) || !right_ok) {
      return(right_ok)
    }
  }
  
  TRUE
}

modules::export("has_consistent_sizes")
has_consistent_sizes <- function(heap) {
  root_key <- heap$min$key
  is_consistent_size_node(heap, root_key)
}

is_consistent_size_node <- function(heap, key) {
  node <- get_node(heap, key)
  left_size <- if (is.na(node$left_key)) {
    0L
  } else {
    left <- get_node(heap, node$left_key)
    left$size
  }
  right_size <- if (is.na(node$right_key)) {
    0L
  } else {
    right <- get_node(heap, node$right_key)
    right$size
  }
  if (node$size != 1L + left_size + right_size) {
    return(key)
  }
  
  if (!is.na(node$left_key)) {
    left_ok <- is_consistent_size_node(heap, node$left_key)
    if (!is.logical(left_ok) || is.na(left_ok) || !left_ok) {
      return(left_ok)
    }
  }
  if (!is.na(node$right_key)) {
    right_ok <- is_consistent_size_node(heap, node$right_key)
    if (!is.logical(right_ok) || is.na(right_ok) || !right_ok) {
      return(right_ok)
    }
  } 
  
  TRUE
}

modules::export("debug_info")
debug_info <- function(heap) {
  list(
    contents = hashmap$as_list(heap$heap),
    heap_property = is_heap(heap),
    consistent_sizes = has_consistent_sizes(heap),
    min = heap$min
  )
}

is_root <- function(node) {
  is.na(node$parent_key)
}

get_node <- function(heap, key) {
  if (is.na(key)) {
    NA
  } else {
    hashmap$get(heap$heap, key)
  }
}

parent <- function(heap, node) {
  if (is_root(node)) {
    NA
  } else {
    get_node(heap, node$parent_key)
  }
}

set_node <- function(heap, key, node) {
  heap$heap <- hashmap$set(heap$heap, key, node)
  heap
}

remove_node <- function(heap, key) {
  heap$heap <- hashmap$remove(heap$heap, key)
  heap
}


swap_with_parent <- function(heap, key) {
  current_key <- key
  current_node <- get_node(heap, current_key)
  parent_key <- current_node$parent_key
  parent_node <- get_node(heap, parent_key)
  other_child_key <- if (!is.na(parent_node$left_key) && parent_node$left_key == current_key) {
    parent_node$right_key
  } else {
    parent_node$left_key
  }
  
  # Rewire pointers
  parent_node$left_key <- current_node$left_key
  parent_node$right_key <- current_node$right_key
  # The order of children does not matter.
  current_node$right_key <- other_child_key
  current_node$left_key <- parent_key
  current_node$parent_key <- parent_node$parent_key
  parent_node$parent_key <- current_key
  
  # Swap sizes
  parent_size <- parent_node$size
  parent_node$size <- current_node$size
  current_node$size <- parent_size
  
  heap <- set_node(heap, parent_key, parent_node)
  heap <- set_node(heap, current_key, current_node)
  
  # Align child parent pointers
  if (!is.na(other_child_key)) {
    other_child <- get_node(heap, other_child_key)
    other_child$parent_key <- current_key
    heap <- set_node(heap, other_child_key, other_child)
  }
  
  if (!is.na(parent_node$left_key)) {
    left_child <- get_node(heap, parent_node$left_key)
    left_child$parent_key <- parent_key
    heap <- set_node(heap, parent_node$left_key, left_child)
  }
  
  if (!is.na(parent_node$right_key)) {
    right_child <- get_node(heap, parent_node$right_key)
    right_child$parent_key <- parent_key
    heap <- set_node(heap, parent_node$right_key, right_child)
  }
  
  # align parent_pointer
  if (!is.na(current_node$parent_key)) {
    new_parent <- get_node(heap, current_node$parent_key)
    if (!is.na(new_parent$left_key) && new_parent$left_key == parent_key) {
      new_parent$left_key <- current_key
    } else {
      new_parent$right_key <- current_key
    }
    heap <- set_node(heap, current_node$parent_key, new_parent)
  }
  
  heap
}


modules::export("decrease_priority")
decrease_priority <- function(heap, key, new_priority) {
  current_node <- get_node(heap, key)
  
  # update priority
  current_node$priority <- new_priority 
  heap <- set_node(heap, key, current_node)
  
  # bubble up
  while (!is_root(current_node) && parent(heap, current_node)$priority > new_priority) {
    heap <- swap_with_parent(heap, key)
    current_node <- get_node(heap, key)
  }
  
  heap$min <- if (is_root(current_node)) {
    list(
      key = key,
      priority = new_priority
    )
  } else {
    heap$min
  }
  
  heap
}

insert_at_end <- function(non_empty_heap, key, priority) {
  heap <- non_empty_heap
  current_key <- heap$min$key
  current_node <- get_node(heap, current_key)
  
  while (!is.na(current_node$left_key) && !is.na(current_node$right_key)) {
    # update size
    current_node$size <- current_node$size + 1L
    heap <- set_node(heap, current_key, current_node)
    
    # descend
    left <- get_node(heap, current_node$left_key)
    right <- get_node(heap, current_node$right_key)
    if (left$size <= right$size) {
      current_key <- current_node$left_key
      current_node <- left
    } else {
      current_key <- current_node$right_key
      current_node <- right
    }
  }
  
  # add leaf
  new_node <- list(
    priority = priority,
    size = 1L,
    parent_key = current_key,
    left_key = NA,
    right_key = NA
  )
  heap <- set_node(heap, key, new_node)
  
  # update current node
  if (is.na(current_node$left_key)) {
    current_node$left_key <- key
  } else {
    current_node$right_key <- key
  }
  current_node$size <- current_node$size + 1L
  heap <- set_node(heap, current_key, current_node)
  
  heap
}

modules::export("insert")
insert <- function(heap, key, priority) {
  if (is_empty(heap)) {
    heap$min <- list(
      key = key,
      priority = priority
    )
    new_node <- list(
      priority = priority,
      size = 1L,
      parent_key = NA,
      left_key = NA,
      right_key = NA
    )
    heap <- set_node(
      heap, 
      key,
      new_node
    )
    
    return(heap)
  }
  
  heap <- insert_at_end(heap, key, priority)
  heap <- decrease_priority(heap, key, priority)
  
  heap
}

replace_top_with_last_node <- function(non_empty_heap) {
  heap <- non_empty_heap
  root_key <- heap$min$key
  current_key <- root_key
  current_node <- get_node(heap, current_key)
  
  if (is.na(current_node$left_key) || is.na(current_node$right_key)) {
    # there is only the root.
    return(heap) 
  }
  
  while (!is.na(current_node$left_key) || !is.na(current_node$right_key)) {
    # update size
    current_node$size <- current_node$size - 1L
    heap <- set_node(heap, current_key, current_node)
    
    # descend to last node
    current_key <- if (is.na(current_node$left_key)) {
      current_node$right_key
    } else if (is.na(current_node$right_key)) {
      current_node$left_key
    } else {
      left <- get_node(heap, current_node$left_key)
      right <- get_node(heap, current_node$right_key)
      if (left$size > right$size) {
        current_key <- current_node$left_key
      } else {
        current_key <- current_node$right_key
      }
    }
    current_node <- get_node(heap, current_key)
  }
  
  # detach last node
  parent_node <- parent(heap, current_node)
  if (!is.na(parent_node$right_key) && parent_node$right_key == current_key) {
    parent_node$right_key <- NA
  } else {
    parent_node$left_key <- NA
  }
  heap <- set_node(heap, current_node$parent, parent_node)
  
  # wire up as top node
  root_node <- get_node(heap, root_key)
  current_node$left_key <- root_node$left_key
  current_node$right_key <- root_node$right_key
  current_node$size <- root_node$size
  current_node$parent_key <- root_node$parent_key
  heap <- set_node(heap, current_key, current_node)
  
  if (!is.na(current_node$left_key)) {
    left_child <- get_node(heap, current_node$left_key)
    left_child$parent_key <- current_key
    heap <- set_node(heap, current_node$left_key, left_child)
  }
  
  if (!is.na(current_node$right_key)) {
    right_child <- get_node(heap, current_node$right_key)
    right_child$parent_key <- current_key
    heap <- set_node(heap, current_node$right_key, right_child)
  }
  
  heap <- remove_node(heap, root_key)
  
  heap$min <- list(
    key = current_key,
    priority = current_node$priority
  )
  
  heap
}

min_node_key <- function(heap, key){
  current_node <- get_node(heap, key)
  min_key <- key
  min_priority <- current_node$priority
  if (!is.na(current_node$left_key)) {
    left <- get_node(heap, current_node$left_key)
    if (left$priority < min_priority) {
      min_key <- current_node$left_key
      min_priority <- left$priority
    }
  }
  if (!is.na(current_node$right_key)) {
    right <- get_node(heap, current_node$right_key)
    if (right$priority < min_priority) {
      min_key <- current_node$right_key
      min_priority <- right$priority
    }
  }
  min_key
}

heapify <- function(heap, key) {
  min_key <- min_node_key(heap, key)
  
  root_node = heap$min$key
  if (min_key != key && root_node == key) {
    # the root node switches
    min_node <- get_node(heap, min_key)
    heap$min <- list(
      key = min_key,
      priority = min_node$priority
    )
  }
  
  while (min_key != key) {
    heap <- swap_with_parent(heap, min_key)
    min_key <- min_node_key(heap, key)
  }
  
  heap
}

modules::export("remove_min")
remove_min <- function(heap) {
  if (is_empty(heap)) {
    cli::cli_abort(c("x" = "Cannot remove the min from an empty heap."))
  }
  
  root_key <- heap$min$key
  root_node <- get_node(heap, root_key)
  if (root_node$size == 1L) {
    heap <- remove_node(heap, root_key)
    heap$min <- NA
    
    return(heap)
  }
  
  if (is.na(root_node$left_key)) {
    new_root_key <- root_node$right_key
    new_root_node <- get_node(heap, new_root_key)
    new_root_node$parent_key <- NA
    heap <- set_node(heap, new_root_key, new_root_node)
    
    heap <- remove_node(heap, root_key)
    
    heap$min <- list(
      key = new_root_key,
      priority = new_root_node$priority
    )
    
    return(heap)
  }
  
  if (is.na(root_node$right_key)) {
    new_root_key <- root_node$left_key
    new_root_node <- get_node(heap, new_root_key)
    new_root_node$parent_key <- NA
    heap <- set_node(heap, new_root_key, new_root_node)
    
    heap <- remove_node(heap, root_key)
    
    heap$min <- list(
      key = new_root_key,
      priority = new_root_node$priority
    )
    
    return(heap)
  }
  
  heap <- replace_top_with_last_node(heap)
  root_key <- heap$min$key
  
  heap <- heapify(heap, root_key)
  
  
  heap
}


