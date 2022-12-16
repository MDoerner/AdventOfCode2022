
## Implementation of the Disjkstra algorithm

heap <- modules::use("R/data_structures/heap.R")
hashmap <- modules::use("R/data_structures/hashmap.R")

set_distance <- function(distances, point, distance) {
  hashmap$set(distances, point, distance)
}

get_distance <- function(distances, point) {
  hashmap$get(distances, point)
}

has_distance <- function(distances, point) {
  hashmap$has(distances, point)
}

remove_distance <- function(distances, point) {
  hashmap$remove(distances, point)
}

decrease_distance <- function(search_heap, point, old_distance, new_distance) {
  heap$decrease_priority(search_heap, point, new_distance)
}

insert <- function(search_heap, point, distance) {
  heap$insert(search_heap, point, distance)
}

get_closest_location <- function(search_heap) {
  heap$get_min(search_heap) 
}

remove_closest_location <- function(search_heap) {
  heap$remove_min(search_heap)
}

modules::export("shortest_path_distance")
shortest_path_distance <- function(start, end, connections_of) {
  end_condition <- function(current_location) all(current_location == end)
  shortest_path_distance_until(start, end_condition, connections_of)
}

modules::export("shortest_path_distance_until")
shortest_path_distance_until <- function(start, end_condition, connections_of) {
  distances <- hashmap$empty_hashmap()
  search_heap <- heap$empty_heap()
  set_distance(distances, start, 0L)
  
  current_location <- start
  while (!end_condition(current_location)) {
    current_distance_to_start <- get_distance(distances, current_location)
    connections <- connections_of(current_location)
    for (connection in connections) {
      distance_to_start <- current_distance_to_start + connection$distance
      if (!has_distance(distances, connection$destination)) {
        distances <- set_distance(distances, connection$destination, distance_to_start)
        search_heap <- insert(search_heap, connection$destination, distance_to_start)
      } else {
        current_destination_distance <- get_distance(distances, connection$destination)
        if (current_destination_distance > distance_to_start) {
          distances <- set_distance(distances, connection$destination, distance_to_start)
          search_heap <- decrease_distance(
            search_heap = search_heap, 
            point = connection$destination, 
            old_distance = current_destination_distance, 
            new_distance = distance_to_start
          )
        }
      }
    }
    current_location <- get_closest_location(search_heap)
    search_heap <- remove_closest_location(search_heap)
  }
  get_distance(distances, current_location)
}


modules::export("shortest_path_distances")
shortest_path_distances <- function(start, connections_of) {
  distances <- hashmap$empty_hashmap()
  search_heap <- heap$empty_heap()
  set_distance(distances, start, 0L)
  
  current_location <- start
  while (!is.null(current_location)) {
    current_distance_to_start <- get_distance(distances, current_location)
    connections <- connections_of(current_location)
    for (connection in connections) {
      distance_to_start <- current_distance_to_start + connection$distance
      if (!has_distance(distances, connection$destination)) {
        distances <- set_distance(distances, connection$destination, distance_to_start)
        search_heap <- insert(search_heap, connection$destination, distance_to_start)
      } else {
        current_destination_distance <- get_distance(distances, connection$destination)
        if (current_destination_distance > distance_to_start) {
          distances <- set_distance(distances, connection$destination, distance_to_start)
          search_heap <- decrease_distance(
            search_heap = search_heap, 
            point = connection$destination, 
            old_distance = current_destination_distance, 
            new_distance = distance_to_start
          )
        }
      }
    }
    if (!heap$is_empty(search_heap)) {
      current_location <- get_closest_location(search_heap)
      search_heap <- remove_closest_location(search_heap)
    } else {
      current_location <- NULL
    }
  }
  distances <- remove_distance(distances, start)
  hashmap$as_list(distances)
}


