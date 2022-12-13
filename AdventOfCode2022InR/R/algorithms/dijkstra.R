
## Implementation of the Disjkstra algorithm


# TODO: Replace by own implementation.
heap <- modules::module({
  
  modules::export("empty_heap")
  empty_heap <- function() {
    datastructures::fibonacci_heap("integer")
  }
  
  modules::export("get_min")
  get_min <- function(heap) {
    datastructures::peek(heap)[[1]]
  }
  
  modules::export("is_empty")
  is_empty <- function(heap) {
    datastructures::size(heap) == 0
  }
  
  modules::export("decrease_priority")
  decrease_priority <- function(heap, key, old_priority, new_priority) {
    handle <- datastructures::handle(heap, old_priority, key)
    datastructures::decrease_key(heap, old_priority, new_priority, handle[[1]]$handle)
    heap
  }
  
  modules::export("insert")
  insert <- function(heap, key, priority) {
    datastructures::insert(heap, priority, key)
    heap
  }
  
  modules::export("remove_min")
  remove_min <- function(heap) {
    datastructures::pop(heap)
    heap
  }
})

hashmap <- modules::use("R/data_structures/hashmap.R")
point_encoder <- modules::use("R/utility/point_utils.R")

set_distance <- function(distances, point, distance) {
  hashmap$set(distances, point_encoder$encode_point(point), distance)
}

get_distance <- function(distances, point) {
  hashmap$get(distances, point_encoder$encode_point(point))
}

has_distance <- function(distances, point) {
  hashmap$has(distances, point_encoder$encode_point(point))
}

decrease_distance <- function(search_heap, point, old_distance, new_distance) {
  heap$decrease_priority(search_heap, point_encoder$encode_point(point), old_distance, new_distance)
}

insert <- function(search_heap, point, distance) {
  heap$insert(search_heap, point_encoder$encode_point(point), distance)
}

get_closest_location <- function(search_heap) {
  point_encoder$decode_point(heap$get_min(search_heap)) 
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
        set_distance(distances, connection$destination, distance_to_start)
        insert(search_heap, connection$destination, distance_to_start)
      } else {
        current_destination_distance <- get_distance(distances, connection$destination)
        if (current_destination_distance > distance_to_start) {
          set_distance(distances, connection$destination, distance_to_start)
          decrease_distance(
            search_heap = search_heap, 
            point = connection$destination, 
            old_distance = current_destination_distance, 
            new_distance = distance_to_start
          )
        }
      }
    }
    current_location <- get_closest_location(search_heap)
    remove_closest_location(search_heap)
  }
  get_distance(distances, current_location)
}


