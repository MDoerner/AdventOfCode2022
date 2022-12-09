


#' Parsed Input to Day8 of Advent Of Code 2022
#'
#' @param input_string Input string to day 8 of AdventOfCode
#'
#' @return A tibble representing a map of tree heights, each row representing a tree   
#' @importFrom magrittr %>%
#' @importFrom rlang .data
parse_day8_input <- function(input_string) {
  input_string %>%
    to_character_grid() %>%
    dplyr::mutate(
      Height = readr::parse_integer(.data$Value)
    ) %>%
    dplyr::select("X", "Y", "Height")
}


tree_map_with_visibility <- function(tree_map) {
  tree_map %>%
    dplyr::group_by(.data$Y) %>%
    dplyr::arrange(-.data$X, .by_group = TRUE) %>%
    dplyr::mutate(
      MaxRightHeight = dplyr::lag(cummax(.data$Height), n = 1, default = -1)
    ) %>%
    dplyr::arrange(.data$X, .by_group = TRUE) %>%
    dplyr::mutate(
      MaxLeftHeight = dplyr::lag(cummax(.data$Height), n = 1, default = -1)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$X) %>%
    dplyr::arrange(-.data$Y, .by_group = TRUE) %>%
    dplyr::mutate(
      MaxBelowHeight = dplyr::lag(cummax(.data$Height), n = 1, default = -1)
    ) %>% 
    dplyr::arrange(.data$Y, .by_group = TRUE) %>%
    dplyr::mutate(
      MaxAboveHeight = dplyr::lag(cummax(.data$Height), n = 1, default = -1)
    ) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(
      VisibleFromAbove = .data$Height > .data$MaxAboveHeight,
      VisibleFromBelow = .data$Height > .data$MaxBelowHeight,
      VisibleFromLeft = .data$Height > .data$MaxLeftHeight,
      VisibleFromRight = .data$Height > .data$MaxRightHeight
    ) %>%
    dplyr::mutate(
      Visible = .data$VisibleFromAbove | 
        .data$VisibleFromBelow | 
        .data$VisibleFromLeft | 
        .data$VisibleFromRight
    ) %>%
    dplyr::select(c(
      "X", 
      "Y", 
      "Height", 
      "VisibleFromAbove", 
      "VisibleFromBelow", 
      "VisibleFromLeft", 
      "VisibleFromRight", 
      "Visible"
      )
    )
}


#' Solution to Day8 Part1 of Advent of Code 2022
#'
#' @param tree_map  A tibble representing a map of tree heights, each row representing a tree
#'
#' @return The number of visible trees on the map
#' @importFrom magrittr %>%
#' @importFrom rlang .data
solve_day8_part1 <- function(tree_map) {
  tree_map %>%
    tree_map_with_visibility %>%
    dplyr::filter(.data$Visible) %>%
    nrow() %>%
    as.character()
}


## Distances

## coords needs to be orderes appropriately and height must have a matching order
front_view_distance_wo_visibility <- function(coords, height) {
  possible_heights <- 0:9
  indices <- seq_along(coords)
  distances <- vector(mode = "integer", length = length(coords))
  greater_or_equal_height_last_seen_at <- rep(1, times = length(possible_heights))
  for (index in indices) {
    current_height <- height[[index]]
    current_coord <- coords[[index]]
    distances[[index]] <- current_coord - greater_or_equal_height_last_seen_at[[current_height + 1]]
    greater_or_equal_height_last_seen_at[possible_heights <= current_height] <- current_coord
  }
  distances
}

## coords needs to be orderes appropriately and height must have a matching order
back_view_distance_wo_visibility <- function(coords, height) {
  possible_heights <- 0:9
  indices <- rev(seq_along(coords))
  last_coord <- coords[[length(coords)]]
  distances <- vector(mode = "integer", length = length(coords))
  greater_or_equal_height_last_seen_at <- rep(last_coord, times = length(possible_heights))
  for (index in indices) {
    current_height <- height[[index]]
    current_coord <- coords[[index]]
    distances[[index]] <- greater_or_equal_height_last_seen_at[[current_height + 1]] - current_coord
    greater_or_equal_height_last_seen_at[possible_heights <= current_height] <- current_coord
  }
  distances
}

## coords needs to be orderes appropriately and height must have a matching order
front_view_distance <- function(coords, height, visible_from_front){
  dplyr::if_else(
    condition = visible_from_front,
    true = coords - 1,
    false = front_view_distance_wo_visibility(coords, height)
  )
}

## coords needs to be orderes appropriately and height must have a matching order
back_view_distance <- function(coords, height, visible_from_back){
  dplyr::if_else(
    condition = visible_from_back,
    true = coords[[length(coords)]] - coords,
    false = back_view_distance_wo_visibility(coords, height)
  )
}

top_view_distance <- function(x, y, height, visible_from_above, tree_map){
  if (visible_from_above) {
    y - 1
  } else {
    last_visible_tree_y <- tree_map %>%
      dplyr::filter(.data$X == x & .data$Y < y & .data$Height >= height) %>%
      dplyr::pull(.data$Y) %>%
      max()
    y - last_visible_tree_y
  }
}

down_view_distance <- function(x, y, height, visible_from_below, tree_map){
  if (visible_from_below) {
    max(tree_map$Y) - y
  } else {
    last_visible_tree_y <- tree_map %>%
      dplyr::filter(.data$X == x & .data$Y > y & .data$Height >= height) %>%
      dplyr::pull(.data$Y) %>%
      min()
    last_visible_tree_y - y
  }
}

up_and_down_distances_for_column <- function(column_map) {
  column_map %>%
    dplyr::mutate(
      TopViewDistance = front_view_distance(
        .data$Y,
        .data$Height,
        .data$VisibleFromAbove
      ),
      DownViewDistance = back_view_distance(
        .data$Y, 
        .data$Height, 
        .data$VisibleFromBelow
      )
    )
}

add_up_and_down_distances <- function(tree_map_with_visi) {
  column_maps <- tree_map_with_visi %>%
    dplyr::group_by(.data$X) %>%
    dplyr::group_split()
  column_maps %>%
    purrr::map(up_and_down_distances_for_column) %>%
    dplyr::bind_rows()
}

left_and_right_distances_for_row <- function(row_map) {
  row_map %>%
    dplyr::arrange(.data$X) %>%
    dplyr::mutate(
      LeftViewDistance = front_view_distance(
        .data$X,
        .data$Height,
        .data$VisibleFromLeft
      ),
      RightViewDistance = back_view_distance(
        .data$X, 
        .data$Height, 
        .data$VisibleFromRight
      )
    )
}

add_left_and_right_distances <- function(tree_map_with_visi) {
  row_maps <- tree_map_with_visi %>%
    dplyr::group_by(.data$Y) %>%
    dplyr::group_split()
  row_maps %>%
    purrr::map(left_and_right_distances_for_row) %>%
    dplyr::bind_rows()
}



tree_map_with_view_distances_and_score <- function(tree_map_with_visi){
  tree_map_with_visi %>%
    add_up_and_down_distances %>%
    add_left_and_right_distances %>%
    dplyr::mutate(
          ScenicScore = .data$TopViewDistance * .data$DownViewDistance * .data$LeftViewDistance * .data$RightViewDistance
        )
}


# TODO: Optimize this; it takes way too long on the actual input

#' Solution to Day8 Part2 of Advent of Code 2022
#'
#' @param tree_map  A tibble representing a map of tree heights, each row representing a tree
#'
#' @return The number of visible trees on the map
#' @importFrom magrittr %>%
#' @importFrom rlang .data
solve_day8_part2 <- function(tree_map) {
  tree_map %>%
    tree_map_with_visibility() %>%
    tree_map_with_view_distances_and_score() %>%
    dplyr::pull(.data$ScenicScore) %>%
    max() %>%
    as.character()
}
