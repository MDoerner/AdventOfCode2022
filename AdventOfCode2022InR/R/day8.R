


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

left_view_distance <- function(x, y, height, visible_from_left, tree_map){
  if (visible_from_left) {
    x - 1
  } else {
    last_visible_tree_x <- tree_map %>%
      dplyr::filter(.data$Y == y & .data$X < x & .data$Height >= height) %>%
      dplyr::pull(.data$X) %>%
      max()
    x - last_visible_tree_x
  }
}

right_view_distance <- function(x, y, height, visible_from_right, tree_map){
  if (visible_from_right) {
    max(tree_map$X) - x
  } else {
    last_visible_tree_x <- tree_map %>%
      dplyr::filter(.data$Y == y & .data$X > x & .data$Height >= height) %>%
      dplyr::pull(.data$X) %>%
      min()
    last_visible_tree_x - x
  }
}

up_and_down_distances_for_column <- function(column_map) {
  top_view <- purrr::partial(top_view_distance, tree_map = column_map)
  down_view <- purrr::partial(down_view_distance, tree_map = column_map)
  column_map %>%
    dplyr::mutate(
      TopViewDistance = purrr::pmap_dbl(
        .l = list(.data$X, .data$Y, .data$Height, .data$VisibleFromAbove),
        .f = top_view
      ),
      DownViewDistance = purrr::pmap_dbl(
        .l = list(.data$X, .data$Y, .data$Height, .data$VisibleFromBelow),
        .f = down_view
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
  left_view <- purrr::partial(left_view_distance, tree_map = row_map)
  right_view <- purrr::partial(right_view_distance, tree_map = row_map)
  row_map %>%
    dplyr::mutate(
      LeftViewDistance = purrr::pmap_dbl(
        .l = list(.data$X, .data$Y, .data$Height, .data$VisibleFromLeft),
        .f = left_view
      ),
      RightViewDistance = purrr::pmap_dbl(
        .l = list(.data$X, .data$Y, .data$Height, .data$VisibleFromRight),
        .f = right_view
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
    
  
  
  # top_view <- purrr::partial(top_view_distance, tree_map = tree_map_with_visi)
  # down_view <- purrr::partial(down_view_distance, tree_map = tree_map_with_visi)
  # left_view <- purrr::partial(left_view_distance, tree_map = tree_map_with_visi)
  # right_view <- purrr::partial(right_view_distance, tree_map = tree_map_with_visi)
  # tree_map_with_visi %>%
  #   dplyr::mutate(
  #     TopViewDistance = purrr::pmap_dbl(
  #       .l = list(.data$X, .data$Y, .data$Height, .data$VisibleFromAbove),
  #       .f = top_view
  #     ),
  #     DownViewDistance = purrr::pmap_dbl(
  #       .l = list(.data$X, .data$Y, .data$Height, .data$VisibleFromBelow),
  #       .f = down_view
  #     ),
  #     LeftViewDistance = purrr::pmap_dbl(
  #       .l = list(.data$X, .data$Y, .data$Height, .data$VisibleFromLeft),
  #       .f = left_view
  #     ),
  #     RightViewDistance = purrr::pmap_dbl(
  #       .l = list(.data$X, .data$Y, .data$Height, .data$VisibleFromRight),
  #       .f = right_view
  #     )
  #   ) %>%
  #   dplyr::mutate(
  #     ScenicScore = .data$TopViewDistance * .data$DownViewDistance * .data$LeftViewDistance * .data$RightViewDistance
  #   )
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
