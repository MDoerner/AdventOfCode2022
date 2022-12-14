
#' Module containing the implementation for Day13 of Advent Of Code 2022
#' @noRd
day13 <- modules::module(
  {
    parsing <- modules::use("R/utility/parsing_utils.R")
    
    find_next_non_match <- function(seq, start_index, is_match) {
      index <- start_index
      while (index <= length(seq) && is_match(seq[[index]])) {
        index <- index + 1
      } 
      if (index > length(seq)) {
        NA
      } else {
        index
      }
    }
    
    is_digit <- function(character) {
      character %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
    }
    
    parse_number <- function(char_sequence, index) {
      first_non_digit_index <- find_next_non_match(char_sequence, index, is_digit)
      number <- char_sequence[index:(first_non_digit_index - 1L)] %>%
        paste0(collapse = "") %>%
        readr::parse_integer()
      list(
        index = first_non_digit_index,
        value = number
      )
    }
    
    parse_list <- function(char_sequence, index) {
      result <- list()
      current_index <- index
      current_char <- char_sequence[[current_index]]
      while (current_char != "]") {
        if (current_char == "[") {
          new_list <- parse_list(char_sequence, current_index + 1L)
          result <- append(result, list(new_list$value))
          current_index <- new_list$index
        } else if (is_digit(current_char)) {
          new_number <- parse_number(char_sequence, current_index)
          result <- append(result, new_number$value)
          current_index <- new_number$index
        } else {
          current_index <- current_index + 1L
        }
        current_char <- char_sequence[[current_index]]
      }
      
      list(
        index = current_index + 1L,
        value = result
      )
    }
    
    parse_packet <- function(text) {
      char_sequence <- parsing$split_characters(text)
      # The first character is always an opening paren.
      parse_list(char_sequence, 2L)$value
    }
    
    parse_pair <- function(text) {
      packets <- split_lines(text) %>%
        purrr::map(parse_packet)
      names(packets) = list("left", "right")
      packets
    }
    
    modules::export("parse_input")
    parse_input <- function(input_string) {
      input_string %>%
        parsing$split_line_separated_blocks() %>%
        purrr::map(parse_pair)
    }
    
    compare_packets <- function(packet, other_paket) {
      packet_length <- length(packet)
      other_length <- length(other_paket)
      
      if (packet_length == 0L) {
        if (other_length == 0L) {
          return(0L)
        } else {
          return(-1L)
        }
      } else if (other_length == 0L) {
        return(1L)
      }
      
      min_length <- min(c(packet_length,other_length ))
      for (index in 1:min_length) {
        item <- packet[[index]]
        other_item <- other_paket[[index]]
        comp <- if (is.integer(item)) {
          if (is.integer(other_item)) {
            sign(item - other_item)
          } else {
            compare_packets(list(item), other_item)
          }
        } else if (is.integer(other_item)) {
          compare_packets(item, list(other_item))
        } else {
          compare_packets(item, other_item)
        }
        if (comp != 0L) {
          return(comp)
        }
      }
      
      sign(packet_length - other_length)
    }
    
    modules::export("solve_part1")
    solve_part1 <- function(pairs) {
      indices <- seq_along(pairs)
      has_correct_order <- purrr::map_lgl(
        .x = pairs,
        .f = function(pair) {
          compare_packets(pair$left, pair$right) <= 0L
        } 
      )
      indices[has_correct_order] %>%
        sum() %>% 
        as.character()
    }
    
    merge <- function(packets, other_packets) {
      packets_length <- length(packets)
      others_length <- length(other_packets)
      result <- vector(mode = "list", length = packets_length + others_length)
      
      write_index <- 1L
      packet_index <- 1L
      other_index <- 1L
      
      while (packet_index <= packets_length && other_index <= others_length) {
        if (compare_packets(packets[[packet_index]], other_packets[[other_index]]) <= 0L) {
          result[[write_index]] <- packets[[packet_index]]
          packet_index <- packet_index + 1L
        } else {
          result[[write_index]] <- other_packets[[other_index]]
          other_index <- other_index + 1L
        }
        write_index <- write_index + 1L
      }
      
      result[write_index:length(result)] <- if (packet_index <= packets_length) {
        packets[packet_index:packets_length] 
      } else {
        other_packets[other_index:others_length]
      }
      
      result
    }
    
    merge_all <- function(packet_lists) {
      current_list <- packet_lists
      current_length <- length(current_list)
      while (current_length > 1L) {
        merged_length <- ceiling(current_length / 2)
        merged_list <- vector(mode = "list", length = merged_length)
        pair_count <- floor(current_length / 2)
        for (merged_index in 1:pair_count) {
          merged_list[[merged_index]] <- merge(current_list[[2L * merged_index - 1L]], current_list[[2L * merged_index]])
        }
        if (pair_count < merged_length) {
          merged_list[[merged_length]] <- current_list[[current_length]]
        }
        current_list <- merged_list
        current_length <- merged_length
      }
      
      current_list[[1]]
    }
    
    sort_pair <- function(pair) {
      if (compare_packets(pair$left, pair$right) > 0L) {
        left <- pair$left
        pair$left <- pair$right
        pair$right <- left
      }
      pair
    }
    
    find_packet <- function(seq, packet, start_index = 1L) {
      index <- start_index
      while (index <= length(seq) && !identical(seq[[index]], packet)) {
        index <- index + 1
      } 
      if (index > length(seq)) {
        NA
      } else {
        index
      }
    }
    
    modules::export("solve_part2")
    solve_part2 <- function(pairs) {
      marker_pair <- parse_pair("[[2]]
[[6]]")
      all_pairs <- append(pairs, list(marker_pair))
      sorted_pairs <- purrr::map(
        .x = all_pairs,
        .f = sort_pair
      )
      sorted_packets <- merge_all(sorted_pairs)
      left_marker_index <- find_packet(sorted_packets, marker_pair$left)
      right_marker_index <- find_packet(sorted_packets, marker_pair$right)
      as.character(left_marker_index * right_marker_index)
    }
  }
)


#' Parsed Input to Day13 of Advent Of Code 2022
#'
#' @param input_string Input string to day 13 of AdventOfCode
#'
#' @return A list of pairs of packets
#' @importFrom magrittr %>%
parse_day13_input <- function(input_string) {
  day13$parse_input(input_string)
}

#' Solution to Day13 Part1 of Advent of Code 2022
#'
#' @param pairs A list of pairs of packets
#'
#' @return The sum of the indices of the pairs in the correct order 
#' @importFrom magrittr %>%
solve_day13_part1 <- function(pairs) {
  day13$solve_part1(pairs)
}

#' Solution to Day13 Part2 of Advent of Code 2022
#'
#' @param pairs A list of pairs of packets
#'
#' @return The product of the indices of the divider packets after merging and sorting all pairs
#' @importFrom magrittr %>%
solve_day13_part2 <- function(pairs) {
  day13$solve_part2(pairs)
}
