
## A simple implementation of a queue


modules::export("empty_queue")
empty_queue <- function() {
  c()
}


modules::export("size")
size <- function(queue) {
  length(queue)
}


modules::export("is_empty")
is_empty <- function(queue) {
  size(queue) == 0
}


modules::export("enque")
enque <- function(queue, item) {
  c(queue, item)
}

modules::export("peek_queue")
peek_queue <- function(queue) {
  queue[[1]]
}


modules::export("dequeue")
dequeue <- function(queue) {
  if (size(queue) == 1) {
    c()
  } else (
    queue[2:length(queue)]
  )
}



