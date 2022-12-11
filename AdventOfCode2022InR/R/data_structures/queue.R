
## A simple implementation of a queue for atomic types


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


modules::export("enqueue")
enqueue <- function(queue, item) {
  c(queue, item)
}

modules::export("peek")
peek <- function(queue) {
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

modules::export("as_vector")
as_vector <- function(queue) {
  queue
}

