
## A simple implementation of a stack for atomic types


modules::export("empty_stack")
empty_stack <- function() {
  c()
}

modules::export("size")
size <- function(stack) {
  length(stack)
}

modules::export("is_empty")
is_empty <- function(stack) {
  size(stack) == 0
}

modules::export("push")
push <- function(stack, item) {
  c(stack, item)
}

modules::export("peek")
peek <- function(stack, n = 1){
  stack[(length(stack) - n + 1):length(stack)]
}

modules::export("pop")
pop <- function(stack, n = 1){
  if (length(stack) == n) {
    c()
  } else {
    stack[1:(length(stack) - n)]
  }
}