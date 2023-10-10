#' @export
`%from%` <- function(input, domain) {
    list(input = input, domain = domain)
}

#' @export
`%to%` <- function(from, codomain) {
    codomain[match(from$input, from$domain)]
}

#' @export
`%with%` <- function(lhs, rhs) {
    lhs <- rlang::enquo(lhs)
    rlang::eval_tidy(lhs, data = rhs)
}
