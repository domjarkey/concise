#' Infix functions
#'
#' @description
#' Infix functions for mapping from one vector to another. \code{%from%} and
#' \code{%to%} should be used in sequence as ternary operators to map an input
#' from a domain vector to a codomain vector by position. \code{%with%} can
#' follow an expression to evaluate that expression using a list or data frame
#' as the environment for evaluation. See examples.
#'
#' @examples
#' # Map a sequence of letters to their numerical positions in the alphabet
#' c('d', 'o', 'g') %from% letters %to% 1:26
#'
#' # Map US states to their abbreviations
#' c('California', 'Virginia', 'Texas') %from% state.name %to% state.abb
#'
#' # Map character names to species using the dplyr::starwars dataset
#' data("starwars", package = "dplyr")
#' c("Han Solo", "R2-D2", "Chewbacca") %from% name %to% species %with% starwars
#'
#' @name concise-infixes
NULL

#' @rdname concise-infixes
#' @export
`%from%` <- function(input, domain) {
    list(input = input, domain = domain)
}

#' @rdname concise-infixes
#' @export
`%to%` <- function(from, codomain) {
    codomain[match(from$input, from$domain)]
}

#' @rdname concise-infixes
#' @export
`%with%` <- function(lhs, rhs) {
    lhs <- rlang::enquo(lhs)
    rlang::eval_tidy(lhs, data = rhs)
}
