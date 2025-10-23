#' Infix functions
#'
#' @description
#' Infix functions for mapping from one vector to another. \code{%from%} and
#' \code{%to%} should be used in sequence as ternary operators to map an input
#' from a domain vector to a codomain vector by position. \code{%with%} can
#' follow an expression to evaluate that expression using a list or data frame
#' as the environment for evaluation. See examples.
#'
#' @param input A vector of values to map (must contain only elements in domain
#' vector)
#' @param domain A domain vector of unique elements to map from
#' @param from The output of %from% (see examples)
#' @param codomain A codomain vector of elements to map the input to (must be of
#' equal length to domain)
#' @param expr Any simple R expression to be evaluated
#' @param data A data frame or named list to be used as the local environment
#' variables for evaluation of expr
#'
#' @examples
#' # Map a sequence of letters to their numerical positions in the alphabet
#' c("d", "o", "g") %from% letters %to% 1:26
#'
#' # Map US states to their abbreviations
#' c("California", "Virginia", "Texas") %from% state.name %to% state.abb
#'
#' # Map character names to species using the dplyr::starwars dataset
#' data("starwars", package = "dplyr")
#' c("Han Solo", "R2-D2", "Chewbacca") %from% name %to% species %with% starwars
#'
#' # Find mean height of characters in the starwars dataset
#' mean(height, na.rm = TRUE) %with% starwars
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
  ifelse(
    from$input %in% from$domain,
    codomain[match(from$input, from$domain)],
    from$input
  )
}

#' @rdname concise-infixes
#' @export
`%to_NA%` <- function(from, codomain) {
  codomain[match(from$input, from$domain)]
}

#' @rdname concise-infixes
#' @export
`%with%` <- function(expr, data) {
  expr <- rlang::enquo(expr)
  rlang::eval_tidy(expr, data = data)
}
