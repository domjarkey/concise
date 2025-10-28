#' Filter rows with Concise notation
#'
#' @description
#' `filter.` behaves identically to [dplyr::filter] while adding support for
#' Concise notation and pronouns, mirroring the behaviour of [mutate.].
#' Expressions that begin with `~` are interpreted as concise formulas and are
#' evaluated rowwise using the same pronouns that `mutate.` recognises. These
#' expressions can also specify optional mappings and additional arguments using
#' the `?` operator, just like `mutate.`.
#'
#' @param .data A data frame.
#' @param ... Expressions passed on to [dplyr::filter]. Any expression that
#'   begins with `~` is treated as a concise formula and gains access to the
#'   additional pronouns and helpers provided by [mutate.].
#'
#' @return A data frame containing the rows that satisfy all filtering
#'   expressions.
#'
#' @examples
#' tibble::tibble(x = list("apple", NULL, "banana")) |>
#'   filter.(~ !is.null(x))
#'
#' tibble::tibble(x = 1:5) |>
#'   filter.(~ .i %% 2 == 1)
#'
#' @export
filter. <- function(.data, ...) {
    .args <- rlang::enquos(...)
    .out <- .data

    for (i in seq_along(.args)) {
        .expr <- rlang::quo_get_expr(.args[[i]])

        if (is_concise_formula(.expr)) {
            .expr <- ensure_concise_type(.expr, "lgl")
            .parsed <- parse_concise_expression(.out, !!.expr)
            .args[[i]] <- rlang::quo_set_expr(
                .args[[i]],
                .parsed
            )
        }

        .out <- dplyr::filter(.out, !!.args[[i]])
    }

    .out
}
