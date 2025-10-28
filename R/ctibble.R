#' Create tibbles with Concise notation
#'
#' @description
#' `ctibble` functions exactly like [tibble::tibble] with the additional
#' feature that column definitions specified using a `~` instead of `=` are
#' iteratively evaluated as a lambda function with access to the same pronouns
#' and helpers available in [cmutate]. This allows concise row-wise operations
#' while constructing a tibble, including referencing columns defined earlier in
#' the call, retrieving row indices, and accessing entire columns.
#'
#' @param ... Name-value pairs giving tibble columns. As with
#'   [tibble::tibble], evaluation happens sequentially so columns can refer to
#'   previously created columns. When a column is defined using a two-sided
#'   formula `x ~ expr`, the expression `expr` is evaluated iteratively as a
#'   lambda in the same fashion as [cmutate], including support for pronouns and
#'   optional type specification via `?`.
#'
#' @return A tibble created in the same way as [tibble::tibble] but with
#'   support for concise formulas.
#'
#' @examples
#' ctibble(x = 1:3, y = 2 * x)
#'
#' ctibble(
#'   x = 1:3,
#'   y = 3:1,
#'   sum ~ x + y ? int
#' )
#'
#' @export
ctibble <- function(...) {
    # TODO: Mimic tibble:::tibble_quos behaviour to allow row definitions of
    # different lengths to be recycled
    .args <- rlang::enquos(...)
    .out <- tibble::tibble()

    for (i in seq_along(.args)) {
        .expr <- rlang::quo_get_expr(.args[[i]])
        if (is_concise_formula(.expr)) {
            if (names(.args)[i] == "") {
                names(.args)[i] <- as.character(get_lhs(.expr))
            }
            .parsed <- parse_concise_expression(.out, !!.expr)
            .args[[i]] <- rlang::quo_set_expr(
                .args[[i]],
                .parsed
            )
        }

        .expr <- rlang::quo_get_expr(.args[[i]]) # Needs to be reset

        col_name <- names(.args)[i]
        if (col_name == "") {
            col_name <- paste0("...", i)
            names(.args)[i] <- col_name
        }

        if (ncol(.out) == 0) {
            value <- rlang::eval_tidy(.args[[i]], data = .out)

            if (
                rlang::is_call(.expr) &&
                identical(.expr[[1]], default_map_fn)
            ) {
                value <- try_simplify(value)
            }

            .out <- tibble::as_tibble(
                rlang::set_names(list(value), col_name),
                .name_repair = "minimal"
            )
        } else {
            .out <- .out |>
                dplyr::mutate(!!!(.args[i]))

            if (
                rlang::is_call(.expr) &&
                identical(.expr[[1]], default_map_fn)
            ) {
                .out[[col_name]] <- try_simplify(.out[[col_name]])
            }
        }
    }

    tibble::as_tibble(.out, .name_repair = "unique")
}
