#' Create and modify columns with Concise notation
#'
#' @description
#' `ctransmute` functions exactly like [dplyr::transmute] with the additional
#' feature that column definitions specified using a `~` instead of `=` are
#' iteratively evaluated as a lambda function. In practice, this works similarly
#' to using [dplyr::rowwise] before [dplyr::transmute], except the result is
#' calculated much faster and is less computationally expensive, and does so
#' without overwriting the existing groupings of the data.
#'
#' Columns defined using a `~` also allow for the usage of additional "pronouns"
#' to concisely refer to related objects or properties of the data columns, such
#' as row number, groups, as well as the anonymous function itself for use in
#' recursion.
#'
#' A `?` following the column definition allows for specifying additional
#' arguments to pass to the lambda function, as well as the output type of the
#' column, e.g. integer, character, list, etc.
#'
#' @inheritParams cmutate
#'
#' @return
#' A data frame containing only the columns created or referenced in the
#' expressions passed through `...`, consistent with the behaviour of
#' [dplyr::transmute()]. Grouping variables are preserved.
#'
#' @examples
#' tibble::tibble(fruit = list("apple", "banana", NULL, "dragonfruit", NULL)) |>
#'   ctransmute(fruit_exists ~ !is.null(fruit))
#'
#' tibble::tibble(x = c(29L, 11L, 72L), y = c(38L, 80L, 98L)) |>
#'   ctransmute(
#'     largest ~ max(x, y),
#'     diff = largest - x
#'   )
#'
#' @export
ctransmute <- function(.data, ...) {
    .args <- rlang::enquos(...)
    .out <- .data
    keep_names <- character(0)

    for (i in seq_along(.args)) {
        if (is_concise_formula(rlang::quo_get_expr(.args[[i]]))) {
            if (names(.args)[i] == "") {
                names(.args)[i] <- as.character(
                    get_lhs(rlang::quo_get_expr(.args[[i]]))
                )
            }
            .args[[i]] <- rlang::quo_set_expr(
                .args[[i]],
                parse_concise_expression(.out, !!(rlang::quo_get_expr(.args[[i]])))
            )
        }

        .out <- .out |> dplyr::mutate(!!!(.args[i]))

        if (
            rlang::is_call(rlang::quo_get_expr(.args[[i]])) &&
            identical(rlang::quo_get_expr(.args[[i]])[[1]], default_map_fn)
        ) {
            .out[[names(.args)[i]]] <- try_simplify(.out[[names(.args)[i]]])
        }

        col_name <- if (names(.args)[i] != "") {
            names(.args)[i]
        } else {
            rlang::quo_name(.args[[i]])
        }

        keep_names <- keep_names[keep_names != col_name]
        keep_names <- c(keep_names, col_name)
    }

    grouping_vars <- dplyr::group_vars(.out)
    grouping_vars <- grouping_vars[grouping_vars %in% names(.out)]
    final_names <- c(grouping_vars, keep_names[!keep_names %in% grouping_vars])

    if (length(final_names) == 0) {
        dplyr::transmute(.out)
    } else {
        .out |> dplyr::select(dplyr::all_of(final_names))
    }
}
