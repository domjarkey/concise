#' @export
cmutate <- function(.data, ...) {
    .args <- rlang::enquos(...)
    .out <- .data
    for (i in seq_along(.args)) {
        if (is_concise_formula(rlang::quo_get_expr(.args[[i]]))) {
            if (names(.args)[i] == "") {
                names(.args)[i] <- as.character(get_lhs(rlang::quo_get_expr(.args[[i]])))
            }
            .args[[i]] <- rlang::quo_set_expr(
                .args[[i]],
                parse_concise_expression(.out, !!(rlang::quo_get_expr(.args[[i]])))
            )
        }
        .out <- .out |> dplyr::mutate(!!!(.args[i]))
        if (identical(rlang::quo_get_expr(.args[[i]])[[1]], default_map_fn)) {
            .out[[names(.args)[i]]] <- try_simplify(.out[[names(.args)[i]]])
        }
    }

    .out
}
