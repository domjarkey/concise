#' @export
cmutate <- function(.data, ...) {
    # TODO: is tidyselect possible inside formula; if not possible use list.
    #       Maybe needs another argument to pass cols

    .args <- rlang::enquos(...)
    .out <- .data
    for (i in seq_along(.args)) {
        if (is_concise_formula(rlang::quo_get_expr(.args[[i]]))) {
            if (names(.args)[i] == "") {
                names(.args)[i] <- as.character(get_lhs(rlang::quo_get_expr(.args[[i]])))
            }
            .args[[i]] <- rlang::quo_set_expr(
                .args[[i]],
                parse_concise_expression(.out, .args[[i]])
            )
        }
        .out <- .out |> dplyr::mutate(!!!(.args[i]))
    }

    .out
}
