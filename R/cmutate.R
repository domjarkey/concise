cmutate <- function(.data, ...) {
    .args <- rlang::enquos(...)
    for (i in seq_along(.args)) {
        if (is_concise_formula(rlang::quo_get_expr(.args[[i]]))) {
            if (names(.args)[i] == "") {
                names(.args)[i] <- as.character(get_lhs(rlang::quo_get_expr(.args[[i]])))
            }
            .args[[i]] <- rlang::quo_set_expr(
                .args[[i]],
                rlang::call2("cmap", rlang::quo_get_expr(.args[[i]]))
            )
        }
    }
    dplyr::mutate(.data, !!!.args)
}
