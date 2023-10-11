#' @import rlang
#' @export
cmap <- function( .l, .f, ..., env = parent.frame(), map_fn = purrr::pmap, simplify = FALSE ) {
    # TODO: type checking of arguments with errors
    # TODO: implement argument passing inside formula

    .f <- rlang::enexpr(.f)
    .args <- rlang::enquos(...)

    for (i in seq_along(.args)) {
        assign(
            names(.args)[i],
            rlang::eval_tidy(.args[[i]])
        )
    }

    if (length(.f) == 3) {
        # ignore RHS
        .f <- .f[-2]
    }

    if (".this" %in% get_formula_names(.f)) {
        .f <- insert_argument(.f, ".this", ".i", rlang::sym(".i"))
        .f <- insert_argument(.f, ".this", ".nm", rlang::sym(".nm"))
    }
    .l <- list(
        .x = .l,
        .i = seq_along(.l),
        .nm = if(is.null(names(.l))) {rep_len(NA_character_, length(.l))} else {names(.l)}
    )
    nms <- names(.l)
    .this <- rlang::new_function(
        args = purrr::map(
            purrr::set_names(nms),
            ~ if (.x %in% c(".i", ".nm")) {
                rlang::call2(rlang::expr("rlang::`!!`"), rlang::sym(.x))
            } else {
                rlang::expr(rlang::missing_arg())
            }
        ),
        body = .f[[2]]
    )
    out <- map_fn(.l, .f = .this)
    if (simplify && length(out) == length(unlist(out))) {
        unlist(out)
    } else {
        out
    }
}

#' @export
cmap_chr <- function(.l, .f = NULL, ..., env = parent.frame(), map_fn = purrr::pmap_chr) {
    cmap(.l = .l, .f = !!.f, ..., env = env, map_fn = map_fn)
}

#' @export
cmap_dbl <- function(.l, .f = NULL, ..., env = parent.frame(), map_fn = purrr::pmap_dbl) {
    cmap(.l = .l, .f = !!.f, ..., env = env, map_fn = map_fn)
}

#' @export
cmap_df <- function(.l, .f = NULL, ..., env = parent.frame(), map_fn = purrr::pmap_df) {
    cmap(.l = .l, .f = !!.f, ..., env = env, map_fn = map_fn)
}

#' @export
cmap_int <- function(.l, .f = NULL, ..., env = parent.frame(), map_fn = purrr::pmap_int) {
    cmap(.l = .l, .f = !!.f, ..., env = env, map_fn = map_fn)
}

#' @export
cmap_lgl <- function(.l, .f = NULL, ..., env = parent.frame(), map_fn = purrr::pmap_lgl) {
    cmap(.l = .l, .f = !!.f, ..., env = env, map_fn = map_fn)
}
