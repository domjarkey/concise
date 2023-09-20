cmap <- function( .l, .f, ..., env = parent.frame(), map_fn = purrr::pmap, simplify = FALSE ) {
    # TODO: implement .x for length(.l) == 1 / .l atomic
    # TODO: implement argument passing inside formula

    .f <- rlang::enexpr(.f)

    if (length(.f) > 1 && .f[[1]] == rlang::sym("?")) {
        if (any(.f[[3]] == rlang::exprs(chr, dbl, df, int, lgl))) {
            map_fn <- switch(
                as.character(.f[[3]]),
                chr = purrr::pmap_chr,
                dbl = purrr::pmap_dbl,
                df = purrr::pmap_df,
                int = purrr::pmap_int,
                lgl = purrr::pmap_lgl
            )
            .f <- as.formula(.f[[2]], env = env)
            return(cmap(.l, !!.f, ..., env = env, map_fn = map_fn, simplify = simplify))
        }
    } else if (length(.f) == 3) {
        # ignore RHS
        .f <- .f[-2]
    }

    .l <- list(
        .x = .l,
        .i = seq_along(.l),
        .nm = if(is.null(names(.l))) {seq_along(.l)} else {names(.l)}
    )
    nms <- names(.l)
    .this <- rlang::new_function(
        args = purrr::map(
            purrr::set_names(nms),
            ~ rlang::expr(rlang::missing_arg())
        ),
        body = .f[[2]]
    )
    out <- map_fn(.l, .f = .this, ...)
    if (simplify && length(out) == length(unlist(out))) {
        unlist(out)
    } else {
        out
    }
}

# TODO: add cmap_x etc.
# cmap_chr <- function(.l, .f = NULL, ..., env = parent.frame(), map_fn = purrr::pmap_chr) {
#     cmap(.l = .l, .f = .f, ..., env = env, map_fn = map_fn)
# }
#
# cmap_dbl <- function(.l, .f = NULL, ..., env = parent.frame(), map_fn = purrr::pmap_dbl) {
#     cmap(.l = .l, .f = .f, ..., env = env, map_fn = map_fn)
# }
#
# cmap_df <- function(.l, .f = NULL, ..., env = parent.frame(), map_fn = purrr::pmap_df) {
#     cmap(.l = .l, .f = .f, ..., env = env, map_fn = map_fn)
# }
#
# cmap_int <- function(.l, .f = NULL, ..., env = parent.frame(), map_fn = purrr::pmap_int) {
#     cmap(.l = .l, .f = .f, ..., env = env, map_fn = map_fn)
# }
