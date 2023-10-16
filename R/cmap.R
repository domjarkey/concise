#' @import rlang
#' @export
cmap <- function(
        .l,
        .f,
        ...,
        env = rlang::caller_env(),
        map_fn = purrr::pmap,
        simplify = FALSE
) {
    # TODO: type checking of arguments with errors

    .f <- get_rhs(rlang::enexpr(.f))

    formula_names <- get_formula_names(.f)

    if (".this" %in% formula_names) {
        .f <- insert_argument(.f, ".this", ".i", rlang::sym(".i"))
        .f <- insert_argument(.f, ".this", ".nm", rlang::sym(".nm"))
    }

    .l <- list(
        .x = .l,
        .i = seq_along(.l),
        .nm = if(is.null(names(.l))) {rep_len(list(NULL), length(.l))} else {names(.l)}
    )

    # evaluate .args only after defining pronouns
    .args <- purrr::map(
        rlang::enquos(...),
        ~ rlang::eval_tidy(.x, data = .l)
    )

    if (".col" %in% formula_names) {
        .args <- append(.args, list(.col = .l$.x))
    }

    if (".n" %in% formula_names) {
        .args <- append(.args, list(.n = length(.l$.x)))
    }

    nms <- names(.l)

    rlang::env_bind_lazy(env, .this = .this)

    .this <- rlang::new_function(
        args = purrr::map(
            purrr::set_names(nms),
            ~ if (.x %in% c(".i", ".nm")) {
                rlang::call2(rlang::expr("rlang::`!!`"), rlang::sym(.x))
            } else {
                rlang::expr(rlang::missing_arg())
            }
        ),
        body = .f,
        env = rlang::env(
            env,
            !!!.args
        )
    )

    .out <- map_fn(.l, .f = .this)

    if (simplify && length(.out) == length(unlist(.out))) {
        unlist(.out)
    } else {
        .out
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
