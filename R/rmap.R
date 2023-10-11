#' @import rlang
#' @export
rmap <- function(
        .l,
        .f = NULL,
        ...,
        env = parent.frame(),
        map_fn = purrr::pmap,
        simplify = TRUE
) {
    # TODO: type checking of arguments with errors
    # TODO: implement .x for length(.l) == 1 / .l atomic
    # TODO: implement .x for name in rhs of .f
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
    if (is.atomic(.l)) {
        .l <- tibble::tibble(`...1` = .l)
    }
    if (is.null(names(.l))) {
        names(.l) <- paste0("...", seq_along(.l))
    }
    if (is.list(.l) && !is.data.frame(.l)) {
        if (any(length(.l[[1]]) != lengths(.l))) {
            stop("All elements of .l must be of equal length")
        }
        .l <- tibble::as_tibble(.l)
    }
    formula_names <- get_formula_names(.f)
    recursive <- ".this" %in% formula_names
    if (!".i" %in% names(.l)) {
        # if (is.null(.i)) {
            .l <- dplyr::mutate(.l, .i = dplyr::row_number())
        # } else {
        #     .l$.i <- .i
        # }
        if (recursive) {
            .f <- insert_argument(.f, ".this", ".i", rlang::sym(".i"))
        }
    }
    name_references <- intersect(paste0(names(.l), ".nm"), formula_names) |>
        purrr::discard(~ .x %in% names(.l))
    for (nm in name_references) {
        .l[[nm]] <- names(.l[[sub(".nm$", "", nm)]]) %||% rep_len(NA_character_, length(.l[[1]]))
        if (recursive) {
            .f <- insert_argument(.f, ".this", nm, rlang::sym(nm))
        }
    }
    # Always include .i so .l has at least one column
    nms <- intersect(names(.l), c(formula_names, ".i"))
    .this <- rlang::new_function(
        args = purrr::map(
            purrr::set_names(nms),
            ~ rlang::expr(rlang::missing_arg())
        ),
        body = .f[[2]]
    )
    out <- map_fn(.l = .l[nms], .f = .this)
    # TODO: add error detection/reporting if length(nms) == 0 and nrow(out) == 0
    if (simplify && length(out) == length(unlist(out))) {
        unlist(out)
    } else {
        out
    }
}

#' @export
rmap_chr <- function(.l, .f = NULL, ..., env = parent.frame(), map_fn = purrr::pmap_chr) {
    rmap(.l = .l, .f = !!.f, ..., env = env, map_fn = map_fn)
}

#' @export
rmap_dbl <- function(.l, .f = NULL, ..., env = parent.frame(), map_fn = purrr::pmap_dbl) {
    rmap(.l = .l, .f = !!.f, ..., env = env, map_fn = map_fn)
}

#' @export
rmap_df <- function(.l, .f = NULL, ..., env = parent.frame(), map_fn = purrr::pmap_df) {
    rmap(.l = .l, .f = !!.f, ..., env = env, map_fn = map_fn)
}

#' @export
rmap_int <- function(.l, .f = NULL, ..., env = parent.frame(), map_fn = purrr::pmap_int) {
    rmap(.l = .l, .f = !!.f, ..., env = env, map_fn = map_fn)
}

#' @export
rmap_lgl <- function(.l, .f = NULL, ..., env = parent.frame(), map_fn = purrr::pmap_lgl) {
    rmap(.l = .l, .f = !!.f, ..., env = env, map_fn = map_fn)
}
