#' @import rlang
#' @export
rmap <- function(
        .l,
        .f,
        ...,
        env = rlang::caller_env(),
        map_fn = purrr::pmap,
        simplify = TRUE
) {
    # TODO: type checking of arguments with errors
    # TODO: implement .x for length(.l) == 1 / .l atomic
    # TODO: implement .x for name in rhs of .f

    .f <- rlang::enexpr(.f)

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
            # TODO: remove this error to allow argument recycling
            stop("All elements of .l must be of equal length")
        }
        .l <- tibble::as_tibble(.l)
    }

    formula_names <- c(
        get_formula_names(.f),
        purrr::map(rlang::enexprs(...), get_formula_names)
    ) |> unlist() |> unique()

    recursive <- ".this" %in% formula_names

    if (!".i" %in% names(.l)) {
        .l <- dplyr::mutate(.l, .i = dplyr::row_number())
        if (recursive) {
            .f <- insert_argument(.f, ".this", ".i", rlang::sym(".i"))
        }
    }

    name_references <- intersect(paste0(names(.l), ".nm"), formula_names) |>
        purrr::discard(~ .x %in% names(.l))

    for (nm in name_references) {
        .l[[nm]] <- names(.l[[sub(".nm$", "", nm)]]) %||% rep_len(list(NULL), length(.l[[1]]))
        if (recursive) {
            .f <- insert_argument(.f, ".this", nm, rlang::sym(nm))
        }
    }

    # evaluate .args only after defining pronouns
    .args <- purrr::map(
        rlang::enquos(...),
        ~ rlang::eval_tidy(.x, data = .l)
    )

    nms <- intersect(names(.l), c(formula_names, ".i"))

    rlang::env_bind_lazy(env, .this = .this)

    .this <- rlang::new_function(
        args = purrr::map(
            purrr::set_names(nms),
            ~ rlang::missing_arg()
        ),
        body = .f[[2]],
        env = rlang::env(
            env,
            !!!.args
        )
    )

    out <- map_fn(.l = .l[nms], .f = .this)

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
