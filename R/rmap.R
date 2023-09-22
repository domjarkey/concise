#' @importFrom rlang `%||%`
#' @export
rmap <- function(
        .l,
        .f = NULL,
        ...,
        env = parent.frame(),
        map_fn = purrr::pmap,
        simplify = TRUE,
        .i = tryCatch(
            dplyr::row_number(),
            error = function(e)
                NULL
        )
) {
    # TODO: implement .x for length(.l) == 1 / .l atomic
    # TODO: implement .x for name in rhs of .f
    # TODO: implement argument passing inside formula

    ..l <- rlang::enexpr(.l)
    .f <- rlang::enexpr(.f)

    if (is_concise_formula(..l)) {
        formula_names <- get_formula_names(..l)
        recursive <- ".this" %in% formula_names
        nms <- purrr::keep(
            formula_names,
            \( .x ) {
                tryCatch(
                    !is.null(env$.data[[.x]]),
                    error = function(e) FALSE
                )
            }
        ) |> unique()
        name_references <- purrr::keep(
            stringr::str_subset(formula_names, ".nm$"),
            \( .x ) {
                tryCatch(
                    !is.null(env$.data[[sub(".nm$", "", .x)]]),
                    error = function(e) FALSE
                )
            }
        ) |> unique()
        .f <- ..l
        .l <- purrr::map_dfc(nms, ~ tibble::tibble({{ .x }} := env$.data[[.x]]))
        if (!is.null(.i) && !(".i" %in% nms)) {
            if (length(nms) == 0) {
                .l <- list(.i = .i)
            } else {
                .l$.i <- .i
            }
            if (recursive) {
                .f <- insert_argument(.f, ".this", ".i", rlang::sym(".i"))
            }
        }
        for (nm in name_references) {
            if (!nm %in% names(.l)) {
                .l[[nm]] <- names(env$.data[[sub(".nm$", "", nm)]]) %||% rep_len(NA_character_, length(.i))
            }
            if (recursive) {
                .f <- insert_argument(.f, ".this", nm, rlang::sym(nm))
            }
        }
        return(rmap(.l, !!.f, ..., env = env, map_fn = map_fn, simplify = simplify, .i = .i))
    }

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
            return(rmap(.l, !!.f, ..., env = env, map_fn = map_fn, simplify = simplify, .i = .i))
        }
    } else if (length(.f) == 3) {
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
        if (is.null(.i)) {
            .l <- dplyr::mutate(.l, .i = dplyr::row_number())
        } else {
            .l$.i <- .i
        }
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
    nms <- intersect(names(.l), formula_names)
    .this <- rlang::new_function(
        args = purrr::map(
            purrr::set_names(nms),
            ~ rlang::expr(rlang::missing_arg())
        ),
        body = .f[[2]]
    )
    out <- map_fn(.l[nms], .f = .this, ...)
    # TODO: add error detection/reporting if length(nms) == 0 and nrow(out) == 0
    if (simplify && length(out) == length(unlist(out))) {
        unlist(out)
    } else {
        out
    }
}

# TODO: add rmap_x etc.
# rmap_chr <- function(.l, .f = NULL, ..., env = parent.frame(), map_fn = purrr::pmap_chr) {
#     rmap(.l = .l, .f = .f, ..., env = env, map_fn = map_fn)
# }
#
# rmap_dbl <- function(.l, .f = NULL, ..., env = parent.frame(), map_fn = purrr::pmap_dbl) {
#     rmap(.l = .l, .f = .f, ..., env = env, map_fn = map_fn)
# }
#
# rmap_df <- function(.l, .f = NULL, ..., env = parent.frame(), map_fn = purrr::pmap_df) {
#     rmap(.l = .l, .f = .f, ..., env = env, map_fn = map_fn)
# }
#
# rmap_int <- function(.l, .f = NULL, ..., env = parent.frame(), map_fn = purrr::pmap_int) {
#     rmap(.l = .l, .f = .f, ..., env = env, map_fn = map_fn)
# }
