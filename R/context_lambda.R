#' @import rlang
#' @export
context_lambda <- function(
        .f,
        ...,
        env = rlang::caller_env(),
        simplify = TRUE,
        .i = tryCatch(
            dplyr::row_number(),
            error = function(e)
                NULL
        )
) {
    # TODO: type checking of arguments with errors
    # TODO: implement .x for length(.l) == 1 / .l atomic
    # TODO: implement .x for name in rhs of .f
    # TODO: implement argument passing inside formula

    .f <- rlang::enexpr(.f)

    formula_names <- get_formula_names(.f)
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

    .l <- purrr::map_dfc(nms, ~ tibble::tibble({{ .x }} := env$.data[[.x]]))

    if (!is.null(.i) && !(".i" %in% nms)) {
        if (length(nms) == 0) {
            .l <- tibble::tibble(.i = .i)
        } else {
            .l$.i <- .i
        }
        if (recursive) {
            .f <- insert_argument(.f, ".this", ".i", rlang::sym(".i"))
        }
    }

    for (nm in name_references) {
        if (!nm %in% names(.l)) {
            .l[[nm]] <- names(env$.data[[sub(".nm$", "", nm)]]) %||% rep_len(list(NULL), length(.i))
        }
        if (recursive) {
            .f <- insert_argument(.f, ".this", nm, rlang::sym(nm))
        }
    }

    args <- concise_syntax(!!.f) |>
        append(
            list(.l = .l, env = env)
        )

    do.call("rmap", args)

    # return(rmap(.l, !!.f, ..., env = env, map_fn = map_fn, simplify = simplify, .i = .i))
}

concise_syntax <- function(expr) {
    expr <- rlang::enexpr(expr)

    e <- rlang::env(
        rlang::caller_env(),
        `?` = function(lhs, rhs) {
            append(list(.f = lhs), eval(rlang::enexpr(rhs), e))
        },
        `&` = function(lhs, rhs) {
            c(lhs, rhs)
        },
        `=` = function(lhs, rhs) {
            out <- list(rlang::enexpr(rhs))
            names(out) <- as.character(rlang::enexpr(lhs))
            out
        },
        chr = list(map_fn = rlang::expr(purrr::pmap_chr)),
        dbl = list(map_fn = rlang::expr(purrr::pmap_dbl)),
        df = list(map_fn = rlang::expr(purrr::pmap_df)),
        int = list(map_fn = rlang::expr(purrr::pmap_int)),
        lgl = list(map_fn = rlang::expr(purrr::pmap_lgl)),
        list = list(simplify = FALSE)
    )

    if (rlang::is_formula(expr)) {
        list(.f = eval(expr))
    } else {
        eval(rlang::enexpr(expr), e)
    }
}
