#' Apply an anonymous function to each element of a vector
#'
#' @description
#' `cmap` works very similarly to `purrr::map` when specifying the formula of the
#' anonymous function using tilde (`~`) formula notation, but includes extra custom
#' pronouns to refer to related objects such as element names, element indeces,
#' and the function itself, so as to permit recursive function definitions.
#'
#' `cmap_chr`, `cmap_dbl`, `cmap_df`, `cmap_int`, and `cmap_lgl` work like their
#' `purrr::map_etc` equivalents, attempting to output a vector of the specified type (or
#' a data.frame in the case of `cmap_df`) instead of a list.
#'
#' @param .x A list or atomic vector.
#' @param .f A formula defining the anonymous function to be applied to every
#' element of `.x`, in which `.x` refers to the given element. See Examples for
#' how this differs from formulas in `purrr::map`.
#' @param ... Additional named arguments are passed directly to the execution
#' environment of the anonymous function as variables. These variables may include
#' transformations on the entire `.x` object as well as any variables local to the
#' calling environment of the `cmap` function.
#' @param env Specify the parent environment of the execution environment of the
#' anonymous function to be created. By default, this will be the environment in
#' which `cmap` is called.
#' @param map_fn The underlying function used to perform the mapping - this should
#' not be chosen directly as safe options are prespecified by the defaults of the
#' `_chr`, `_dbl`, `_df`, `_int`, and `_lgl` variants of `cmap`.
#' @param simplify If `TRUE`, the function will attempt to flatten list outputs
#' to a vector where possible (if "unlisting" is not possible, say, in the case of
#' a list of lists of length > 1, the original output in list form will be returned).
#'
#' @details
#' # Pronouns
#' `cmap` supports reference to a set of useful "pronouns" that allow you to refer
#' to other objects within the formula `.f` as though they were locally defined
#' as variables. Note these are not pronouns in the `rlang` sense of the term, but
#' a convenient shorthand to provide additional functionality in a readable format.
#' See Examples for further clarification on the usage of each. Supported pronouns
#' are:
#'
#' * `.x` -- as in `purrr::map`, this refers to an individual element of the list
#' `.x`.
#' * `.i` -- the position of the element `.x` in a list.
#' * `.nm` -- if `.x` is named, this returns the name corresponding to the element
#' `.x`; If `.x` is unnamed, returns `NULL`.
#' * `.n` -- the value of the final position of the list, equivalent to `length(.x)`.
#' * `.col` -- the entire input list object or vector, as opposed to just the single
#' element refered to in .x.
#' * `.this` -- the anonymous function itself, to be used in cases where recursion
#' is needed.
#' @returns Returns a list (or vector) of the same length as `.x`. By default a
#' list, unless `cmap_` suffixes are used to specify the output vector type, or
#' if `simplify=TRUE` a vector of whichever type uniformly fits the unlisted
#' outputs.
#'
#'
#' @import rlang
#' @export
cmap <- function(.x, .f, ..., env = rlang::caller_env(), map_fn = purrr::pmap, simplify = FALSE) {
    # TODO: type checking of arguments with errors
    # TODO: change .args from execution environment variables to function arguments
    # TODO: look at pmap_vec for simplifying

    .f <- get_rhs(rlang::enexpr(.f))

    formula_names <- get_formula_names(.f)

    if (".this" %in% formula_names) {
        .f <- insert_argument(.f, ".this", ".i", rlang::sym(".i"))
        .f <- insert_argument(.f, ".this", ".nm", rlang::sym(".nm"))
    }

    .x <- list(
        .x = .x,
        .i = seq_along(.x),
        .nm = if (is.null(names(.x))) {
            rep_len(list(NULL), length(.x))
        } else {
            names(.x)
        }
    )

    # evaluate .args only after defining pronouns
    .args <- purrr::map(
        rlang::enquos(...),
        \(dot) rlang::eval_tidy(dot, data = .x)
    )

    if (".col" %in% formula_names) {
        .args <- append(.args, list(.col = .x$.x))
    }

    if (".n" %in% formula_names) {
        .args <- append(.args, list(.n = length(.x$.x)))
    }

    nms <- names(.x)

    rlang::env_bind_lazy(env, .this = .this)

    .this <- rlang::new_function(
        args = purrr::map(
            purrr::set_names(nms),
            \(nm) {
                if (nm %in% c(".i", ".nm")) {
                    rlang::call2(rlang::expr("rlang::`!!`"), rlang::sym(nm))
                } else {
                    rlang::expr(rlang::missing_arg())
                }
            }
        ),
        body = .f,
        env = rlang::env(
            env,
            !!!.args
        )
    )

    .out <- map_fn(.x, .f = .this)

    if (simplify && length(.out) == length(unlist(.out))) {
        unlist(.out)
    } else {
        .out
    }
}

#' @rdname cmap
#' @export
cmap_chr <- function(.x, .f, ..., env = parent.frame()) {
    cmap(.x = .x, .f = !!.f, ..., env = env, map_fn = purrr::pmap_chr)
}

#' @rdname cmap
#' @export
cmap_dbl <- function(.x, .f, ..., env = parent.frame()) {
    cmap(.x = .x, .f = !!.f, ..., env = env, map_fn = purrr::pmap_dbl)
}

#' @rdname cmap
#' @export
cmap_df <- function(.x, .f, ..., env = parent.frame()) {
    cmap(.x = .x, .f = !!.f, ..., env = env, map_fn = purrr::pmap_df)
}

#' @rdname cmap
#' @export
cmap_int <- function(.x, .f, ..., env = parent.frame()) {
    cmap(.x = .x, .f = !!.f, ..., env = env, map_fn = purrr::pmap_int)
}

#' @rdname cmap
#' @export
cmap_lgl <- function(.x, .f, ..., env = parent.frame()) {
    cmap(.x = .x, .f = !!.f, ..., env = env, map_fn = purrr::pmap_lgl)
}
