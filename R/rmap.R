#' Map over rows of a data frame
#'
#' @description
#' `rmap` iteratively applies an anonymous function to rows of a data frame or a
#' named list of lists of equal length. `rmap` works similarly to [purrr::pmap],
#' except the data frame does not need to be subset to only those columns used by
#' the function, and columns can be directly referred to by name inside the definition
#' of the anonymous function.
#'
#' `rmap` also permits the usage of additional "pronouns" to concisely refer to
#' related objects or properties of the data columns, such as row number, grouping
#' (if the data frame is grouped using [dplyr::group_by] or similar), as well as
#' the anonymous function itself for use in recursion.
#'
#' `rmap_chr`, `rmap_dbl`, `rmap_df`, `rmap_int`, and `rmap_lgl` work like their
#' `purrr::pmap_etc` equivalents, attempting to output a vector of the specified
#'  type (or a data.frame in the case of `rmap_df`) instead of a list.
#'
#' @param .l A data frame or named list of lists/vectors of equal length.
#' @param .f A formula defining the anonymous function to be applied to every
#' element of `.l`, in which `.x` refers to the given element. See Examples for
#' how this differs from formulas in [purrr::map].
#' @param ... Additional named arguments are passed directly to the execution
#' environment of the anonymous function as variables. These variables may include
#' transformations on the entire `.l` object as well as any variables local to the
#' calling environment of the `rmap` function.
#' @param env Specify the parent environment of the execution environment of the
#' anonymous function to be created. By default, this will be the environment in
#' which `rmap` is called.
#' @param map_fn The underlying function used to perform the mapping - this should
#' not be chosen directly as safe options are prespecified by the defaults of the
#' `_chr`, `_dbl`, `_df`, `_int`, and `_lgl` variants of `rmap`.
#' @param simplify If `TRUE`, the function will attempt to flatten list outputs
#' to a vector where possible (if "unlisting" is not possible, say, in the case of
#' a list of lists of length > 1, the original output in list form will be returned).
#'
#' @details
#' # Pronouns
#' `rmap` supports reference to a set of useful "pronouns" that allow you to refer
#' to other objects within the formula `.f` as though they were locally defined
#' as variables. Note these are not pronouns in the `rlang` sense of the term, but
#' a convenient shorthand to provide additional functionality in a readable format.
#' Unlike its sister function `cmap`, `rmap` takes multiple named inputs, so row
#' elements a referred to by the name of the column (here indicated as `<column_name>`)
#' instead of `.x`.
#' See Examples for further clarification on the usage of each. Supported pronouns
#' are:
#'
#' * `<column_name>` -- in the formula, this refers to an individual element of
#' the named data column.
#' * `.i` -- the row index, or, if `.l` is grouped, the row index within the group.
#' Equivalent to [dplyr::row_number()].
#' * `.I` -- the absolute row index; whether or not `.l` is grouped, this will
#' return the overall position of the current row. Equivalent to
#' [dplyr::cur_group_rows()].
#' * `<column_name>.nm` -- if the column `<column_name>` in `.l` is named, this
#' returns the name corresponding to the current element of `<column_name>`; If
#' this column of `.l` is unnamed, returns `NULL`. Note that columns in `tibble`
#' data frames retain their `names` attribute, but ordinary base R data frames
#' do not.
#' * `.n` -- the index of the final row in `.l`, or, if `.l` is grouped, the final
#' row in the given group. Equivalent to [dplyr::n()].
#' * `.N` -- the index of the final row in `.l`, whether or not `.l` is grouped.
#' Equivalent to `nrow(.l)`.
#' * `<column_name>.grp` -- all elements in the current group as a list object.
#' If `.l` is not grouped, returns the same as `<column_name>.col`, i.e. the entire
#' column `<column_name>` as a list object.
#' * `<column_name>.col` -- the entire data column `<column_name>` in `.l`, as
#' opposed to just the current element. Returns the full column whether or not `.l`
#' is grouped.
#' * `.this` -- the anonymous function itself, to be used in cases where recursion
#' is needed.
#' @returns Returns a list (or vector) of the same length as `.l`. By default a
#' list, unless `rmap_` suffixes are used to specify the output vector type, or
#' if `simplify=TRUE` a vector of whichever type uniformly fits the unlisted
#' outputs.
#'
#' @examples
#' numbers <- tibble(
#' x = c(29L, 11L, 72L, 81L, 27L, 61L, 42L, 26L, 57L, 39L),
#' y = c(38L, 80L, 98L, 93L, 34L, 26L, 4L, 31L, 18L, 69L),
#' z = c(31L, 83L, 91L, 69L, 82L, 65L, 75L, 3L, 20L, 71L),
#' letter = rep(c('A', 'B'), each = 5)
#' )
#'
#' numbers |>
#' rmap_chr(~ paste0("Row ", .i, ", Group ", letter, ": ", mean(c(x, y, z))))
#'
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
    .f <- get_rhs(rlang::enexpr(.f))

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

    if (!".I" %in% names(.l) && ".I" %in% formula_names) {
        .l <- dplyr::mutate(.l, .I = dplyr::cur_group_rows())
        if (recursive) {
            .f <- insert_argument(.f, ".this", ".I", rlang::sym(".I"))
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

    group_references <- intersect(paste0(names(.l), ".grp"), formula_names) |>
        purrr::discard(~ .x %in% names(.l))

    for (grp in group_references) {
        .l <- .l |> cmutate(!!grp ~ !!rlang::sym(grp))
        if (recursive) {
            .f <- insert_argument(.f, ".this", grp, rlang::sym(grp))
        }
    }

    if (!".n" %in% names(.l) && ".n" %in% formula_names) {
        .l <- .l |> dplyr::mutate(.n = dplyr::n())
        if (recursive) {
            .f <- insert_argument(.f, ".this", ".n", rlang::sym(".n"))
        }
    }

    .args <- purrr::map(
        rlang::enquos(...),
        ~ rlang::eval_tidy(.x, data = .l)
    )

    execution_environment_variables <- list()

    col_references <- intersect(paste0(names(.l), ".col"), formula_names) |>
        purrr::discard(~ .x %in% names(.l))

    for (col in col_references) {
        execution_environment_variables <- append(
            execution_environment_variables,
            rlang::list2(!!col := .l[[stringr::str_remove(col, ".col$")]])
        )
    }

    if (".N" %in% formula_names) {
        execution_environment_variables[[".N"]] <- nrow(.l)
    }

    nms <- intersect(names(.l), c(formula_names, ".i"))

    rlang::env_bind_lazy(env, .this = .this)

    .args <- append(
        purrr::map(
            purrr::set_names(nms),
            ~ rlang::missing_arg()
        ),
        .args
    )

    .this <- rlang::new_function(
        args = .args,
        body = .f,
        env = rlang::env(
            env,
            !!!execution_environment_variables
        )
    )

    .out <- map_fn(.l = .l[nms], .f = .this)

    if (simplify && length(.out) == length(unlist(.out))) {
        unlist(.out)
    } else {
        .out
    }
}

#' @rdname rmap
#' @export
rmap_chr <- function(.l, .f = NULL, ..., env = parent.frame()) {
    rmap(.l = .l, .f = !!.f, ..., env = env, map_fn = purrr::pmap_chr)
}

#' @rdname rmap
#' @export
rmap_dbl <- function(.l, .f = NULL, ..., env = parent.frame()) {
    rmap(.l = .l, .f = !!.f, ..., env = env, map_fn = purrr::pmap_dbl)
}

#' @rdname rmap
#' @export
rmap_df <- function(.l, .f = NULL, ..., env = parent.frame()) {
    rmap(.l = .l, .f = !!.f, ..., env = env, map_fn = purrr::pmap_df)
}

#' @rdname rmap
#' @export
rmap_int <- function(.l, .f = NULL, ..., env = parent.frame()) {
    rmap(.l = .l, .f = !!.f, ..., env = env, map_fn = purrr::pmap_int)
}

#' @rdname rmap
#' @export
rmap_lgl <- function(.l, .f = NULL, ..., env = parent.frame()) {
    rmap(.l = .l, .f = !!.f, ..., env = env, map_fn = purrr::pmap_lgl)
}
