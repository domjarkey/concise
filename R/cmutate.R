#' Create, modify, and delete columns with Concise notation
#'
#' @description
#' `cmutate` functions exactly like [dplyr::mutate] with the additional feature
#' that column definitions specified using a `~` instead of `=` are iteratively
#' evaluated as a lambda function. In practise, this works similarly to using
#' [dplyr::rowwise] before [dplyr::mutate], except the result is calculated much
#' faster and is less computationally expensive, and does so without overwriting
#' the existing groupings of the data.
#'
#' Columns defined using a `~` also allow for the usage of additional "pronouns"
#' to concisely refer to related objects or properties of the data columns, such
#' as row number, groups, as well as the anonymous function itself for use in
#' recursion.
#'
#' A `?` following the column definition allows for specifying additional arguments
#' to pass to the lambda function, as well as the output type of the column, e.g.
#' integer, character, list, etc.
#'
#' @param .data A data frame
#' @param ... Any combination of one or more name-value pairs to be evaluated as
#' columns in the same fashion as in [dplyr::mutate] and two-sided formula of the
#' form `x ~ expr` where `x` gives the new name of the column, and `expr` denotes
#' the anonymous function to be iteratively evaluated to create the new column.
#' As with [dplyr::mutate], the expression `expr` can refer to data columns in
#' `.data` as though they were variables defined in the local scope.
#'
#' @details
#' # Optional column type specification with `?`
#' By default, columns specified with a `~` are simplified to a vector if possible,
#' similar to `rmap` with `simplify=TRUE`. If a specific output type is desired,
#' à la `rmap_int`, `rmap_dbl`, etc., the column definition can be followed by a
#' `?` and then `chr`, `dbl`, `df`, `int`, `lgl`, or `list`. When using this
#' notation, the column definition is of the form `x ~ expr ? type`. See Examples
#' for further clarification.
#'
#' # Additional lambda function arguments with `?`
#' Occasionally it is helpful to pass additional arguments to be used in the
#' evaluation of the lambda function. Like the type specification above, these
#' can be defined inside parentheses after a `?` following the column definition.
#' In this case the column definition is of the form `x ~ expr ? (arg = value)`.
#' If multiple arguments need to be specified, they can be chained together inside
#' curly braces and separated by semi-colons. This takes the following form:
#' `x ~ expr ? {arg1 = val1; arg2 = val2; ...}` or if type needs to be specified,
#' `x ~ expr ? {type; arg1 = val1; arg2 = val2; ...}`. See examples for further
#' clarification.
#'
#' # Pronouns
#' `cmutate` supports a set of useful "pronouns" that allow you to refer
#' to other objects within the column formula as though they were locally defined
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
#' @returns Returns a data frame with new columns for each name-value pair and/or
#' formula specified in ... arguments. Number of rows and data frame attributes
#' are preserved. Groups are preserved unless grouping variables are mutated, in
#' which case they will be recomputed.
#'
#' @examples
#' # Apply non-vectorised functions row-by-row
#' tibble::tibble(fruit = list('apple', 'banana', NULL, 'dragonfruit', NULL)) |>
#' cmutate(fruit_exists ~ !is.null(fruit))
#'
#' # Perform multiple column mutations, including standard (non-rowwise) column
#' # mutations using `=` instead of `~`. Mutations may depend on earlier changes
#' # made in the same `cmutate` call, and may modify existing columns in place
#' tibble::tibble(
#'     fruit = list('apple', 'banana', NULL, 'dragonfruit', NULL)
#' ) |>
#' cmutate(
#'     fruit_exists ~ !is.null(fruit),
#'     fruit_name_length ~ ifelse(fruit_exists, stringr::str_length(fruit), 0),
#'     fruit = as.character(ifelse(fruit_exists, fruit, "NO FRUIT FOUND"))
#' )
#'
#' numbers <- tibble::tibble(
#' x = c(29L, 11L, 72L, 81L, 27L, 61L, 42L, 26L, 57L, 39L),
#' y = c(38L, 80L, 98L, 93L, 34L, 26L, 4L, 31L, 18L, 69L),
#' z = c(31L, 83L, 91L, 69L, 82L, 65L, 75L, 3L, 20L, 71L)
#' )
#'
#' numbers |> cmutate(largest ~ max(x, y, z))
#'
#' # Use pronouns such as `.i` and `<column_name>.col` to succinctly create
#' # anonymous window functions
#' numbers |> cmutate(avg_x ~ mean(x.col[max(.i - 3, 1):.i]))
#'
#' # Concise pronouns can be used in combination with groupings created by
#' dplyr::group_by
#' numbers$letter <- rep(c('A', 'B'), each = 5)
#'
#' numbers |>
#'     dplyr::select(letter, x) |>
#'     dplyr::group_by(letter) |>
#'     cmutate(
#'         prop_of_group ~ x / sum(x.grp),
#'         prop_of_whole ~ x / sum(x.col),
#'         group_row_index ~ .i,
#'         column_row_index ~ .I
#' )
#'
#' # The `?` operator can be used after an expression to indicate the expected
#' # data type of the column
#' numbers |>
#' dplyr::select(x, y, z) |>
#'     cmutate(
#'         max ~ max(x, y, z),
#'         max_int ~ max(x, y, z) ? int,
#'         max_dbl ~ max(x, y, z) ? dbl,
#'         max_chr ~ max(x, y, z) ? chr,
#'         max_list ~ max(x, y, z) ? list,
#'  )
#'
#' # Recursive functions can be implemented using the `.this` pronoun to refer
#' # to the anonymous function
#' tibble::tibble(n = 1:10) |> cmutate(
#'     fibonacci ~ if (n <= 2) {1} else {.this(n - 1) + .this(n - 2)}
#' )
#'
#' @export
cmutate <- function(.data, ...) {
    .args <- rlang::enquos(...)
    .out <- .data
    for (i in seq_along(.args)) {
        if (is_concise_formula(rlang::quo_get_expr(.args[[i]]))) {
            if (names(.args)[i] == "") {
                names(.args)[i] <- as.character(get_lhs(rlang::quo_get_expr(.args[[i]])))
            }
            .args[[i]] <- rlang::quo_set_expr(
                .args[[i]],
                parse_concise_expression(.out, !!(rlang::quo_get_expr(.args[[i]])))
            )
        }
        .out <- .out |> dplyr::mutate(!!!(.args[i]))
        if (identical(rlang::quo_get_expr(.args[[i]])[[1]], default_map_fn)) {
            .out[[names(.args)[i]]] <- try_simplify(.out[[names(.args)[i]]])
        }
    }

    .out
}
