#' Infix functions
#'
#' @description
#' Infix functions for mapping from one vector to another. \code{%from%} and
#' \code{%to%} should be used in sequence as ternary operators to map an input
#' from a domain vector to a codomain vector by position. \code{%to_na%} follows
#' the same syntax but returns `NA` when the input is not present in the domain.
#' \code{%with%} can follow an expression to evaluate that expression using a
#' list or data frame as the environment for evaluation. See examples.
#'
#' @param input A vector of values to map (must contain only elements in the
#' domain vector).
#' @param domain A domain vector of unique elements to map from.
#' @param from The output of `%from%` (see examples).
#' @param codomain A codomain vector of elements to map the input to (must be of
#' equal length to `domain`).
#' @param expr Any simple R expression to be evaluated.
#' @param data A data frame or named list to be used as the local environment
#' for evaluating `expr`.
#' @param x A list or vector to be coerced to another type.
#' @param type The desired output type for `x`. May be provided as a bare name
#' (e.g. `dbl`), a character string (e.g. "double"), or any other expression
#' that evaluates to a single string understood by [base::as.vector()]. In
#' addition to the `mode` values supported by [base::as.vector()], this
#' function recognises the shortcuts `lgl`, `int`, `dbl`, `chr`, `list`, `df`,
#' `dfc`, and `dfr`.
#'
#' @examples
#' # Map a sequence of letters to their numerical positions in the alphabet
#' c("d", "o", "g") %from% letters %to% 1:26
#'
#'
#' # Map US states to their abbreviations
#' c("California", "Virginia", "Texas") %from% state.name %to% state.abb
#'
#'
#' # Coerce an object to a different type using `%as%`
#' list(a = 1:3, b = 4:6) %as% dfc
#'
#'
#' # Map character names to species using the dplyr::starwars dataset
#' data("starwars", package = "dplyr")
#' c("Han Solo", "R2-D2", "Chewbacca") %from% name %to% species %with% starwars
#'
#'
#' # Find mean height of characters in the starwars dataset
#' mean(height, na.rm = TRUE) %with% starwars
#'
#'
#' # Use the concise ternary operator to branch on a condition
#' (1:3) %% 2 == 0 %?% "even" %:% "odd"
#'
#'
#' # Return objects of different types depending on the condition
#' TRUE %?% list(value = 1) %:% "no value"
#'
#' @name concise-infixes
NULL

#' @rdname concise-infixes
#' @export
`%?%` <- function(condition, if_true) {
    if (!is.logical(condition)) {
        rlang::abort("`condition` supplied to `%?%` must be a logical vector.")
    }

    structure(
        list(condition = condition, if_true = if_true),
        class = "concise_conditional"
    )
}

#' @rdname concise-infixes
#' @export
`%:%` <- function(partial, if_false) {
    if (!inherits(partial, "concise_conditional")) {
        stop("`%:%` must be used immediately after `%?%`.")
    }

    condition <- partial$condition
    if_true <- partial$if_true

    if (length(condition) == 1) {
        if (isTRUE(condition)) {
            return(if_true)
        }

        if (isFALSE(condition)) {
            return(if_false)
        }

        return(NA)
    }

    recycle <- function(x, len) {
        if (len == 0) {
            return(x[0])
        }

        if (!length(x)) {
            stop("Inputs to `%:%` cannot be empty when other arguments are non-empty.")
        }

        if (len %% length(x) == 0) {
            rep(x, length.out = len)
        } else if (length(x) == 1) {
            rep(x, len)
        } else {
            stop("Inputs to `%:%` must have lengths that are compatible for recycling.")
        }
    }

    target_length <- max(length(condition), length(if_true), length(if_false))

    condition <- recycle(condition, target_length)
    if_true <- recycle(if_true, target_length)
    if_false <- recycle(if_false, target_length)

    result <- if_false
    true_idx <- which(condition)

    if (length(true_idx)) {
        result[true_idx] <- if_true[true_idx]
    }

    na_idx <- which(is.na(condition))

    if (length(na_idx)) {
        if (is.list(result)) {
            result[na_idx] <- rep(list(NA), length(na_idx))
        } else {
            result[na_idx] <- NA
        }
    }

    result
}

#' @rdname concise-infixes
#' @export
`%from%` <- function(input, domain) {
  list(input = input, domain = domain)
}

#' @rdname concise-infixes
#' @export
`%to%` <- function(from, codomain) {
  ifelse(
    from$input %in% from$domain,
    codomain[match(from$input, from$domain)],
    from$input
  )
}

#' @rdname concise-infixes
#' @export
`%as%` <- function(x, type) {
    type_expr <- rlang::enexpr(type)

    type_string <- if (rlang::is_symbol(type_expr)) {
        rlang::as_name(type_expr)
    } else {
        evaluated <- rlang::eval_tidy(type_expr)

        if (!rlang::is_string(evaluated)) {
            rlang::abort("`type` must be a single string or bare name specifying the target type.")
        }

        evaluated
    }

    type_string <- tolower(type_string)

    aliases <- c(
        lgl = "logical",
        int = "integer",
        dbl = "double",
        chr = "character",
        df = "tibble",
        data.frame = "data.frame",
        dfc = "dfc",
        dfr = "dfr"
    )

    if (type_string %in% names(aliases)) {
        type_string <- aliases[[type_string]]
    }

    if (type_string %in% c("tibble", "dfc")) {
        return(
            tibble::as_tibble(tibble::repair_names(x))
        )
    }

    if (type_string == "dfr") {
        if (rlang::is_bare_list(x) || is.data.frame(x)) {
            return(
                purrr::map(x, as.list) |>
                    data.table::rbindlist(use.names = FALSE, fill = TRUE) |>
                    tibble::as_tibble()
            )
        }

        return(tibble::tibble(value = x))
    }

    if (type_string == "data.frame") {
        return(as.data.frame(x, stringsAsFactors = FALSE))
    }

    as.vector(x, mode = type_string)
}

#' @rdname concise-infixes
#' @export
`%to_na%` <- function(from, codomain) {
  codomain[match(from$input, from$domain)]
}

#' @rdname concise-infixes
#' @export
`%with%` <- function(expr, data) {
  expr <- rlang::enquo(expr)
  rlang::eval_tidy(expr, data = data)
}
