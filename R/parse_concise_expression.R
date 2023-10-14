#' @import rlang
parse_concise_expression <- function(.data, .expr) {
    .expr <- rlang::enexpr(.expr)
    .expr_components <- concise_syntax(!!.expr)
    .f <- get_rhs(.expr_components$.f)
    .f_names <- get_formula_names(.f)
    .data_names <- names(.data)
    .f_arg_names <- intersect(.data_names, .f_names)
    .f_other_names <- setdiff(.f_names, .data_names)
    .extra_args <- list()

    execution_environment_variables <- purrr::map(
        .expr_components[-1],
        ~ rlang::eval_tidy(.x, data = .data)
    )

    name_references <- setdiff(
        paste0(.data_names, ".nm"),
        .data_names
    ) |> intersect(.f_other_names)

    for (nm_ref in name_references) {
        nm <- rlang::sym(stringr::str_remove(nm_ref, "\\.nm$"))
        .extra_args[[nm_ref]] <- rlang::expr(
            names(!!nm) %||% !!rep_len(list(NULL), nrow(.data))
        )
    }

    group_references <- setdiff(
        paste0(.data_names, ".grp"),
        .data_names
    ) |> intersect(.f_other_names)

    for (grp_ref in group_references) {
        grp <- rlang::sym(stringr::str_remove(grp_ref, "\\.grp$"))
        .extra_args[[grp_ref]] <- rlang::expr(list(!!grp))
    }

    if (".i" %in% .f_other_names) {
        .extra_args[[".i"]] <- rlang::expr(dplyr::row_number())
    }

    if (".I" %in% .f_other_names || (length(.f_arg_names) + length(.extra_args) == 0)) {
        .extra_args[[".I"]] <- rlang::expr(dplyr::cur_group_rows())
    }

    if (".n" %in% .f_other_names) {
        .extra_args[[".n"]] <- rlang::expr(dplyr::n())
    }

    if (".N" %in% .f_other_names) {
        execution_environment_variables[[".N"]] <- nrow(.data)
    }

    col_references <- setdiff(
        paste0(.data_names, ".col"),
        .data_names
    ) |> intersect(.f_other_names)

    for (col_ref in col_references) {
        col <- rlang::sym(stringr::str_remove(col_ref, "\\.col$"))
        execution_environment_variables[[col_ref]] <- rlang::eval_tidy(col, data = .data)
    }

    .f_args <- purrr::map(
        purrr::set_names(unique(c(.f_arg_names, names(.extra_args)))),
        ~ rlang::missing_arg()
    )

    .map_fn <- execution_environment_variables$.map_fn %||% default_map_fn

    execution_environment_variables$.map_fn <- NULL

    .f_env <- rlang::env(
        rlang::caller_env(),
        !!!execution_environment_variables
    )

    rlang::env_bind_lazy(.f_env, .this = .this)

    .this <- rlang::new_function(
        args = .f_args,
        body = .f[[2]],
        env = .f_env
    )

    rlang::call2(
        .map_fn,
        rlang::call2("list", !!!(rlang::syms(.f_arg_names)), !!!.extra_args),
        .this
    )
}

default_map_fn <- function(.l, .f) {
    purrr::pmap(.l, .f)
}

try_simplify <- function(col) {
    if (length(unlist(col)) == length(col)) {
        unlist(col)
    } else {
        col
    }
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
            .out <- list(rlang::enexpr(rhs))
            names(.out) <- as.character(rlang::enexpr(lhs))
            .out
        },
        chr = list(.map_fn = rlang::expr(purrr::pmap_chr)),
        dbl = list(.map_fn = rlang::expr(purrr::pmap_dbl)),
        df = list(.map_fn = rlang::expr(purrr::pmap_df)),
        int = list(.map_fn = rlang::expr(purrr::pmap_int)),
        lgl = list(.map_fn = rlang::expr(purrr::pmap_lgl)),
        list = list(.map_fn = rlang::expr(purrr::pmap))
    )

    if (rlang::is_formula(expr)) {
        list(.f = eval(expr))
    } else {
        eval(rlang::enexpr(expr), e)
    }
}
