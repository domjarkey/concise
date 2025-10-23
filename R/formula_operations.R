atomise_formula <- function(.f) {
  if (purrr::is_formula(.f)) {
    return(purrr::map(as.list(.f)[-1], atomise_formula))
  } else if (is.call(.f)) {
    return(purrr::map(as.list(.f), atomise_formula))
  } else {
    return(.f)
  }
}

substitute_name <- function(.f, nm_old, nm_new) {
  if (length(.f) == 1) {
    if (.f == nm_old) {
      .f <- as.name(nm_new)
    }
  } else {
    for (i in seq_along(.f)) {
      .f[[i]] <- substitute_name(.f[[i]], nm_old, nm_new)
    }
  }
  .f
}

get_formula_names <- function(.f) {
  as.character(unlist(atomise_formula(.f)))
}

is_concise_formula <- function(.f) {
  length(.f) > 1 &&
    length(as.character(.f[[1]])) == 1 &&
    as.character(.f[[1]]) %in% c("~", "?")
}

get_lhs <- function(.f) {
  stopifnot(is_concise_formula(.f))
  if (.f[[1]] == rlang::sym("?")) {
    get_lhs(.f[[2]])
  } else if (length(.f) == 3) {
    .f[[2]]
  } else {
    NULL
  }
}

get_rhs <- function(.f) {
  stopifnot(is_concise_formula(.f))
  if (.f[[1]] == rlang::sym("?")) {
    get_rhs(.f[[2]])
  } else if (length(.f) == 3) {
    .f[[3]]
  } else {
    .f[[2]]
  }
}

insert_argument <- function(.f, .f_name, arg_name, arg_value, force = FALSE) {
  if (length(.f) >= 2 && .f[[1]] == rlang::sym(.f_name)) {
    if (force | !(arg_name %in% names(.f))) {
      .f[[arg_name]] <- arg_value
    }
  } else if (length(.f) >= 2) {
    for (i in seq_along(.f)) {
      .f[[i]] <- insert_argument(.f[[i]], .f_name, arg_name, arg_value, force)
    }
  }
  .f
}
