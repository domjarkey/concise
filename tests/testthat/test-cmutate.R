test_that("Input modes", {
    df <- tibble::tibble(x = 1:3, y = list(4, NULL, 6))

    # cmutate == mutate
    expect_equal(
        df |> cmutate(z = x + 1),
        df |> dplyr::mutate(z = x + 1)
    )

    # mode: cmutate(name = cmap(~ .f))
    expect_equal(
        df |> cmutate(z = rmap(~ is.null(y))),
        df |> cmutate(z = purrr::map_lgl(y, ~ is.null(.x)))
    )

    # mode: cmutate(name = ~ .f)
    expect_equal(
        df |> cmutate(z = ~ is.null(y)),
        df |> cmutate(z = purrr::map_lgl(y, ~ is.null(.x)))
    )

    # mode: cmutate(name ~ .f)
    expect_equal(
        df |> cmutate(z ~ is.null(y)),
        df |> cmutate(z = purrr::map_lgl(y, ~ is.null(.x)))
    )

    # overwrite column
    expect_equal(
        df |> cmutate(y ~ is.null(y)),
        df |> cmutate(y = purrr::map_lgl(y, ~ is.null(.x)))
    )
})

test_that("Multiple column inputs", {
    df <- tibble::tibble(x = 1:3, y = list(4, NULL, 1))

    expect_equal(
        df |> cmutate(z ~ ifelse(is.null(y), x, y) ? int),
        df |> cmutate(z = purrr::map2_int(x, y, ~ ifelse(is.null(.y), .x, .y)))
    )

    expect_equal(
        df |> cmutate(
            z ~ ifelse(is.null(y), x, y),
            w ~ max(x, z)
        ),
        df |> cmutate(
            z = purrr::map2_dbl(x, y, ~ ifelse(is.null(.y), .x, .y)),
            w = purrr::map2_dbl(x, z, ~ max(.x, .y))
        )
    )
})

# TODO: test .i, test .i when .i exists in data columns
test_that("Pronouns", {
    expect_equal(
        tibble::tibble(x = c(a = 1, b = 2)) |> cmutate(z ~ x.nm),
        structure(
            list(x = c(a = 1, b = 2), z = c("a", "b")),
            row.names = c(NA,  -2L),
            class = c("tbl_df", "tbl", "data.frame")
        )
    )

    expect_equal(
        tibble::tibble(x = c(1, 2)) |> cmutate(z ~ x.nm),
        structure(
            list(x = c(1, 2), z = c(NA_character_, NA_character_)),
            row.names = c(NA,  -2L),
            class = c("tbl_df", "tbl", "data.frame")
        )
    )

    expect_equal(
        tibble::tibble(x = c(a = 4, b = 5)) |> cmutate(z ~ .i),
        structure(
            list(x = c(a = 4, b = 5), z = 1:2),
            row.names = c(NA,  -2L),
            class = c("tbl_df", "tbl", "data.frame")
        )
    )
})
