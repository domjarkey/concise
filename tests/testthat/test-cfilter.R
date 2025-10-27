test_that("cfilter mirrors dplyr::filter for standard expressions", {
    df <- tibble::tibble(x = 1:5, y = c(TRUE, FALSE, TRUE, TRUE, FALSE))

    expect_equal(
        df |> cfilter(x > 2, y),
        df |> dplyr::filter(x > 2, y)
    )
})

test_that("cfilter supports concise formulas", {
    df <- tibble::tibble(x = list(1, NULL, 3, NULL))

    expect_equal(
        df |> cfilter(~ !is.null(x)),
        df |> dplyr::filter(purrr::map_lgl(x, ~ !is.null(.x)))
    )
})

test_that("cfilter pronouns work with grouping", {
    df <- tibble::tibble(group = rep(c("a", "b"), each = 3), value = 1:6)

    expect_equal(
        df |>
            dplyr::group_by(group) |>
            cfilter(~ .i == 1 ? lgl),
        df |>
            dplyr::group_by(group) |>
            dplyr::filter(dplyr::row_number() == 1)
    )
})

test_that("cfilter pronouns work without grouping", {
    df <- tibble::tibble(x = 1:5)

    expect_equal(
        df |> cfilter(~ .I == .N),
        df |> dplyr::filter(dplyr::row_number() == dplyr::n())
    )
})


test_that("cfilter pronouns work with grouping and ? assignment", {
    df <- tibble::tibble(group = rep(c("a", "b"), each = 3), value = 1:6)

    expect_equal(
        df |>
            dplyr::group_by(group) |>
            cfilter(~ .i == z ? {z = 2}),
        df |>
            dplyr::group_by(group) |>
            dplyr::filter(dplyr::row_number() == 2)
    )
})

test_that("cfilter pronouns work without grouping and ? assignment", {
    df <- tibble::tibble(x = 1:5)

    expect_equal(
        df |> cfilter(~ .I %% u == v ? {u = 2; v = 1}),
        df |> dplyr::filter(dplyr::row_number() %% 2 == 1)
    )
})
