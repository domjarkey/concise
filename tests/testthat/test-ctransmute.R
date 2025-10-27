test_that("Input modes", {
    df <- tibble::tibble(x = 1:3, y = list(4, NULL, 6))

    expect_equal(
        df |> ctransmute(z = x + 1),
        df |> dplyr::transmute(z = x + 1)
    )

    expect_equal(
        df |> ctransmute(z = x),
        df |> dplyr::transmute(z = x)
    )

    expect_equal(
        df |> ctransmute(z = y),
        df |> dplyr::transmute(z = y)
    )

    expect_equal(
        df |> ctransmute(z = context_lambda(~ is.null(y))),
        df |> ctransmute(z = purrr::map_lgl(y, ~ is.null(.x)))
    )

    expect_equal(
        df |> ctransmute(z ~ is.null(y)),
        df |> ctransmute(z = purrr::map_lgl(y, ~ is.null(.x)))
    )

    expect_equal(
        df |> ctransmute(y ~ is.null(y)),
        df |> ctransmute(y = purrr::map_lgl(y, ~ is.null(.x)))
    )
})

test_that("Multiple column inputs", {
    df <- tibble::tibble(x = 1:3, y = list(4, NULL, 1))

    expect_equal(
        df |> ctransmute(z ~ ifelse(is.null(y), x, y) ? int),
        df |> ctransmute(z = purrr::map2_int(x, y, ~ ifelse(is.null(.y), .x, .y)))
    )

    expect_equal(
        df |> ctransmute(z ~ ifelse(is.null(y), x, y) ? list),
        df |> ctransmute(z = purrr::map2(x, y, ~ ifelse(is.null(.y), .x, .y)))
    )

    expect_equal(
        df |> ctransmute(
            z ~ ifelse(is.null(y), x, y),
            w ~ max(x, z)
        ),
        df |> ctransmute(
            z = purrr::map2_dbl(x, y, ~ ifelse(is.null(.y), .x, .y)),
            w = purrr::map2_dbl(x, z, ~ max(.x, .y))
        )
    )
})

test_that("Pronouns", {
    expect_equal(
        tibble::tibble(x = c(a = 1, b = 2)) |> ctransmute(z ~ x.nm),
        tibble::tibble(z = c("a", "b"))
    )

    expect_equal(
        tibble::tibble(x = c(1, 2)) |> ctransmute(z ~ x.nm),
        tibble::tibble(z = list(NULL, NULL))
    )

    expect_equal(
        tibble::tibble(x = c(a = 4, b = 5)) |> ctransmute(z ~ .i),
        tibble::tibble(z = 1:2)
    )

    expect_equal(
        tibble::tibble(
            x = c(4, 5),
            .i = c("a", "b")
        ) |> ctransmute(z ~ .i),
        tibble::tibble(z = c("a", "b"))
    )

    expect_equal(
        tibble::tibble(
            x = c(a = 4, b = 5),
            x.nm = c("c", "d")
        ) |> ctransmute(z ~ x.nm),
        tibble::tibble(z = c("c", "d"))
    )

    expect_equal(
        tibble::tibble(x = 3:1) |>
            ctransmute(y ~ x + sum(x.col)),
        tibble::tibble(y = 9:7)
    )

    expect_equal(
        tibble::tibble(x = 3:1) |>
            ctransmute(y ~ sum(x.col)),
        tibble::tibble(y = c(6, 6, 6))
    )
})

test_that("Groups hold", {
    df <- tibble::tibble(
        colour = c("blue", "green", "blue", "blue", "green", "blue", "red", "blue"),
        shape = rep(c("triangle", "square"), 4),
        number = c(8, 5, 1, 2, 4, 7, 3, 6)
    )

    expect_equal(
        df |>
            dplyr::group_by(colour) |>
            ctransmute(index ~ .i),
        df |>
            dplyr::group_by(colour) |>
            dplyr::transmute(index = dplyr::row_number())
    )

    expect_equal(
        df |>
            dplyr::group_by(colour, shape) |>
            ctransmute(index ~ .i),
        df |>
            dplyr::group_by(colour, shape) |>
            dplyr::transmute(index = dplyr::row_number())
    )

    expect_equal(
        df |>
            dplyr::group_by(colour, shape) |>
            ctransmute(group_row_index ~ .i, overall_row_index ~ .I),
        df |>
            dplyr::group_by(colour, shape) |>
            dplyr::transmute(
                group_row_index = dplyr::row_number(),
                overall_row_index = dplyr::cur_group_rows()
            )
    )

    expect_equal(
        df |>
            dplyr::group_by(colour) |>
            ctransmute(x ~ sum(number.grp), y ~ sum(number.col)),
        df |>
            tibble::as_tibble() |>
            dplyr::group_by(colour) |>
            dplyr::transmute(
                colour,
                x = sum(number),
                y = 36
            )
    )

    expect_equal(
        df |>
            dplyr::group_by(colour) |>
            ctransmute(x ~ number.grp),
        df |>
            tibble::as_tibble() |>
            dplyr::group_by(colour) |>
            dplyr::transmute(
                colour,
                x = purrr::pmap(
                    list(number.grp = list(number)),
                    \(number.grp) number.grp
                )
            )
    )

    expect_equal(
        df |> dplyr::group_by(colour) |>
            ctransmute(
                avg ~ mean(number.col[max(1, .I - 1):.I]),
                grp_avg ~ mean(number.grp[max(1, .i - 1):.i])
            ),
        df |>
            tibble::as_tibble() |>
            dplyr::group_by(colour) |>
            dplyr::transmute(
                colour,
                avg = purrr::pmap_dbl(
                    list(
                        number.col = list(dplyr:::peek_mask()$get_current_data()$number),
                        .I = dplyr::cur_group_rows()
                    ),
                    \(number.col, .I) {
                        mean(number.col[max(1, .I - 1):.I])
                    }
                ),
                grp_avg = purrr::pmap_dbl(
                    list(
                        number.grp = list(number),
                        .i = dplyr::row_number()
                    ),
                    \(number.grp, .i) {
                        mean(number.grp[max(1, .i - 1):.i])
                    }
                )
            )
    )
})
