test_that("Input modes", {
    df <- tibble::tibble(x = 1:3, y = list(4, NULL, 6))

    # cmutate == mutate
    expect_equal(
        df |> cmutate(z = x + 1),
        df |> dplyr::mutate(z = x + 1)
    )

    # mode: cmutate(name = context_lambda(~ .f))
    expect_equal(
        df |> cmutate(z = context_lambda(~ is.null(y))),
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
        df |> cmutate(z ~ ifelse(is.null(y), x, y) ? list),
        df |> cmutate(z = purrr::map2(x, y, ~ ifelse(is.null(.y), .x, .y)))
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

test_that("Pronouns", {
    expect_equal(
        tibble::tibble(x = c(a = 1, b = 2)) |> cmutate(z ~ x.nm),
        tibble::tibble(x = c(a = 1, b = 2), z = c("a", "b"))
    )

    expect_equal(
        tibble::tibble(x = c(1, 2)) |> cmutate(z ~ x.nm),
        tibble::tibble(x = c(1, 2), z = list(NULL, NULL))
    )

    expect_equal(
        tibble::tibble(x = c(a = 4, b = 5)) |> cmutate(z ~ .i),
        tibble::tibble(x = c(a = 4, b = 5), z = 1:2)
    )

    expect_equal(
        tibble::tibble(
            x = c(4, 5),
            .i = c("a", "b")
        ) |> cmutate(z ~ .i),
        tibble::tibble(
            x = c(4, 5),
            .i = c("a", "b"),
            z = c("a", "b")
        )
    )

    expect_equal(
        tibble::tibble(
            x = c(a = 4, b = 5),
            x.nm = c("c", "d")
        ) |> cmutate(z ~ x.nm),
        tibble::tibble(
            x = c(a = 4, b = 5),
            x.nm = c("c", "d"),
            z = c("c", "d")
        )
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
            cmutate(index ~ .i),
        df |>
            dplyr::group_by(colour) |>
            dplyr::mutate(index = dplyr::row_number())
    )

    expect_equal(
        df |>
            dplyr::group_by(colour, shape) |>
            cmutate(index ~ .i),
        df |>
            dplyr::group_by(colour, shape) |>
            dplyr::mutate(index = dplyr::row_number())
    )
})

test_that("Argument passing with ?", {
    # Pass constants
    expect_equal(
        tibble::tibble(x = 3:1) |>
            cmutate(y ~ x + z ? (z = 10)),
        tibble::tibble(x = 3:1, y = as.numeric(13:11))
    )

    expect_equal(
        tibble::tibble(x = 3:1) |>
            cmutate(y ~ x + z ? int & (z = 10)),
        tibble::tibble(x = 3:1, y = 13:11)
    )

    # Pass transformations of data variable(s) and pronouns
    expect_equal(
        tibble::tibble(x = 3:1) |>
            cmutate(y ~ x + X ? int & (X = sum(x))),
        tibble::tibble(x = 3:1, y = 9:7)
    )

    expect_equal(
        tibble::tibble(x = 3:1) |>
            cmutate(y ~ x + I ? int & (I = sum(.i))),
        tibble::tibble(x = 3:1, y = 9:7)
    )

    expect_equal(
        list(a = purrr::set_names(letters[1:3], LETTERS[1:3]), b = letters[1:3]) |>
            tibble::as_tibble() |>
            cmutate(c ~ Z ? (Z = paste0(a.nm, collapse = ''))),
        list(a = purrr::set_names(letters[1:3], LETTERS[1:3]), b = letters[1:3]) |>
            tibble::as_tibble() |>
            dplyr::mutate(c = "ABC")
    )

    # Differentiate local variables using !! injection
    x <- 1:10

    expect_equal(
        tibble::tibble(x = 3:1) |>
            cmutate(y ~ x + X ? int & (X = sum(!!x))),
        tibble::tibble(x = 3:1, y = 58:56)
    )

    # Specify other objects in lambda function's execution environment
    expect_equal(
        tibble::tibble(
            determiner = c("the", "a", "those"),
            adjective = c("quick", "slow", "naughty"),
            noun = c("fox", "loris", "children")
        ) |> cmutate(sentence ~ determiner + adjective + noun - end ? (`+` = paste) & (`-` = paste0) & (end = ".")),
        tibble::tibble(
            determiner = c("the", "a", "those"),
            adjective = c("quick", "slow", "naughty"),
            noun = c("fox", "loris", "children")
        ) |> dplyr::mutate(sentence = paste0(paste(determiner, adjective, noun), "."))
    )

})

test_that("Recursion", {
    expect_equal(
        tibble::tibble(
            value = 6:1
        ) |> cmutate(
            fib ~ if (value <= 2) {1} else {.this(value - 1) + .this(value - 2)}
        ),
        tibble::tibble(value = 6:1, fib = c(8, 5, 3, 2, 1, 1))
    )

    expect_equal(
        tibble::tibble(
            value = 6:1
        ) |> cmutate(
            fib ~ if (value <= 2) {z} else {.this(value - 1) + .this(value - 2)} ? int & (z = 1)
        ),
        tibble::tibble(value = 6:1, fib = c(8L, 5L, 3L, 2L, 1L, 1L))
    )
})
