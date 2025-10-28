test_that("cfilter mirrors dplyr::filter across input modes", {
    df <- tibble::tibble(
        x = 1:5,
        y = c(TRUE, FALSE, TRUE, TRUE, FALSE),
        z = list(4, NULL, 6, NULL, 9)
    )

    # Standard expressions are delegated directly to dplyr::filter
    expect_equal(
        df |> cfilter(x > 2, y),
        df |> dplyr::filter(x > 2, y)
    )

    # context_lambda predicates behave the same as their purrr counterparts
    expect_equal(
        df |> cfilter(context_lambda(~ is.null(z))),
        df |> dplyr::filter(purrr::map_lgl(z, ~ is.null(.x)))
    )

    # Concise formulas are converted into rowwise predicates
    expect_equal(
        df |> cfilter(~ is.null(z)),
        df |> dplyr::filter(purrr::map_lgl(z, ~ is.null(.x)))
    )

    # Explicit type declarations are coerced to logical output
    expect_equal(
        df |> cfilter(~ is.null(z) ? int),
        df |> dplyr::filter(purrr::map_lgl(z, ~ is.null(.x)))
    )

    # Nested concise declarations retain only logical outputs
    expect_equal(
        df |> cfilter(~ is.null(z) ? {int ; extra = 1}),
        df |> dplyr::filter(purrr::map_lgl(z, ~ is.null(.x)))
    )
})

test_that("cfilter supports concise predicates with multiple inputs", {
    df <- tibble::tibble(
        x = 1:4,
        y = list(4, NULL, 1, NULL),
        z = c(TRUE, FALSE, TRUE, FALSE)
    )

    # Multiple column inputs map correctly when using concise notation
    expect_equal(
        df |> cfilter(~ ifelse(is.null(y), z, x > y) ? lgl),
        df |> dplyr::filter(
            purrr::pmap_lgl(
                list(x = x, y = y, z = z),
                ~ ifelse(is.null(..2), ..3, ..1 > ..2)
            )
        )
    )

    # Sequential concise predicates honour the progressively filtered data
    expect_equal(
        df |> cfilter(~ !is.null(y), ~ x > limit ? {limit = 1}),
        df |> dplyr::filter(!purrr::map_lgl(y, is.null)) |>
            dplyr::filter(x > 1)
    )
})

test_that("cfilter pronouns work without grouping", {
    df <- tibble::tibble(x = 3:1)

    # .I exposes the overall row index
    expect_equal(
        df |> cfilter(~ .I <= 2),
        df |> dplyr::filter(dplyr::row_number() <= 2)
    )

    # .N provides the total number of rows in the input
    expect_equal(
        df |> cfilter(~ .I == .N),
        df |> dplyr::filter(dplyr::row_number() == dplyr::n())
    )

    # .col pronouns expose the full column vector for aggregations
    expect_equal(
        df |> cfilter(~ x > mean(x.col)),
        df |> dplyr::filter(x > mean(x))
    )

    # Named vectors surface their names via the .nm pronoun
    expect_equal(
        tibble::tibble(x = c(a = 1, b = 2)) |> cfilter(~ x.nm == "a"),
        tibble::tibble(x = c(a = 1, b = 2)) |> dplyr::slice(1)
    )

    # Existing columns that shadow pronouns are respected
    expect_equal(
        tibble::tibble(x = 1:3, .i = c("a", "b", "c")) |> cfilter(~ .i == "b"),
        tibble::tibble(x = 1:3, .i = c("a", "b", "c")) |> dplyr::filter(.i == "b")
    )
})

test_that("cfilter pronouns respect grouping", {
    df <- tibble::tibble(
        colour = c("blue", "green", "blue", "blue", "green", "blue", "red", "blue"),
        shape = rep(c("triangle", "square"), 4),
        number = c(8, 5, 1, 2, 4, 7, 3, 6)
    )

    # .i yields the row number inside each group
    expect_equal(
        df |>
            dplyr::group_by(colour) |>
            cfilter(~ .i == 1 ? lgl),
        df |>
            dplyr::group_by(colour) |>
            dplyr::filter(dplyr::row_number() == 1)
    )

    # .I references the original row indices for grouped data
    expect_equal(
        df |>
            dplyr::group_by(colour, shape) |>
            cfilter(~ .I %in% 1:2),
        df |>
            dplyr::group_by(colour, shape) |>
            dplyr::filter(dplyr::cur_group_rows() %in% 1:2)
    )

    # Group size pronouns .n and .N behave like dplyr::n() and the input row count
    expect_equal(
        df |>
            dplyr::group_by(colour) |>
            cfilter(~ .n == 2),
        df |>
            dplyr::group_by(colour) |>
            dplyr::filter(dplyr::n() == 2)
    )

    expect_equal(
        df |>
            dplyr::group_by(colour) |>
            cfilter(~ .I <= .N - 2),
        df |>
            dplyr::group_by(colour) |>
            dplyr::filter(dplyr::cur_group_rows() <= nrow(df) - 2)
    )

    # .grp pronouns expose the grouped list-column values
    expect_equal(
        df |>
            dplyr::group_by(colour) |>
            cfilter(~ sum(number.grp) > 10),
        df |>
            dplyr::group_by(colour) |>
            dplyr::filter(sum(number) > 10)
    )
})

test_that("cfilter supports argument passing with ?", {
    df <- tibble::tibble(x = 3:1)

    # Constants can be injected into concise predicates
    expect_equal(
        df |> cfilter(~ x > threshold ? (threshold = 2)),
        df |> dplyr::filter(x > 2)
    )

    # Type declarations composed with & behave the same way
    expect_equal(
        df |> cfilter(~ x > threshold ? lgl & (threshold = 2)),
        df |> dplyr::filter(x > 2)
    )

    # Block syntax can provide multiple arguments at once
    expect_equal(
        df |> cfilter(~ x > lower & x < upper ? {lgl ; lower = 1 ; upper = 3}),
        df |> dplyr::filter(x > 1 & x < 3)
    )

    # Arguments can be derived from the incoming data
    expect_equal(
        df |> cfilter(~ x > avg ? {lgl ; avg = mean(x)}),
        df |> dplyr::filter(x > mean(x))
    )

    # Local variables are distinguishable from columns using !!
    threshold <- 57L

    expect_equal(
        tibble::tibble(x = 3:1) |>
            cfilter(~ x + X > limit ? {lgl ; X = sum(!!(1:10)) ; limit = !!threshold}),
        tibble::tibble(x = 3:1) |>
            dplyr::filter(x + sum(1:10) > 57L)
    )
})

test_that("cfilter supports recursive concise predicates", {
    df <- tibble::tibble(value = 6:1)

    # Recursion through .this allows referencing other rows
    expect_equal(
        df |>
            cfilter(
                ~ if (value <= 3) {value %% 2 == 1} else {.this(value - 1)}
            ),
        df |> dplyr::slice(c(1, 2, 3, 4, 6))
    )
})
