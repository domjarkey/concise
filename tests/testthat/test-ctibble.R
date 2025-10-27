test_that("ctibble matches tibble sequential evaluation", {
    expected <- tibble::tibble(x = 1:3, y = 2 * x)
    result <- ctibble(x = 1:3, y = 2 * x)

    expect_identical(result, expected)
})

test_that("ctibble supports both standard and concise column definitions", {
    # Standard evaluation should mirror tibble::tibble
    expect_identical(
        ctibble(x = 1:3, y = list(4, NULL, 6)),
        tibble::tibble(x = 1:3, y = list(4, NULL, 6))
    )

    # context_lambda expressions supplied with = should evaluate row wise
    expect_identical(
        ctibble(
            x = 1:3,
            y = list(4, NULL, 6),
            z = context_lambda(~ is.null(y))
        ),
        tibble::tibble(
            x = 1:3,
            y = list(4, NULL, 6),
            z = purrr::map_lgl(list(4, NULL, 6), ~ is.null(.x))
        )
    )

    # Concise ~ syntax should behave identically to context_lambda
    expect_identical(
        ctibble(
            x = 1:3,
            y = list(4, NULL, 6),
            z ~ is.null(y)
        ),
        tibble::tibble(
            x = 1:3,
            y = list(4, NULL, 6),
            z = purrr::map_lgl(list(4, NULL, 6), ~ is.null(.x))
        )
    )

    # Later concise columns can overwrite earlier columns in place
    expect_identical(
        ctibble(
            values = list(4, NULL, 6),
            values ~ is.null(values)
        ),
        tibble::tibble(
            values = purrr::map_lgl(list(4, NULL, 6), ~ is.null(.x))
        )
    )
})

test_that("ctibble evaluates concise formulas with multiple column inputs", {
    # Multi-column concise expressions should map over each row
    expect_identical(
        ctibble(
            x = 1:3,
            y = list(4, NULL, 1),
            z ~ ifelse(is.null(y), x, y) ? int
        ),
        tibble::tibble(
            x = 1:3,
            y = list(4, NULL, 1),
            z = purrr::map2_int(1:3, list(4, NULL, 1), ~ ifelse(is.null(.y), .x, .y))
        )
    )

    # Additional concise columns can build on previous concise results
    expect_identical(
        ctibble(
            x = 1:3,
            y = list(4, NULL, 1),
            z ~ ifelse(is.null(y), x, y),
            w ~ max(x, z)
        ),
        tibble::tibble(
            x = 1:3,
            y = list(4, NULL, 1),
            z = purrr::map2_dbl(
                1:3,
                list(4, NULL, 1),
                ~ ifelse(is.null(.y), .x, .y)
            ),
            w = purrr::map2_dbl(
                1:3,
                purrr::map2_dbl(
                    1:3,
                    list(4, NULL, 1),
                    ~ ifelse(is.null(.y), .x, .y)
                ),
                ~ max(.x, .y)
            )
        )
    )
})

test_that("ctibble pronouns expose names, indices, and column data", {
    # Named vectors should expose names through the .nm pronoun
    expect_identical(
        ctibble(x = c(a = 1, b = 2), x_names ~ x.nm),
        tibble::tibble(x = c(a = 1, b = 2), x_names = c("a", "b"))
    )

    # Unnamed vectors should surface NULL through the .nm pronoun
    expect_identical(
        ctibble(x = c(1, 2), x_names ~ x.nm),
        tibble::tibble(x = c(1, 2), x_names = list(NULL, NULL))
    )

    # .i and .I should both yield row indices in an ungrouped tibble
    row_indices <- ctibble(
        values = 5:7,
        local_row ~ .i ? int,
        global_row ~ .I ? int
    )
    expect_identical(row_indices$local_row, 1:3)
    expect_identical(row_indices$global_row, 1:3)

    # .N should provide the total number of rows for each evaluation
    expect_identical(
        ctibble(values = 1:4, total_rows ~ .N ? int)$total_rows,
        rep.int(4L, 4)
    )

    # .col should expose whole-column vectors for aggregate calculations
    cumulative <- ctibble(
        values = c(2, 4, 6),
        running_sum ~ sum(values.col[seq_len(.I)]) ? dbl
    )
    expect_identical(cumulative$running_sum, c(2, 6, 12))

    # .grp should surface list-columns containing the full column values
    grouped_view <- ctibble(
        values = c(10, 20, 30),
        group_values ~ values.grp
    )
    expect_identical(
        grouped_view$group_values,
        rep_len(list(c(10, 20, 30)), 3)
    )
})

test_that("ctibble supports argument injection with ? syntax", {
    # Inject constants into concise lambdas using tuple syntax
    expect_identical(
        ctibble(x = 3:1, y ~ x + z ? (z = 10)),
        tibble::tibble(x = 3:1, y = as.numeric(13:11))
    )

    # Explicit output type declarations should coerce results appropriately
    expect_identical(
        ctibble(x = 3:1, y ~ x + z ? int & (z = 10)),
        tibble::tibble(x = 3:1, y = 13:11)
    )

    # Block syntax should allow multiple injected bindings alongside type coercion
    expect_identical(
        ctibble(x = 3:1, y ~ x + z - w ? {int ; z = 10 ; w = 1}),
        tibble::tibble(x = 3:1, y = 12:10)
    )

    # Inject values derived from existing columns into concise lambdas
    expect_identical(
        ctibble(x = 3:1, y ~ x + X ? int & (X = sum(x))),
        tibble::tibble(x = 3:1, y = 9:7)
    )

    # Block injection should also work for computed values
    expect_identical(
        ctibble(x = 3:1, y ~ x + X ? {int; X = sum(x)}),
        tibble::tibble(x = 3:1, y = 9:7)
    )

    # Local variables should require explicit injection to avoid masking columns
    local_x <- 1:10
    expect_identical(
        ctibble(x = 3:1, y ~ x + X ? int & (X = sum(!!local_x))),
        tibble::tibble(x = 3:1, y = 58:56)
    )

    # Block syntax must honour injections of local variables via !!
    expect_identical(
        ctibble(x = 3:1, y ~ x + X ? {int; X = sum(!!local_x)}),
        tibble::tibble(x = 3:1, y = 58:56)
    )
})

test_that("ctibble supports recursive concise lambdas via .this", {
    # Recursive lambdas without type coercion should return numeric results
    expect_identical(
        ctibble(
            value = 6:1,
            fib ~ if (value <= 2) {1} else {.this(value - 1) + .this(value - 2)}
        ),
        tibble::tibble(value = 6:1, fib = c(8, 5, 3, 2, 1, 1))
    )

    # Recursive lambdas with explicit integer coercion should return integer vectors
    expect_identical(
        ctibble(
            value = 6:1,
            fib ~ if (value <= 2) {z} else {.this(value - 1) + .this(value - 2)} ? {int ; z = 1}
        ),
        tibble::tibble(value = 6:1, fib = c(8L, 5L, 3L, 2L, 1L, 1L))
    )
})
