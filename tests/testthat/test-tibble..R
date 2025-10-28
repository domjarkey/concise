test_that("tibble. matches tibble sequential evaluation", {
    expected <- tibble::tibble(x = 1:3, y = 2 * x)
    result <- tibble.(x = 1:3, y = 2 * x)

    expect_identical(result, expected)
})

test_that("tibble. supports both standard and concise column definitions", {
    # Standard evaluation should mirror tibble::tibble
    expect_identical(
        tibble.(x = 1:3, y = list(4, NULL, 6)),
        tibble::tibble(x = 1:3, y = list(4, NULL, 6))
    )

    # context_lambda expressions supplied with = should evaluate row wise
    expect_identical(
        tibble.(
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
        tibble.(
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
        tibble.(
            values = list(4, NULL, 6),
            values ~ is.null(values)
        ),
        tibble::tibble(
            values = purrr::map_lgl(list(4, NULL, 6), ~ is.null(.x))
        )
    )
})

test_that("tibble. evaluates concise formulas with multiple column inputs", {
    # Multi-column concise expressions should map over each row
    expect_identical(
        tibble.(
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
        tibble.(
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

test_that("tibble. pronouns expose names, indices, and column data", {
    # Named vectors should expose names through the .nm pronoun
    expect_identical(
        tibble.(x = c(a = 1, b = 2), x_names ~ x.nm),
        tibble::tibble(x = c(a = 1, b = 2), x_names = c("a", "b"))
    )

    # Unnamed vectors should surface NULL through the .nm pronoun
    expect_identical(
        tibble.(x = c(1, 2), x_names ~ x.nm),
        tibble::tibble(x = c(1, 2), x_names = list(NULL, NULL))
    )

    # .i and .I should both yield row indices in an ungrouped tibble
    row_indices <- tibble.(
        values = 5:7,
        local_row ~ .i ? int,
        global_row ~ .I ? int
    )
    expect_identical(row_indices$local_row, 1:3)
    expect_identical(row_indices$global_row, 1:3)

    # .N should provide the total number of rows for each evaluation
    expect_identical(
        tibble.(values = 1:4, total_rows ~ .N ? int)$total_rows,
        rep.int(4L, 4)
    )

    # .col should expose whole-column vectors for aggregate calculations
    cumulative <- tibble.(
        values = c(2, 4, 6),
        running_sum ~ sum(values.col[seq_len(.I)]) ? dbl
    )
    expect_identical(cumulative$running_sum, c(2, 6, 12))

    # .grp should surface list-columns containing the full column values
    grouped_view <- tibble.(
        values = c(10, 20, 30),
        group_values ~ values.grp
    )
    expect_identical(
        grouped_view$group_values,
        rep_len(list(c(10, 20, 30)), 3)
    )

    # .n should resolve to the last row index while .N returns row count
    final_indices <- tibble.(values = 1:4, last_index ~ .n ? int)
    expect_identical(final_indices$last_index, rep.int(4L, 4))
})

test_that("tibble. supports argument injection with ? syntax", {
    # Inject constants into concise lambdas using tuple syntax
    expect_identical(
        tibble.(x = 3:1, y ~ x + z ? (z = 10)),
        tibble::tibble(x = 3:1, y = as.numeric(13:11))
    )

    # Explicit output type declarations should coerce results appropriately
    expect_identical(
        tibble.(x = 3:1, y ~ x + z ? int & (z = 10)),
        tibble::tibble(x = 3:1, y = 13:11)
    )

    # Block syntax should allow multiple injected bindings alongside type coercion
    expect_identical(
        tibble.(x = 3:1, y ~ x + z - w ? {int ; z = 10 ; w = 1}),
        tibble::tibble(x = 3:1, y = 12:10)
    )

    # Inject values derived from existing columns into concise lambdas
    expect_identical(
        tibble.(x = 3:1, y ~ x + X ? int & (X = sum(x))),
        tibble::tibble(x = 3:1, y = 9:7)
    )

    # Block injection should also work for computed values
    expect_identical(
        tibble.(x = 3:1, y ~ x + X ? {int; X = sum(x)}),
        tibble::tibble(x = 3:1, y = 9:7)
    )

    # Local variables should require explicit injection to avoid masking columns
    local_x <- 1:10
    expect_identical(
        tibble.(x = 3:1, y ~ x + X ? int & (X = sum(!!local_x))),
        tibble::tibble(x = 3:1, y = 58:56)
    )

    # Block syntax must honour injections of local variables via !!
    expect_identical(
        tibble.(x = 3:1, y ~ x + X ? {int; X = sum(!!local_x)}),
        tibble::tibble(x = 3:1, y = 58:56)
    )
})

test_that("tibble. supports recursive concise lambdas via .this", {
    # Recursive lambdas without type coercion should return numeric results
    expect_identical(
        tibble.(
            value = 6:1,
            fib ~ if (value <= 2) {1} else {.this(value - 1) + .this(value - 2)}
        ),
        tibble::tibble(value = 6:1, fib = c(8, 5, 3, 2, 1, 1))
    )

    # Recursive lambdas with explicit integer coercion should return integer vectors
    expect_identical(
        tibble.(
            value = 6:1,
            fib ~ if (value <= 2) {z} else {.this(value - 1) + .this(value - 2)} ? {int ; z = 1}
        ),
        tibble::tibble(value = 6:1, fib = c(8L, 5L, 3L, 2L, 1L, 1L))
    )
})

test_that("tibble. interleaves standard and concise columns sequentially", {
    # Later standard columns should see concise results defined earlier
    result <- tibble.(
        base = 1:3,
        double ~ base * 2 ? int,
        sum = base + double,
        triple ~ sum + base ? int,
        quadruple = double * 2
    )

    expect_identical(
        result,
        tibble::tibble(
            base = 1:3,
            double = c(2L, 4L, 6L),
            sum = c(3L, 6L, 9L),
            triple = c(4L, 8L, 12L),
            quadruple = c(4, 8, 12)
        )
    )
})

test_that("tibble. pronouns remain accessible when column names overlap", {
    testthat::skip("This feature is not available")
    # Columns named after pronouns should not mask the pronoun bindings
    pronoun_vs_columns <- tibble.(
        .i = c("a", "b", "c"),
        .I = c("A", "B", "C"),
        .n = letters[1:3],
        .N = LETTERS[1:3],
        pronoun_snapshot ~ paste(.i, .I, .n, .N),
        column_snapshot ~ paste(!!rlang::sym(".i"), !!rlang::sym(".I"), !!rlang::sym(".n"), !!rlang::sym(".N"))
    )

    expect_identical(
        pronoun_vs_columns$pronoun_snapshot,
        c("1 1 3 3", "2 2 3 3", "3 3 3 3")
    )

    expect_identical(
        pronoun_vs_columns$column_snapshot,
        c("a A a A", "b B b B", "c C c C")
    )
})

test_that("tibble. exposes .col pronouns for newly created concise columns", {
    # Columns created via ~ should themselves expose .col aggregates
    result <- tibble.(
        values = c(2, 4, 6),
        cumulative ~ sum(values.col[seq_len(.I)]),
        total1 ~ sum(cumulative.col),
        total2 ~ sum(cumulative.col) ? dbl
    )

    expect_identical(result$cumulative, c(2, 6, 12))
    expect_identical(result$total1, rep_len(20, 3))
    expect_identical(result$total2, rep_len(20, 3))
})

test_that("tibble. supports every concise type helper", {
    # All ? helpers should coerce or retain types consistently
    typed <- tibble.(
        values = 1:3,
        as_chr ~ paste0("value_", values) ? chr,
        as_dbl ~ values * 2 ? dbl,
        as_lgl ~ values > 1 ? lgl,
        as_list ~ values + 1 ? list
    )

    expect_identical(typed$as_chr, c("value_1", "value_2", "value_3"))
    expect_identical(typed$as_dbl, c(2, 4, 6))
    expect_identical(typed$as_lgl, c(FALSE, TRUE, TRUE))
    expect_identical(typed$as_list, list(2, 3, 4))
})
