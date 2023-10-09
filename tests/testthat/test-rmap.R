test_that("Atomic mapping", {
    # ...1 pronoun
    expect_equal(rmap(6:10, ~ ...1 ^ 2), c(36, 49, 64, 81, 100))
    expect_equal(rmap(letters[1:3], ~ paste0(...1, "zzz")), c("azzz", "bzzz", "czzz"))

    # TODO: .x pronoun (when implemented)

    # .i pronoun
    expect_equal(rmap(6:10, ~ .i ^ 2), c(1, 4, 9, 16, 25))
    expect_equal(rmap(6:10, ~ ...1 * .i), c(6L, 14L, 24L, 36L, 50L))

    # TODO: test .i, test .i when .i exists in data columns

    # type casting
    expect_type(rmap(6:10, ~ ...1 ^ 2), "double")
    expect_type(rmap(6:10, ~ ...1 ^ 2 ? int), "integer")
    expect_type(rmap(6:10, ~ ...1 ^ 2 ? dbl), "double")
    expect_type(
        suppressWarnings(rmap(6:10, ~ ...1 ^ 2 ? chr)),
        "character"
    )
    expect_type(rmap(letters[1:3], ~ paste0(...1, "zzz")), "character")
    expect_type(rmap(letters[1:3], ~ paste0(...1, "zzz") ? chr), "character")
    expect_equal(rmap(0:1, ~ ...1 ? lgl), c(FALSE, TRUE))
    expect_equal(
        rmap(purrr::set_names(1:3, letters[1:3]), ~ c(value = ...1, name = ...1.nm) ? df),
        structure(
            list(
                value = c("1", "2", "3"),
                name = c("a", "b", "c")
            ),
            class = c("tbl_df", "tbl", "data.frame"),
            row.names = c(NA, -3L)
        )
    )

})

test_that("List mapping", {
    expect_equal(
        rmap(list(1:3, 4:6), ~ ...1 + ...2),
        c(5, 7, 9)
    )
    expect_equal(
        rmap(list(alpha = 1:3, beta = 4:6), ~ alpha + beta),
        c(5, 7, 9)
    )

    # .nm pronoun
    expect_equal(
        rmap(
            list(first = c(a = 1, b = 2), second = c(x = 3, y = 4)),
            ~ paste0(first.nm, second.nm)
        ),
        c("ax", "by")
    )
})

test_that("Data frame mapping", {
    expect_equal(
        rmap(data.frame(x = 6:10, y = c(1, 2, 1, 2, 1)), ~ if(y %% 2 == 0) {x} else {x + y}),
        c(7, 7, 9, 9, 11)
    )

    expect_equal(
        rmap(data.frame(x = 6:10, y = c(1, 2, 1, 2, 1)), ~ if(y %% 2 == 0) {x} else {x + y} ? int),
        c(7L, 7L, 9L, 9L, 11L)
    )

    # .nm pronoun
    expect_equal(
        rmap(tibble::tibble(x = c(a = 1, b = 2)), ~ paste0(x.nm, x)),
        c(a = "a1", b = "b2")
    )
    expect_equal(
        rmap(tibble::tibble(x = c(a = 1, b = 2), x.nm = c("hello", "goodbye")), ~ paste0(x.nm, x)),
        c(a = "hello1", b = "goodbye2")
    )
    expect_equal(
        rmap(tibble::tibble(x = c(1, 2)), ~ x.nm),
        c(NA_character_, NA_character_)
    )
})

# TODO: test recursion with single arg function on first column in data, second
#       second column in data, w. and w/o explicit argument names in .this call

test_that("Fails correctly", {
    expect_error(
        rmap(list(1:3, 4:7), ~ ...1 + ...2),
        "All elements of .l must be of equal length"
    )
})

test_that("Constant functions", {
    expect_equal(
        rmap(1:3, ~5),
        c(5, 5, 5)
    )
})
