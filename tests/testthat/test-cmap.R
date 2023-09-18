test_that("Atomic mapping", {
    # ...1 pronoun
    expect_equal(cmap(6:10, ~ ...1 ^ 2), c(36, 49, 64, 81, 100))
    expect_equal(cmap(letters[1:3], ~ paste0(...1, "zzz")), c("azzz", "bzzz", "czzz"))

    # TODO: .x pronoun (when implemented)

    # .i pronoun
    expect_equal(cmap(6:10, ~ .i ^ 2), c(1, 4, 9, 16, 25))
    expect_equal(cmap(6:10, ~ ...1 * .i), c(6L, 14L, 24L, 36L, 50L))

    # type casting
    expect_type(cmap(6:10, ~ ...1 ^ 2), "double")
    expect_type(cmap(6:10, ~ ...1 ^ 2 ? int), "integer")
    expect_type(cmap(6:10, ~ ...1 ^ 2 ? dbl), "double")
    res <- suppressWarnings(cmap(6:10, ~ ...1 ^ 2 ? chr))
    expect_type(res, "character")
    expect_type(cmap(letters[1:3], ~ paste0(...1, "zzz")), "character")
    expect_type(cmap(letters[1:3], ~ paste0(...1, "zzz") ? chr), "character")
    expect_equal(cmap(0:1, ~ ...1 ? lgl), c(FALSE, TRUE))
    ## TODO: test df

})

test_that("List mapping", {
    # TODO: this one
})

test_that("Data frame mapping", {
    # TODO: and this
})

# TODO: test recursion with single arg function on first column in data, second
#       second column in data, w. and w/o explicit argument names in .this call
