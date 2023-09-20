test_that("Atomic mapping", {
    # .x pronoun
    expect_equal(cmap(6:10, ~ .x ^ 2), list(36, 49, 64, 81, 100))
    expect_equal(cmap(letters[1:3], ~ paste0(.x, "zzz")), list("azzz", "bzzz", "czzz"))

    # .i pronoun
    expect_equal(cmap(6:10, ~ .i ^ 2), list(1, 4, 9, 16, 25))
    expect_equal(cmap(6:10, ~ .x * .i), list(6L, 14L, 24L, 36L, 50L))

    # TODO: test .i, test .i when .i exists in data columns

    # .nm pronoun
    expect_equal(
        cmap(purrr::set_names(6:10, letters[6:10]), ~ .nm),
        list(f = "f", g = "g", h = "h", i = "i", j = "j")
    )
    expect_equal(
        cmap(6:10, ~ .nm),
        list(NA_character_, NA_character_, NA_character_, NA_character_,
             NA_character_)
    )

    # type casting
    expect_type(cmap(6:10, ~ .x ^ 2), "list")
    expect_type(cmap(6:10, ~ .x ^ 2 ? int), "integer")
    expect_type(cmap(6:10, ~ .x ^ 2 ? dbl), "double")
    expect_equal(
        cmap(purrr::set_names(6:10, letters[6:10]), ~ paste(.nm, .i) ? chr),
        c(f = "f 1", g = "g 2", h = "h 3", i = "i 4", j = "j 5")
    )
    expect_type(cmap(letters[1:3], ~ paste0(.x, "zzz")), "list")
    expect_type(cmap(letters[1:3], ~ paste0(.x, "zzz") ? chr), "character")
    expect_equal(cmap(0:1, ~ .x ? lgl), c(FALSE, TRUE))
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
