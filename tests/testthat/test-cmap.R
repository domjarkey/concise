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
    expect_equal(
        cmap(purrr::set_names(1:3, letters[1:3]), ~ c(value = .x, name = .nm) ? df),
        structure(
            list(
                value = c("1", "2", "3"),
                name = c("a", "b", "c")
            ),
            class = c("tbl_df", "tbl", "data.frame"),
            row.names = c(NA, -3L)
        )
    )
    expect_equal(
        cmap(purrr::set_names(1:5, letters[1:5]), ~ data.frame(.x, .nm) ? df),
        structure(
            list(
                .x = 1:5,
                .nm = c("a", "b", "c", "d", "e")
            ),
            class = "data.frame",
            row.names = c(NA, -5L)
        )
    )

})

test_that("List mapping", {
    expect_equal(
        cmap(list(1:3, 4:6), ~ sum(.x) ? dbl),
        c(6, 15)
    )

    expect_error(
        cmap(list(alpha = 1:3, beta = 4:6), ~ alpha + beta),
        "object 'alpha' not found"
    )

    expect_equal(
        cmap(
            list(first = c(a = 1, b = 2), second = c(x = 3, y = 4)),
            ~ paste0("index = ", .i, "; name = ", .nm, "; sum = ", sum(.x))
        ),
        list(
            first = "index = 1; name = first; sum = 3",
            second = "index = 2; name = second; sum = 7"
        )
    )
})

test_that("Data frame mapping", {
    expect_error(
        cmap(data.frame(x = 6:10, y = c(1, 2, 1, 2, 1)), ~ if(y %% 2 == 0) {x} else {x + y}),
        "object 'y' not found"
    )

    expect_equal(
        cmap(data.frame(x = 6:10, y = c(1, 2, 1, 2, 1)), ~ sum(.x)),
        list(x = 40L, y = 7)
    )

    expect_equal(
        cmap(data.frame(x = 6:10, y = c(1, 2, 1, 2, 1)), ~ sum(.x) ? int),
        c(x = 40L, y = 7L)
    )

    # .nm pronoun
    expect_equal(
        cmap(tibble::tibble(x = c(a = 1, b = 2)), ~ paste0(.nm, .x)),
        list(x = c("x1", "x2"))
    )
    expect_error(
        cmap(tibble::tibble(x = c(a = 1, b = 2), x.nm = c("hello", "goodbye")), ~ paste0(x.nm, x)),
        "object 'x.nm' not found"
    )
    expect_equal(
        cmap(tibble::tibble(x = c(1, 2)), ~ .nm),
        list(x = "x")
    )
})

# TODO: test recursion with single arg function on first column in data, second
#       second column in data, w. and w/o explicit argument names in .this call
