test_that("%?% and %:% select scalar branches", {
    expect_identical(TRUE %?% 1 %:% 2, 1)
    expect_identical(FALSE %?% 1 %:% 2, 2)
})

test_that("%?% and %:% work on vectors", {
    result <- (c(TRUE, FALSE, NA)) %?% c("x", "y", "z") %:% c("a", "b", "c")
    expect_identical(result, c("x", "b", NA_character_))
})

test_that("%:% validates the left-hand side", {
    err <- testthat::capture_error("a" %:% "b")$message
    expect_equal(
        err,
        "`%:%` must be used immediately after `%?%`."
    )
})

test_that("%:% handles recycling of inputs", {
    expect_identical(c(TRUE, FALSE, TRUE) %?% 1 %:% 2, c(1, 2, 1))
})

test_that("%?% can return objects of different types", {
    df <- data.frame(a = 1)
    expect_identical(TRUE %?% df %:% "missing", df)
})
