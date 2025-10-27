test_that("ctibble matches tibble sequential evaluation", {
    expected <- tibble::tibble(x = 1:3, y = 2 * x)
    result <- ctibble(x = 1:3, y = 2 * x)

    expect_identical(result, expected)
})

test_that("ctibble evaluates concise formulas", {
    result <- ctibble(
        x = 1:3,
        y = 3:1,
        sum ~ x + y ? int
    )

    expect_identical(result$sum, c(4L, 4L, 4L))
})

test_that("ctibble pronouns include row index", {
    result <- ctibble(
        values = 5:7,
        row_id ~ .i ? int
    )

    expect_identical(result$row_id, 1:3)
})
