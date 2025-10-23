test_that("%as% converts to atomic vector types", {
    expect_type((1L:3L) %as% dbl, "double")
    expect_true(is.integer(c(1, 2) %as% int))
    expect_identical(c(TRUE, FALSE) %as% int, c(1L, 0L))
    expect_equal(c(1, 2) %as% "character", c("1", "2"))
})

test_that("%as% handles data frame conversions", {
    column_df <- list(a = 1:2, b = 3:4) %as% dfc
    expect_s3_class(column_df, "tbl_df")
    expect_named(column_df, c("a", "b"))

    base_df <- list(a = 1:2, b = 3:4) %as% data.frame
    expect_s3_class(base_df, "data.frame")

    row_df <- list(
        list(a = 1, b = 2),
        list(a = 3, b = 4)
    ) %as% dfr

    expect_s3_class(row_df, "tbl_df")
    expect_equal(nrow(row_df), 2)
    expect_named(row_df, c("a", "b"))
})
