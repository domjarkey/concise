test_that("%as% converts to atomic vector types", {
    expect_type((1L:3L) %as% dbl, "double")
    expect_type((1:3) %as% "double", "double")
    expect_type((1:3) %as% "numeric", "double")

    expect_true(is.integer(c(1, 2) %as% int))
    expect_type(c(1, 2) %as% "integer", "integer")

    expect_identical(c(TRUE, FALSE) %as% int, c(1L, 0L))
    expect_equal(c(1, 2) %as% "character", c("1", "2"))
    expect_type(c(TRUE, FALSE) %as% "logical", "logical")

    expect_identical(c(1, 2) %as% chr, c("1", "2"))
    expect_type(c(1, 2) %as% "character", "character")

    expect_type(1:2 %as% "complex", "complex")
    expect_type(1:2 %as% "list", "list")
    expect_identical(
        1:2 %as% "raw",
        as.raw(1:2)
    )

    expression_result <- c("foo", "bar") %as% "expression"
    expect_identical(typeof(expression_result), "expression")
    expect_identical(as.character(expression_result), c("foo", "bar"))

    symbol_result <- "foo" %as% "symbol"
    expect_identical(typeof(symbol_result), "symbol")
    expect_identical(as.character(symbol_result), "foo")

    pairlist_result <- list(a = 1, b = 2) %as% "pairlist"
    expect_true(is.pairlist(pairlist_result))
    expect_identical(as.list(pairlist_result), list(a = 1, b = 2))
})

test_that("%as% handles data frame conversions", {
    column_df <- list(a = 1:2, b = 3:4) %as% dfc
    expect_s3_class(column_df, "tbl_df")
    expect_named(column_df, c("a", "b"))

    column_df <- list(a = 1:2, b = 3:4) %as% columnwise_df
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

    row_df <- list(
        list(a = 1, b = 2),
        list(a = 3, b = 4)
    ) %as% rowwise_df

    expect_s3_class(row_df, "tbl_df")
    expect_equal(nrow(row_df), 2)
    expect_named(row_df, c("a", "b"))
})

test_that("%as% dfr preserves names for data frame like inputs", {
    df_input <- data.frame(a = 1:2, b = 3:4)
    expect_equal(df_input %as% dfr, tibble::as_tibble(df_input))

    tibble_input <- tibble::tibble(a = 1:2, b = 3:4)
    expect_equal(tibble_input %as% dfr, tibble::as_tibble(tibble_input))

    empty_df <- data.frame(a = integer(), b = character())
    expect_equal(empty_df %as% dfr, tibble::as_tibble(empty_df))

    expect_equal(1:3 %as% dfr, tibble::tibble(value = 1:3))
})

test_that("%as% repairs names for tibble-like conversions", {
    unnamed_list <- list(1, 2)

    tibble_df <- unnamed_list %as% df
    expect_s3_class(tibble_df, "tbl_df")
    expect_identical(names(tibble_df), c("V1", "V2"))

    tibble_columns <- unnamed_list %as% dfc
    expect_s3_class(tibble_columns, "tbl_df")
    expect_identical(names(tibble_columns), c("V1", "V2"))

    tibble_rows <- list(list(1, 2)) %as% dfr
    expect_s3_class(tibble_rows, "tbl_df")
    expect_identical(names(tibble_rows), c("V1", "V2"))

    base_df <- unnamed_list %as% data.frame
    expect_s3_class(base_df, "data.frame")
    expect_identical(names(base_df), c("X1", "X2"))
})
