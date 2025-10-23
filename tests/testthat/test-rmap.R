test_that("Atomic mapping", {
  # ...1 pronoun
  expect_equal(rmap(6:10, ~ ...1^2), c(36, 49, 64, 81, 100))
  expect_equal(rmap(letters[1:3], ~ paste0(...1, "zzz")), c("azzz", "bzzz", "czzz"))

  # TODO: .x pronoun (when implemented)

  # .i pronoun
  expect_equal(rmap(6:10, ~ .i^2), c(1, 4, 9, 16, 25))
  expect_equal(rmap(6:10, ~ ...1 * .i), c(6L, 14L, 24L, 36L, 50L))

  # .N pronoun
  expect_equal(
    rmap_lgl(1:10, ~ ...1 > .N / 2),
    rep(c(FALSE, TRUE), each = 5)
  )

  # TODO: test .i, test .i when .i exists in data columns

  # type casting
  expect_type(rmap(6:10, ~ ...1^2), "double")
  expect_type(rmap_int(6:10, ~ ...1^2), "integer")
  expect_type(rmap_dbl(6:10, ~ ...1^2), "double")
  expect_type(
    suppressWarnings(rmap_chr(6:10, ~ ...1^2)),
    "character"
  )
  expect_type(rmap(letters[1:3], ~ paste0(...1, "zzz")), "character")
  expect_type(rmap(letters[1:3], ~ paste0(...1, "zzz")), "character")
  expect_equal(rmap_lgl(0:1, ~...1), c(FALSE, TRUE))
  expect_equal(
    rmap_df(purrr::set_names(1:3, letters[1:3]), ~ c(value = ...1, name = ...1.nm)),
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
    rmap(data.frame(x = 6:10, y = c(1, 2, 1, 2, 1)), ~ if (y %% 2 == 0) {
      x
    } else {
      x + y
    }),
    c(7, 7, 9, 9, 11)
  )

  expect_equal(
    rmap_int(data.frame(x = 6:10, y = c(1, 2, 1, 2, 1)), ~ if (y %% 2 == 0) {
      x
    } else {
      x + y
    }),
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
    rmap(tibble::tibble(x = c(1, 2)), ~x.nm),
    list(NULL, NULL)
  )
})

test_that("Recursion", {
  df <- tibble::tibble(
    tree = list(
      a = list(
        b = 1,
        c = list(
          d = 2,
          e = 3
        )
      ),
      f = 4,
      g = list(h = 5)
    )
  )

  expect_equal(
    rmap_df(
      df,
      ~ if (is.list(tree)) {
        purrr::pmap_df(
          list(tree, tree.nm = names(tree)),
          .this,
          path = paste0(path, "/", tree.nm)
        )
      } else {
        tibble::tibble(path = paste0(path, "/", tree.nm), value = tree)
      },
      path = "root"
    ),
    tibble::tibble(
      path = c("root/a/b", "root/a/c/d", "root/a/c/e", "root/f", "root/g/h"),
      value = 1:5
    )
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

test_that("Groups hold", {
  df <- tibble::tibble(
    colour = c("blue", "green", "blue", "blue", "green", "blue", "red", "blue"),
    shape = rep(c("triangle", "square"), 4),
    number = c(8, 5, 1, 2, 4, 7, 3, 6)
  )

  expect_equal(
    df |>
      dplyr::group_by(colour) |>
      rmap(~.i),
    c(1L, 1L, 2L, 3L, 2L, 4L, 1L, 5L)
  )

  expect_equal(
    df |>
      dplyr::group_by(colour, shape) |>
      rmap(~.i),
    c(1L, 1L, 2L, 1L, 1L, 2L, 1L, 3L)
  )

  expect_equal(
    df |>
      dplyr::group_by(colour) |>
      rmap(~.I),
    1:8
  )

  expect_equal(
    df |>
      dplyr::group_by(colour) |>
      rmap(~.n),
    c(5L, 2L, 5L, 5L, 2L, 5L, 1L, 5L)
  )

  expect_equal(
    df |>
      dplyr::group_by(colour) |>
      rmap(~number.grp),
    list(
      c(8, 1, 2, 7, 6),
      c(5, 4),
      c(8, 1, 2, 7, 6),
      c(8, 1, 2, 7, 6),
      c(5, 4),
      c(8, 1, 2, 7, 6),
      3,
      c(8, 1, 2, 7, 6)
    )
  )

  expect_equal(
    df |>
      dplyr::group_by(colour) |>
      rmap(~ mean(number.col[max(.I - 1, 1):.I])),
    c(8, 6.5, 3, 1.5, 3, 5.5, 5, 4.5)
  )
})

test_that("Use ... to pass additional values", {
  # Pass constants
  expect_equal(
    tibble::tibble(x = 1:3) |>
      rmap(~ x + z, z = 10),
    11:13
  )

  # Pass transformations of data variable(s) and pronouns
  expect_equal(
    tibble::tibble(x = 1:3) |>
      rmap(~ x + X, X = sum(x)),
    7:9
  )

  expect_equal(
    tibble::tibble(x = 1:3, y = 4:6) |>
      rmap(~ x + X - Y, X = sum(x), Y = prod(y)),
    (-113):(-111)
  )

  expect_equal(
    tibble::tibble(x = 1:3, y = 4:6) |>
      rmap(~ x + I, I = sum(.i)),
    7:9
  )

  expect_equal(
    list(a = purrr::set_names(letters[1:3], LETTERS[1:3]), b = letters[1:3]) |>
      tibble::as_tibble() |>
      rmap(~Z, Z = paste0(a.nm, collapse = "")),
    c("ABC", "ABC", "ABC")
  )

  # Differentiate local variables using !! injection
  x <- 1:10

  expect_equal(
    tibble::tibble(x = 1:3) |>
      rmap(~ x + X, X = sum(!!x)),
    56:58
  )

  # Specify other objects in lambda function's execution environment
  expect_equal(
    tibble::tibble(
      determiner = c("the", "a", "those"),
      adjective = c("quick", "slow", "naughty"),
      noun = c("fox", "loris", "children")
    ) |> rmap(~ determiner + adjective + noun, `+` = paste),
    c("the quick fox", "a slow loris", "those naughty children")
  )
})
