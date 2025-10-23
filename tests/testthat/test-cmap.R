test_that("Atomic mapping", {
  # .x pronoun
  expect_equal(cmap(6:10, ~ .x^2), list(36, 49, 64, 81, 100))
  expect_equal(cmap(letters[1:3], ~ paste0(.x, "zzz")), list("azzz", "bzzz", "czzz"))

  # .i pronoun
  expect_equal(cmap(6:10, ~ .i^2), list(1, 4, 9, 16, 25))
  expect_equal(cmap(6:10, ~ .x * .i), list(6L, 14L, 24L, 36L, 50L))

  # .nm pronoun
  expect_equal(
    cmap(purrr::set_names(6:10, letters[6:10]), ~.nm),
    list(f = "f", g = "g", h = "h", i = "i", j = "j")
  )
  expect_equal(
    cmap(6:10, ~.nm),
    rep_len(list(NULL), 5)
  )

  # .col pronoun
  expect_equal(
    cmap_dbl(1:10, ~ mean(.col[max(.i - 3, 1):.i])),
    c(1, 1.5, 2, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5)
  )

  # .n pronoun
  expect_equal(
    cmap_lgl(1:10, ~ .x > .n / 2),
    rep(c(FALSE, TRUE), each = 5)
  )

  # type casting
  expect_type(cmap(6:10, ~ .x^2), "list")
  expect_type(cmap_int(6:10, ~ .x^2), "integer")
  expect_type(cmap_dbl(6:10, ~ .x^2), "double")
  expect_equal(
    cmap_chr(purrr::set_names(6:10, letters[6:10]), ~ paste(.nm, .i)),
    c(f = "f 1", g = "g 2", h = "h 3", i = "i 4", j = "j 5")
  )
  expect_type(cmap(letters[1:3], ~ paste0(.x, "zzz")), "list")
  expect_type(cmap_chr(letters[1:3], ~ paste0(.x, "zzz")), "character")
  expect_equal(cmap_lgl(0:1, ~.x), c(FALSE, TRUE))
  expect_equal(
    cmap_df(purrr::set_names(1:3, letters[1:3]), ~ c(value = .x, name = .nm)),
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
    cmap_df(purrr::set_names(1:5, letters[1:5]), ~ data.frame(.x, .nm)),
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
    cmap_dbl(list(1:3, 4:6), ~ sum(.x)),
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
    cmap(data.frame(x = 6:10, y = c(1, 2, 1, 2, 1)), ~ if (y %% 2 == 0) {
      x
    } else {
      x + y
    }),
    "object 'y' not found"
  )

  expect_equal(
    cmap(data.frame(x = 6:10, y = c(1, 2, 1, 2, 1)), ~ sum(.x)),
    list(x = 40L, y = 7)
  )

  expect_equal(
    cmap_int(data.frame(x = 6:10, y = c(1, 2, 1, 2, 1)), ~ sum(.x)),
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
    cmap(tibble::tibble(x = c(1, 2)), ~.nm),
    list(x = "x")
  )
})

test_that("Recursion", {
  expect_equal(
    cmap(1:10, ~ ifelse(.x <= 2, 1, .this(.x - 1) + .this(.x - 2))),
    list(1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
  )

  expect_equal(
    cmap(10:1, ~ ifelse(.x <= 2, .i, .this(.x = .x - 1) + .this(.x = .x - 2))),
    list(55L, 68L, 63L, 52L, 40L, 30L, 21L, 16L, 9L, 10L)
  )

  expect_equal(
    cmap(10:1, ~ ifelse(.x <= 2, .i, .this(.x = .x - 1, .i = .i - 1) + .this(.x = .x - 2))),
    list(-146, -41, 5, 22, 25, 23, 18, 15, 9L, 10L)
  )

  expect_equal(
    cmap_chr(purrr::set_names(1:4, letters[1:4]), ~ ifelse(.x == 1, "1", paste(.nm, .this(.x - 1)))),
    c(a = "1", b = "b 1", c = "c c 1", d = "d d d 1")
  )

  tree <- list(
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

  expect_equal(
    cmap_df(
      tree,
      ~ if (is.list(.x)) {
        purrr::pmap_df(
          list(.x, .nm = names(.x)),
          .this,
          path = paste0(path, "/", .nm)
        )
      } else {
        tibble::tibble(path = paste0(path, "/", .nm), value = .x)
      },
      path = "root"
    ),
    tibble::tibble(
      path = c("root/a/b", "root/a/c/d", "root/a/c/e", "root/f", "root/g/h"),
      value = 1:5
    )
  )
})

test_that("Constant functions", {
  expect_equal(
    cmap(1:3, ~5),
    list(5, 5, 5)
  )
})

test_that("Use ... to pass additional values", {
  # Pass constants
  expect_equal(
    cmap(1:3, ~ .x + z, z = 10),
    list(11, 12, 13)
  )

  # Pass transformation of data variable
  expect_equal(
    cmap(1:3, ~ .x + X, X = sum(.x)),
    list(7, 8, 9)
  )

  # Dumb stuff
  expect_equal(
    cmap(1:3, ~ .x + X, X = sum(.x), `+` = `/`),
    list(0.166666666666667, 0.333333333333333, 0.5)
  )

  .x <- 1:10

  # Differentiate local variables using {{ embrace
  expect_equal(
    cmap(1:3, ~ .x + X, X = sum({{ .x }})),
    list(56, 57, 58)
  )

  # Differentiate local variables using !! injection
  expect_equal(
    cmap(1:3, ~ .x + X, X = sum(!!.x)),
    list(56, 57, 58)
  )
})
