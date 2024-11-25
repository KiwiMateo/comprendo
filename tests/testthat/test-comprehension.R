test_that("comprehension works", {
  # Mapping
  # Univariate tests
  testthat::expect_equal(.(x | x %in% 1:10), as.list(1:10))

  for (i in 1:10) {
    range <- sample(1000, 100)
    testthat::expect_equal(v(x| x %in% range), range)
    testthat::expect_equal(l(x| x %in% range), as.list(range))
    testthat::expect_equal(.(x| x %in% range), as.list(range))
    testthat::expect_equal(v(x| x %in% range), range)

    testthat::expect_equal(v(x^2| x %in% range), range^2)
    testthat::expect_equal(l(x^2| x %in% range), as.list(range^2))
    testthat::expect_equal(.(x^2| x %in% range), as.list(range^2))
    testthat::expect_equal(v(x^2| x %in% range), range^2)
  }

  # Bivariate
  for (i in 1:10) {
    range1 <- sample(1000, 20)
    range2 <- sample(letters, 20, replace = TRUE)
    testthat::expect_equal(sort(v(paste0(x, y)| x %in% range1 & y %in% range2)),
                           sort(outer(range1, range2, paste0)))
    testthat::expect_equal(v(paste0(x, y)| c(x, y) %in% zip(range1, range2)),
                           paste0(range1, range2))
  }

  # Filtering
  # Univariate
  for (i in 1:10) {
    range <- sample(1000, 100)
    testthat::expect_equal(v(x | x %in% range, x %% 2 == 0),
                           range[range %% 2 == 0])
    testthat::expect_equal(v(x | x %in% range, x %% 2 == 0, x^2 < 50000),
                           range[range %% 2 == 0 & range^2 < 50000])
  }

  x <- v(x * y | x %in% 1:10 & y %in% 10:1, x %% 2 == 0, y %% 3 == 0)
  testthat::expect_length(x, 15)
  testthat::expect_true(all(x %% 6 == 0))
  testthat::expect_equal(sort(x), sort(outer(seq(2, 10, 2),
                                             seq(3, 10, 3))))




  # variables from call environment
  a <- 1:3
  testthat::expect_equal(v(sum(a * c(1, x, x^2)) | x %in% 1:10),
                         sapply(1:10, function(x) 1 + 2 * x + 3 * x^2))
  upper <- 5
  testthat::expect_equal(v(x | x %in% 1:10, x < upper), seq_len(upper - 1))
  testthat::expect_equal(v(sum(a * c(1, x, x^2)) | x %in% 1:10, x < upper),
                         sapply(seq_len(upper - 1),
                                function(x) 1 + 2 * x + 3 * x^2))

  # complex filters
  pythagorean_triples <- l(c(x, y, z) | x %in% 1:20 & y %in% x:20 & z %in% y:20,
                           x^2 + y^2 == z^2)
  testthat::expect_length(unique(pythagorean_triples), 6)
  testthat::expect_true(all(sapply(pythagorean_triples,
                                   function(x) x[1]^2 + x[2]^2 == x[3]^2)))
  # ...

  # Errors
  testthat::expect_error(l(c(x, y) | x %in% 1:10, y %in% 1:10))
  testthat::expect_error(v(c(x, y) | x %in% 1:10 & y %in% 1:10))
  # ...

  # scoping
  a <- 2
  f <- function(b) v(a * x + b | x %in% 1:10)
  testthat::expect_equal(f(2), 2 * (1:10) + 2)

})
