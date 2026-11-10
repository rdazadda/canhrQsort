# The Bayesian page runs each fit in a short-lived background R process.
# These tests pin down the two properties that keep that cheap and correct:
# the job needs only bayesqm on the other side, and runs resolve, fail
# cleanly, and go one at a time.

wait_for_promise <- function(p, timeout = 120) {
  state <- new.env()
  state$done <- FALSE
  promises::then(
    p,
    onFulfilled = function(value) {
      state$value <- value
      state$done <- TRUE
    },
    onRejected = function(error) {
      state$error <- error
      state$done <- TRUE
    }
  )
  deadline <- Sys.time() + timeout
  while (!state$done && Sys.time() < deadline) later::run_now(0.1)
  state
}

test_that("the Bayesian job runs in a fresh R process without this package", {
  skip_on_cran()
  skip_if_not_installed("bayesqm")
  skip_if_not_installed("callr")

  qd <- suppressMessages(load_grizzly_qdata())
  # 12 complete sorts keep the forced distribution intact and the fit quick
  Y <- t(as.matrix(qd@sorts))[, 1:12]
  rownames(Y) <- paste0("S", seq_len(nrow(Y)))
  args <- list(Y = Y, distribution = qd@distribution, mode = "single", K = 1L,
               k_min = 2L, k_max = 3L, q = 0.05, iterations = 600L,
               burn = 100L, thin = 5L, max_iterations = 600L, seed = 7L,
               sigma_scale = 1)

  remote <- callr::r(function(job, args) {
    res <- do.call(job, args)
    list(res = res, loaded = loadedNamespaces())
  }, args = list(job = bq_portable_job(), args = args))

  expect_false("canhrQsort" %in% remote$loaded)
  expect_true("bayesqm" %in% remote$loaded)

  # a 600-iteration fit trips the convergence gate warning by design
  local <- suppressWarnings(do.call(run_bayesqm_job, args))
  expect_identical(remote$res$K, local$K)
  expect_equal(remote$res$tables$loadings, local$tables$loadings)
  expect_equal(remote$res$tables$zscores, local$tables$zscores)
  expect_equal(remote$res$tables$array, local$tables$array)
})

test_that("background runs resolve, surface job errors, and go one at a time", {
  skip_on_cran()
  skip_if_not_installed("callr")
  skip_if_not_installed("promises")
  skip_if_not_installed("later")

  first <- bq_run_in_background(function() {
    started <- as.numeric(Sys.time())
    Sys.sleep(1)
    c(started, as.numeric(Sys.time()))
  }, list(), poll = 0.1)
  second <- bq_run_in_background(function() as.numeric(Sys.time()),
                                 list(), poll = 0.1)

  a <- wait_for_promise(first)
  b <- wait_for_promise(second)
  expect_null(a$error)
  expect_null(b$error)
  # the second run starts only after the first has finished
  expect_gte(b$value, a$value[2])
  expect_identical(.bq_background$running, 0L)

  failed <- wait_for_promise(
    bq_run_in_background(function() stop("sampler exploded"), list(),
                         poll = 0.1)
  )
  expect_s3_class(failed$error, "error")
  expect_match(conditionMessage(failed$error), "sampler exploded")
  expect_identical(.bq_background$running, 0L)
})
