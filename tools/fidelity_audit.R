# Fidelity + functional audit: the dashboard's bayesqm integration must be
# bit-identical to direct bayesqm calls, and every path must function.
suppressMessages(pkgload::load_all(
  "C:/Users/rdazadda/OneDrive - University of Alaska/Desktop/canhrQsort", quiet = TRUE))
suppressMessages(library(bayesqm))

pass <- 0L; fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) { pass <<- pass + 1L; cat("  PASS:", label, "\n") }
  else { fail <<- fail + 1L; cat("  FAIL:", label, "\n") }
}

# A structured two-factor panel
set.seed(11)
J <- 17; N <- 12; distr <- c(1, 2, 3, 3, 3, 3, 1, 1)
F0 <- matrix(rnorm(J * 2), J, 2)
L0 <- matrix(0, N, 2); L0[1:6, 1] <- 1.4; L0[7:12, 2] <- 1.4
U <- L0 %*% t(F0) + matrix(rnorm(N * J), N, J)
qsr <- function(u) { o <- order(u); r <- integer(J); r[o] <- rep(seq_along(distr), distr); r }
Y <- apply(U, 1, qsr)
rownames(Y) <- paste0("S", 1:J); colnames(Y) <- paste0("P", 1:N)

cat("== 1. FIDELITY: dashboard job vs direct bayesqm (same seed) ==\n")
b <- canhrQsort:::run_bayesqm_job(Y, distr, "single", K = 2, k_min = 2, k_max = 3,
  q = 0.05, iterations = 2000, burn = 500, thin = 5, max_iterations = 2000,
  seed = 99, sigma_scale = 1)
qd <- bayesqm::qsort_data(Y, distribution = distr, validate = FALSE)
fit_direct <- bayesqm::fit_bayesian(qd, K = 2, iterations = 2000, burn = 500,
  thin = 5, max_iterations = 2000, seed = 99, sigma_scale = 1, quiet = TRUE)

check("posterior draws identical (F)",
      isTRUE(all.equal(b$fit$draws$F, fit_direct$draws$F)))
check("posterior draws identical (Lambda)",
      isTRUE(all.equal(b$fit$draws$Lambda, fit_direct$draws$Lambda)))
check("loadings table identical",
      isTRUE(all.equal(b$tables$loadings, bayesqm::compute_loadings(fit_direct))))
check("flags table identical",
      isTRUE(all.equal(b$tables$flags,
                       suppressMessages(bayesqm::compute_flags(fit_direct, q = 0.05)),
                       check.attributes = FALSE)))
check("zscores table identical",
      isTRUE(all.equal(b$tables$zscores, bayesqm::compute_zscores(fit_direct))))
check("factor array identical",
      isTRUE(all.equal(b$tables$array, bayesqm::compute_factor_array(fit_direct),
                       check.attributes = FALSE)))
check("qdc verdicts identical",
      isTRUE(all.equal(b$tables$qdc, bayesqm::compute_qdc(fit_direct, q = 0.05),
                       check.attributes = FALSE)))
cl_direct <- bayesqm::claims(fit_direct, q = 0.05)
check("claims identical",
      isTRUE(all.equal(b$tables$claims$flags, cl_direct$flags)) &&
      isTRUE(all.equal(b$tables$claims$distinguishing, cl_direct$distinguishing)))
check("gate report identical",
      isTRUE(all.equal(b$fit$gate, fit_direct$gate)))

cat("== 2. BRIDGE: QsortResults from the bundle ==\n")
qd_s4 <- canhrQsort:::qsort_data(t(Y), validate = FALSE)
res <- canhrQsort:::bayesqm_to_results(b, qd_s4)
check("bridge returns QsortResults", inherits(res, "QsortResults"))
check("bridge loadings dims N x K",
      all(dim(res@rotation$loadings) == c(N, 2)))
check("bridge loadings equal posterior means",
      isTRUE(all.equal(unname(res@rotation$loadings[, 1]),
                       b$tables$loadings$f1_loading)))
check("bridge factor scores rows = J", nrow(res@factor_scores) == J)
check("bridge flagging matches selected flags",
      sum(res@flagging) == sum(b$tables$flags$selected))

cat("== 3. LADDER + auto K = 1 fallback + rejudge ==\n")
b2 <- canhrQsort:::run_bayesqm_job(Y, distr, "ladder", K = 2, k_min = 2, k_max = 3,
  q = 0.05, iterations = 2000, burn = 500, thin = 5, max_iterations = 2000,
  seed = 7, sigma_scale = 1)
check("ladder bundle has selection + verdict",
      inherits(b2$ladder$selection, "bayesqm_selection") &&
      b2$ladder$verdict %in% c("selected", "single_viewpoint", "no_shared_structure",
                               "tension", "adequate_but_unsupported", "no_adequate_rung"))
check("no-selection verdicts land on K = 1 (except tension)",
      !is.na(b2$ladder$K_star) || identical(b2$ladder$verdict, "tension") || b2$K == 1L)
check("K provenance recorded",
      b2$K_source %in% c("model", "user", "fallback", "inspect"))
rj <- tryCatch(canhrQsort:::bq_rejudge(b2, q2 = 0.10), error = function(e) e)
check("bq_rejudge runs and re-selects at q = 0.10",
      !inherits(rj, "error") && identical(rj$q, 0.10) &&
      inherits(rj$ladder$selection, "bayesqm_selection"))

cat("== 4. WORKBOOK: 12 sheets (ladder) / 11 sheets (single) ==\n")
f1 <- tempfile(fileext = ".xlsx"); canhrQsort:::bq_write_workbook(f1, b, qd_s4)
f2 <- tempfile(fileext = ".xlsx"); canhrQsort:::bq_write_workbook(f2, b2, qd_s4)
s1 <- openxlsx::getSheetNames(f1); s2 <- openxlsx::getSheetNames(f2)
check("single workbook sheets",
      setequal(s1, c("Overview", "Loadings & Flags", "Statement Scores", "Factor Arrays",
                     "Distinguishing", "Consensus", "Contrasts", "Claims",
                     "Characteristics", "Diagnostics", "Person Check")))
check("ladder workbook adds Choice of K", "Choice of K" %in% s2)
ov <- openxlsx::read.xlsx(f2, sheet = "Overview", startRow = 3)
check("workbook records K provenance", "K provenance" %in% ov$Item)

cat("== 5. PLOTS: all bayesqm views render ==\n")
tmp <- tempdir()
gg_ok <- vapply(c("loadings", "flags", "contrasts", "array"), function(tp) {
  tryCatch({
    p <- ggplot2::autoplot(b$fit, type = tp) + theme_canhrqsort()
    ggplot2::ggsave(file.path(tmp, paste0("aud_", tp, ".png")), p,
                    width = 8, height = 6, dpi = 72)
    TRUE
  }, error = function(e) { message(tp, ": ", e$message); FALSE })
}, logical(1))
check("all four autoplot views render", all(gg_ok))
base_ok <- tryCatch({
  png(file.path(tmp, "aud_base.png"), width = 800, height = 600)
  bayesqm::plot_convergence(b$fit)
  dev.off()
  png(file.path(tmp, "aud_ppc.png"), width = 800, height = 400)
  bayesqm::plot_ppc(b$tables$checks)
  dev.off()
  png(file.path(tmp, "aud_person.png"), width = 700, height = 500)
  bayesqm::plot_person_check(b$tables$persons)
  dev.off()
  png(file.path(tmp, "aud_choice.png"), width = 800, height = 500)
  bayesqm::plot_choice_k(b2$ladder$selection)
  dev.off()
  TRUE
}, error = function(e) { message(e$message); FALSE })
check("all four base-graphics views render", isTRUE(base_ok))

cat("== 6. COLOR SCHEME hook ==\n")
old <- bayesqm::bayesqm_set_colors(list(
  dark = "#1a4a6f", accent = "#8F272C", grey = "grey40",
  gridgrey = "grey75", fill = "#E1ECF4",
  qual = get_theme_colors()$factor_colors))
check("UAF scheme accepted by bayesqm_set_colors",
      identical(bayesqm::bayesqm_colors()$qual[1], "#236192"))
bayesqm::bayesqm_set_colors(old)

cat("\n=== AUDIT RESULT:", pass, "passed,", fail, "failed ===\n")
if (fail > 0) quit(status = 1)
