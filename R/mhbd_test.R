mhbd_test_single <- function(test_var, data, outcome, strata) {

    tab <- table(data[, outcome],
                 data[, test_var],
                 data[, strata])

    data.frame(test_var = test_var,
               common.OR = tryCatch(mantelhaen.test(tab)$estimate,
                                    error = function(e) return(NA)),
               MH.p.value = tryCatch(mantelhaen.test(tab)$p.value,
                                     error = function(e) return(NA)),
               BD.p.value = tryCatch(DescTools::BreslowDayTest(tab)$p.value,
                                     error = function(e) return(NA)),
               stringsAsFactors = FALSE)
}

#' @export
mhbd_test <- function(data, test_vars, outcome, strata) {
    out <- as.data.frame(t(sapply(test_vars,
                                  mhbd_test_single,
                                  data = data,
                                  outcome = outcome,
                                  strata = strata)))
    rownames(out) <- 1:nrow(out)
    out[, 1] <- as.character(out[, 1])
    out[, 2:4] <- as.numeric(out[, 2:4])
    return(out)
}
