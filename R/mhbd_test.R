mhbd_test_single <- function(test_var, data, outcome, strata) {

    tab <- table(data[, outcome],
                 data[, test_var],
                 data[, strata])

    data.frame(common.OR = tryCatch(mantelhaen.test(tab)$estimate,
                                    error = function(e) return(NA)),
               MH.p.value = tryCatch(mantelhaen.test(tab)$p.value,
                                     error = function(e) return(NA)),
               BD.p.value = tryCatch(DescTools::BreslowDayTest(tab)$p.value,
                                     error = function(e) return(NA)),
               stringsAsFactors = FALSE)
}

#' @export
mhbd_test <- function(data, test_vars, outcome, strata) {
    return(as.data.frame(t(sapply(test_vars, mhbd_test_single,
                                  data = data,
                                  outcome = outcome,
                                  strata = strata))))
}
