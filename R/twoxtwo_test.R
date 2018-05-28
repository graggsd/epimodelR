# # Function to tabulate data
# tabulate_data <- function(test_var, outcome, data) {
#     return(table(data[, test_var], data[, outcome]))
# }
#
# # Get the odds ratio from a simple tabulation
# get_simple_OR <- function(tab) {
#     return(c(Simple.OR = (tab[1, 1] * tab[2, 2]) / (tab[1, 2] * tab[2, 1])))
# }
#
# # Get the confidence interval from a simple tabulation
# get_simple_confint <- function(tab, OR) {
#     stopifnot(identical(dim(tab), c(2L, 2L)))
#     log_OR <- log(OR)
#     diff <- 1.96*sqrt(1/tab[1,1] + 1/tab[1,2] + 1/tab[2,1] + 1/tab[2,2])
#     return(c(`Simple.Lower` = exp(log_OR - diff),
#              `Simple.Upper` = exp(log_OR + diff)))
# }
#
# get_fisher_confint <- function(tab) {
#     out <- fisher.test(tab)$conf.int
#     names(out) <- c("Fisher.Lower", "Fisher.Upper")
#     return(out)
# }
#
# stats_2x2_one_var <- function(test_var, outcome, data, simple_OR = TRUE,
#                               simple_confint = TRUE, pearson_pval = TRUE,
#                               fisher_pval = TRUE, fisher_confint = TRUE) {
#
#     tab <- tabulate_data(test_var, outcome, data)
#     stopifnot(identical(dim(tab), c(2L, 2L)))
#
#     if (simple_OR) {
#         OR <- get_simple_OR(tab)
#     } else {
#         OR <- NULL
#     }
#
#     if (simple_confint) {
#         simple_confint <- get_simple_confint(tab, OR)
#     } else {
#         simple_confint <- NULL
#     }
#
#     if (pearson_pval) {
#         pearson_pval <- c(Pearson.pval = chisq.test(tab)$p.value)
#     } else {
#         pearson_pval <- NULL
#     }
#
#     if (fisher_pval) {
#         fisher_pval <- c(Fisher.pval = fisher.test(tab)$p.value)
#     } else {
#         fisher_pval <- NULL
#     }
#
#     if (fisher_confint) {
#         fisher_confint <- get_fisher_confint(tab)
#     } else {
#         fisher_confint <- NULL
#     }
#
#     return(c(OR, simple_confint, pearson_pval, fisher_pval, fisher_confint))
#
# }