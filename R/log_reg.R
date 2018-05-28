get_tmp_df <- function(data, test_var, covars, outcome) {
    return(na.omit(data[, c(outcome, test_var, covars)]))
}

make_mat <- function(data, test_var, covars) {
    return(list(mat = model.matrix(as.formula(paste0("~",
                                                     paste(c(test_var, covars),
                                                           collapse = "+"))),
                                   data = data),
                N_levels = ifelse(is.numeric(data[, test_var]),
                                  1,
                                  length(unique(data[, test_var])) - 1)))
}

make_fits <- function(x, outcome_vec, test_var) {
    # Retrieve model
    mat <- x$mat
    # Make the complete model
    fit_full <- glm(outcome_vec ~ mat, family = "binomial")
    # Identify the columns that correspond to the test_var
    idx <- 2:(x$N_levels + 1)
    # Make the unadjusted model
    mat_0 <- mat[, -((idx[length(idx)] + 1):ncol(mat))]
    fit_0 <- glm(outcome_vec ~ mat_0,
                 family = "binomial")
    # Create an empty list to match the number of levels of the test
    # variable - 1
    fit_X <- as.list(rep(NA, length(idx)))
    # Give names to this list based on the corresponding columns in the
    # design matrix
    names(fit_X) <- colnames(mat)[idx]
    # For each level of the test variable (minus the base value),
    # generate a model based on a design matrix omiting the corresponding
    # column
    for (i in 1:length(idx)) {
        fit_X[[i]] <- glm(outcome_vec ~ mat[, -idx[i]], family = "binomial")
    }
    return(list(fit_0 = fit_0,
                fit_full = fit_full,
                fit_X = fit_X,
                N_levels = x$N_levels))
}

get_lr_pval <- function(fit_full, fit_1) {
    p.val <-
        tryCatch(
            lmtest::lrtest(fit_full, fit_1)$`Pr(>Chisq)`[2],
            error = function(e) NA
        )
    names(p.val) <- "LR.pval"
    return(p.val)
}

get_lr_pvals <- function(fits) {
    out <- data.frame(NULL)
    for(i in 1:length(fits$fit_X)) {
        out[names(fits$fit_X)[i], "LR.pval"] <-
            get_lr_pval(fits$fit_full, fits$fit_X[[i]])
    }
    return(out)
}

get_lr_confints <- function(fits) {
    idx <- 3:(fits$N_levels + 2)
    suppressMessages(
        out <-
            exp(matrix(confint(fits$fit_full, idx),
                       nrow = length(idx)))
    )
    rownames(out) <- sub("mat", "", names(fits$fit_full$coefficients[idx]))
    colnames(out) <- c("LR.Lower", "LR.Upper")
    out <- as.data.frame(out, stringsAsFactors = FALSE)
    return(out)
}

get_wald_pvals <- function(fits) {
    idx <- 2:(fits$N_levels + 1)
    summary_mat <- summary(fits$fit_full)$coefficients
    out <- matrix(summary_mat[idx, "Pr(>|z|)"], ncol = 1)
    rownames(out) <- sub("mat", "", rownames(summary_mat)[idx])
    colnames(out) <- c("Wald.pval")
    out <- as.data.frame(out, stringsAsFactors = FALSE)
    return(out)
}

get_wald_confints <- function(fits) {
    idx <- 3:(fits$N_levels + 2)
    suppressMessages(
        out <-
            exp(matrix(confint.default(fits$fit_full, idx),
                       nrow = length(idx)))
    )
    rownames(out) <- sub("mat", "", names(fits$fit_full$coefficients[idx]))
    colnames(out) <- c("Wald.Lower", "Wald.Upper")
    out <- as.data.frame(out, stringsAsFactors = FALSE)
    return(out)
}

get_adj_ORs <- function(fits) {
    idx <- 3:(fits$N_levels + 2)
    out <- exp(matrix(fits$fit_full$coefficients[idx], nrow = length(idx)))
    rownames(out) <- sub("mat", "", names(fits$fit_full$coefficients[idx]))
    colnames(out) <- c("adj.OR")
    out <- as.data.frame(out, stringsAsFactors = FALSE)
    return(out)
}

get_unadj_ORs <- function(fits) {
    idx <- 3:(fits$N_levels + 2)
    out <- exp(matrix(fits$fit_0$coefficients[idx], nrow = length(idx)))
    rownames(out) <- sub("mat_0", "", names(fits$fit_0$coefficients[idx]))
    colnames(out) <- c("unadj.OR")
    out <- as.data.frame(out, stringsAsFactors = FALSE)
    return(out)
}

logreg_test_single <- function(test_var, data, covars, outcome) {
    # Create a temporary data.frame devoid of missing values
    tmp_df <- get_tmp_df(data, test_var, covars, outcome)
    # Make a base model.matrix
    mat <- make_mat(tmp_df, test_var, covars)
    # Fit models
    fits <- tryCatch(
        make_fits(mat, tmp_df[, outcome], test_var),
        error = function(e) NA
    )
    out <-
        cbind(get_unadj_ORs(fits),
              get_adj_ORs(fits),
              get_wald_confints(fits),
              get_wald_pvals(fits),
              get_lr_confints(fits),
              get_lr_pvals(fits))
    out <- cbind(test_vars = test_var,
                 level = sub(paste0("^", test_var), "", rownames(out)),
                 out)
    out$test_vars <- as.character(out$test_vars)
    out$level <- as.character(out$level)
    return(out)
}

#' @export
logreg_test <- function(data, test_vars, covars, outcome) {
    out <- data.frame(NULL)
    col_names <- c("test_vars", "level",
                   "unadj.OR", "adj.OR", "Wald.Lower", "Wald.Upper",
                   "Wald.pval", "LR.Lower", "LR.Upper",
                   "LR.pval")
    for (test_var in test_vars) {

        tmp_out <- tryCatch(
            logreg_test_single(test_var, data, covars, outcome),
            error = function(e) {
                data.frame(matrix(data = c(test_var, level = "", rep(NA, 8)),
                                  ncol = 10,
                                  dimnames = list(test_var, col_names)),
                           stringsAsFactors = FALSE)
            }
        )
        out <- rbind(out, tmp_out)
        rownames(out) <- 1:nrow(out)
    }
    return(out)
}