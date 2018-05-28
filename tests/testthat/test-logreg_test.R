context("logreg_test")

set.seed(1)
test_data <-
    data.frame(dependent = rbinom(n = 100, prob = 0.5, size = 1),
               independent1 = rbinom(n = 100, prob = 0.5, size = 1),
               independent2 = factor(rbinom(n=100, prob = 0.5, size = 2)),
               covar1 = rnorm(n = 100, mean = 50, sd = 10),
               covar2 = factor(rbinom(n=100, prob = 0.5, size = 2)))

test_out <-
    logreg_test(test_data,
                c("independent1", "independent2"),
                c("covar1", "covar2"),
                "dependent")

full_model1 <- glm(dependent ~ independent1 + covar1 + covar2,
                   data = test_data,
                   family = "binomial")

full_model2 <- glm(dependent ~ independent2 + covar1 + covar2,
                   data = test_data,
                   family = "binomial")

full_model_0 <- glm(dependent ~ covar1 + covar2,
                    data = test_data,
                    family = "binomial")

test_that("Calculation of unadjusted OR is accurate", {

    simple_model1 <- glm(dependent ~ independent1,
                         data = test_data,
                         family = "binomial")

    expect_equivalent(test_out[1, 3],
                      exp(coefficients(simple_model1))[2])

    simple_model2 <- glm(dependent ~ independent2,
                         data = test_data,
                         family = "binomial")

    expect_equivalent(test_out[2:3, 3],
                      exp(coefficients(simple_model2))[2:3])

})

test_that("Calculation of adjusted OR is accurate", {

    expect_equivalent(test_out[1, 4], exp(coefficients(full_model1))[2])
    expect_equivalent(test_out[2:3, 4], exp(coefficients(full_model2))[2:3])

})

test_that("Likelihood ratio upper and lower bounds are accurate", {
    expect_equivalent(test_out[1, 8:9],
                      suppressMessages(exp(confint(full_model1))[2, ]))
    mod_2_LR_confints <-
        as.data.frame(suppressMessages(exp(confint(full_model2))[2:3, ]))
    expect_equivalent(test_out[2:3, 8:9],
                      mod_2_LR_confints)
})

test_that("Likelihood ratio P-values are accurate", {
    expect_equivalent(lmtest::lrtest(full_model_0, full_model1)$`Pr(>Chisq)`[2],
                      test_out[1, 10])
    #################################################################
    #################################################################
    #################################################################
    # At some point, need to add testing for multi-level categoricals
})

test_that("Wald P-values are accurate", {
    expect_equivalent(summary(full_model1)$coefficients[2, 4],
                      test_out[1, 7])
    expect_equivalent(summary(full_model2)$coefficients[2:3, 4],
                      test_out[2:3, 7])
})

