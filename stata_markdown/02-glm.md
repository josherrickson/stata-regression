^#^ GLM

^#^^#^ Logistic Regression

Let's violate one of the [assumptions](#relationship-is-linear-and-additive). Instead of the relationship being linear, we can generalize to allow the
relationship to be any functional form. These types of models are called "Generalized Linear Models" or "GLMs", mainly because we can transform the
model to be linear in some sense. Logistic regression is one specific form of a GLM, others in Poisson and Negative Binomial regression.

Logistic regression is used when the outcome is dichotomous - either a positive outcome (1) or a negative outcome (0). For example, presence or
absence of some disease. The equation for this model is

^$$^
    P(Y = 1 | X) = \frac{1}{1 + e^{-(\beta_0 + \beta_1X_1 + \cdots + \beta_pX_p)}} + \epsilon.
^$$^

The right hand side is what's known as the "logit" function, that is, logit(^$^z^$^) = ^$^\frac{1}{1 + e^{-z}}^$^. Understanding this form isn't
crucial to understanding the model, but there are two quirks that need to be examined.

1. The relationship between the ^$^X^$^'s and the outcome is nonlinear.
2. Note that the left-hand side of this model is not just ^$^Y^$^, but rather, the probability of ^$^Y^$^ being 1 (a positive result) given the
   predictors. Unlike linear regression where we are explicitly predicting the outcome, a logistic model is instead trying to predict everyone's
   probability of a positive outcome.

^#^^#^^#^ Fitting the logistic model

We can fit this using the `logit` command in State. It works very similarly to `regress`. Let's predict whether a car is foreign based on headroom and
gear ratio again.

~~~~
<<dd_do>>
logit foreign gear_ratio headroom
<</dd_do>>
~~~~

When you try this yourself, you may notice that its not quite as fast as `regress`. That is because for linear regression we have a "closed form
solution" - we just do some quick math and reach an answer. However, almost every other type of regression lacks a closed form solution, so instead we
solve it iteratively - Stata guesses at the best coefficients that minimize error^[Technically that maximizes likelihood, but that distinction is not
important for understanding.], and keeps guessing (using the knowledge of the previous guesses) until it stops getting significantly different
results.

From this output, we get the "Number of obs" again. Instead of an ANOVA table with a F-statistic to test model significance, there is instead a "chi2"
(^$^\chi^2^$^, pronounced "ky-squared" as in "Kyle"). In this model, we reject the null that all coefficients are identically 0.

When we move away from linear regression, we no longer get an ^$^R^2^$^ measure. There have been various pseudo-^$^R^2^$^'s suggested, and Stata
reports one here, but be careful assigning too much meaning to it. It is not uncommon to get pseudo-^$^R^2^$^ values that are negative or
above 1. We'll discuss measuring [goodness of fit](#logistic-goodness-of-fit) below.

The coefficients table is interpreted in almost the same way as with regression. We see that `gear_ratio` has a significant positive coefficient, and
`headroom`'s coefficient is indistinguishable from 0. We *cannot* nicely interpret the coefficients. All we can say is that "As gear ratio increases,
the probability of a car being foreign increases."

To add any interpretability to these coefficients, we should instead look at the odds ratios (these coefficients are known as the log-odds). We can
obtain this with the `or` options.

~~~~
<<dd_do>>
logit, or
<</dd_do>>
~~~~

Notice that the "chi2", "PseudoR2", "z" and "P>|z|" do not change - we're fitting the same model! We're just changing how the coefficients are
represented.

Odds ratios null hypothesis is at 1, not at 0. Odds ratios are always positive. So a significant odds ratio will be away from 1, rather than away from
0 as in linear regression or the log odds. We can interpret odds ratios as percentages. The odds ratio for `gear_ratio` is
<<dd_display: %9.4f exp(_b[gear_ratio])>>, suggesting that a 1 increase in the odds ratio leads to a 30,833% increase in the odds that the car is
foreign! This is massive! But keep in mind, `gear_ratio` had a very narrow range - an increase of 1 is very large. Let's rescale gear_ratio and try again.

~~~~
<<dd_do>>
gen gear_ratio2 = gear_ratio*10
logit foreign gear_ratio2 headroom, or
<</dd_do>>
~~~~

Note once again that the model fit characteristics haven't changed; we've fit the same model, just with different units. Now the interpretation is
more reasonable, for every .1 increase in `gear_ratio` (which corresponds to a 1 increase in `gear_ratio2`), we predict an average mileage increase of
77%.

The odds ratio on headroom is not distinguishable from 1, however, if it were, the interpretation is that increasing the headroom by 1 inch is
predicted to decrease the mileage by about <<dd_display: %9.1f 100*(1-exp(_b[headroom]))>>% on average (1 - <<dd_display: %9.3f exp(_b[headroom])>>).

^#^^#^^#^ Categorical Variables and Interactions

Both [categorical variables](#including-categorical predictors) and [interactions](#interactions) can be included as they were in linear regression,
with the appropriate interpretation of coefficients/odds ratios. The `margins` command also works the same.

The `predict` command adds a new [statistic](#obtaining-predicted-values-and-residuals). `xb` now is the linear predictor, which is often not
useful. Instead, the `pr` statistic returns the estimated probability of a positive outcome.

^#^^#^^#^ Logistic regression assumptions

The assumptions for logistic regression are simpler than linear. The outcome measure must be binary with a 0 for a negative response and 1 for a
positive. (Technically they don't have to be positive/negative. We could think of predicting male/female and code male = 0 and female = 1. However,
the convention would be to consider the outcome as "The person is female" so a 1 represents a "success" and a 0 a "failure" of that statement.) The
errors in a logistic regression model are fairly contained (you can't be wrong than more than 1!) so there are no real assumptions about them. The
[independence](#independence) assumption is still here and still important, again, a mixed logistic model may be appropriate if the data is not
independent.

^#^^#^^#^ Logistic goodness of fit

As we mentioned earlier, there are various issues with the Pseudo ^$^R^2^$^ reported by `logit`, so use it carefully. There are two alternate
approaches.

The first is to look at a classification table:

~~~~
<<dd_do>>
estat classification
<</dd_do>>
~~~~

Classification is based upon the predicted probability, a predicted probability above .5 is classified as "1", below as "0". The output here is rather
long, but the general idea is to capture how well we're predicting without having too many false results. (If your outcome variable was 80% 1's and
20% 0's, if I predicted all 1's, I'd be right 80% of the time! So it's important to also see that I'm wrong in 100% of the true 0's).

- Sensitivity is how likely you are to correctly predicted a positive outcome.
- Specificity is how likely you are to correctly predicted a negative outcome.
- The positive/negative predictive values are how likely a positive/negative prediction is to be correct.

We want those results to be high.

The various false rates are for misclassification, and we want those low. In this model, we've done pretty good!

The second is a more formal test. There are two variants, a Pearson ^$^\chi^2^$^ test and the Hosmer-Lemeshow test. Both are fit with the `estat gof`
command. Both are testing the hypothesis that the observed positive responses match predicted positive responses in subgroups of the
population. Therefore we do *not* want to reject these tests, and a large p-value is desired.

~~~~
<<dd_do>>
estat gof
<</dd_do>>
~~~~

We see here a p-value of <<dd_display: %9.3f `=1 - chi2(`r(df)', `r(chi2)')'>>, failing to reject the null, so there is no evidence that the model
fits well.

There is some concern that when the "number of covariate patterns" is close to the number of observations , the Pearson test is invalid. In this data,
we see that <<dd_display: %9.0f `r(m)'>> is indeed "close" to <<dd_display: %9.0f `r(N)'>>. Instead, we can use the Hosmer-Lemeshow by passing the
`group(#)` option:

~~~~
<<dd_do>>
estat gof, group(10)
<</dd_do>>
~~~~

The p-value remains insignificant at <<dd_display: %9.3f `=1 - chi2(`r(df)', `r(chi2)')'>>, still no evidence of poor model fit.

Why did we choose 10 groups? It's just the standard. The only thing to keep in mind is that the Hosmer-Lemeshow test is only appropriate if the
number of groups is greater than the number of predictors (including the intercept). In this model, we had two predictors, so that's 3 total
(including the intercept), so 10 is OK.

There are some limitations to Hosmer-Lemeshow, and there are more modern alternatives. However, Stata has not implemented any yet that I'm aware of.

^#^^#^^#^ Separation

Since the logistic regression model is solved [iteratively](regression.html#fitting-the-logistic-model), this can fail for a number of reasons. Before
you begin interpreting the model, you'll want to glance at the iteration steps and make sure that no errors were printed. The most common failure is
due to separation.

With a binary outcome instead of a continuous outcome, it is much easier to have a predictor (or set of predictors) that perfectly predict the
outcome. Consider trying to predict gender based on height. With a smaller sample, it's not hard to imagine a scenario where every male is taller than
every female. This is called "perfect separation"; using this sample, knowing height gives perfect information about gender

"Partial separation" can also occur; this is when prediction is perfect only for one limit. Take the height scenario again; say everyone above 5'8" is
male, and there are two men but the rest women below 5'8". Here, we will always predict Male for heights above 5'8".

Separation (of either type) often produces coefficients to be extreme with large standard errors. Stata will sometimes warn about this, but not
always. If you notice these exceptional coefficients or if Stata does warn about separation, you'll need to investigate and consider excluding certain
predictors. It may seem counter-intuitive to exclude extremely highly predictive variables, but if a variable produces perfect separation, you don't
need a model to inform you of that.

You can examine separation by looking at a table. Imagine we wanted to restructure `rep78` as a binary variable, with low repairs (repair record below
3) and high repairs (repair records 3 and above).

~~~~
<<dd_do>>
gen repair_binary = rep78 >= 3
replace repair_binary = . if rep78 >= .
label define repbinlabel 0 "Low repairs" 1 "High repairs"
label value repair_binary repbinlabel
tab repair_binary
<</dd_do>>
~~~~

Let's try fitting the model:

~~~~
<<dd_do>>
logit foreign repair_binary
<</dd_do>>
~~~~

Notice the note at the top of the model, and notice that nothing is estimated for `repair_binary`. We have partial separation:

~~~~
<<dd_do>>
tab foreign repair_binary
<</dd_do>>
~~~~

Since we have a zero in one of the cells, that's partial separation. Complete separation would be zero in both off-diagonal cells.

^#^^#^^#^ `logit` Miscellaneous.

The `logit` model supports the margins command just like `regress` does. It does not support `estat vif` because variance inflation factors are not
defined for logistic models.

Collinearity, overfitting, and model selection remain concerns in the logistic model.

Robust standard errors via `vce(robust)` are supported.

^#^^#^^#^ `logit` vs `logistic`

Instead of `logit`, we could run the `logistic` command. The only difference is that `logistic` reports the odds ratio by default whereas `logit`
reports the log odds. My personal preference is `logit`, but there's no need to use one over the other.

^#^^#^ Other regression models

There are several other models which we will not cover, but function similarly to the above.

- Poisson regression is useful when you have count data; i.e. number of visitors or number of thunderstorms in a month. It can be fit with the
  `poisson` command, and results are interpreted similar to logistic regression (coefficients vs odds ratios); but instead of predicting a positive
  outcome, its predicting a larger count. If you have very large counts (such that a histogram appears to show a bell curve), linear regression can be
  used instead.
- Poisson has the strong assumption that the mean of the outcome is equal to its variance (small average counts have little noise; large average
  counts have a lot of noise). If this assumption is violated (or you want to check it), negative binomial regression also handles count data, without
  that assumptions, using `nbreg`. The output will include a test of "alpha=0", if this fails to reject, then Poisson regression is sufficient.
- There are two extensions to logistic regression, ordinal logistic and multinomial. Ordinal logistic is used when there are more than 2 outcome
  categories, and they are ordered (e.g. not sick (0), mildly sick (1), very sick (2)). Using `ologit`, Stata estimates an underlying continuous
  distribution and returns the "cut points", allowing categorization. If there are multiple groups but not ordered, e.g. race, use `mlogit` for
  multinomial logistic regression. It essentially fits a model predicting membership in each group versus all other, with some restrictions across the
  models.
