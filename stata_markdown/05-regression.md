^#^ Regression

One notable exclusion from the [previous chapter](univariate-and-some-bivariate-analysis.html) was comparing the mean of a continuous variables across
three or more groups. Two-sample t-tests compare the means across two groups, and ^$^\chi^2^$^ tests can compare two categorical variables with
arbitrary number of levels, but the traditional test for comparing means across multiple groups is ANOVA (ANalysis Of VAriance). While historically
this has been a very useful procedure due to the ease with which it can be performed manually, its modern use has been supplanted by regression, which
is mathematically equivalent and easier to extend (the downside of regression is that it is more difficult to calculate, but given that we are no
longer doing statistical analyses by hand...). This relationship extends to other variations of ANOVA such as ANCOVA or MANOVA.

If you still want to fit an ANOVA, it can be done with the `oneway` command. Otherwise we turn now to regression.

Regression is a set of techniques where we attempt to fit a model to a data set estimating the relationships between a set of predictor variables
(either continuous or categorical in nature) and a response variable of interest. There are many versions of regression which are appropriate for
different types of response variables, or address different concerns when fitting the model. In this chapter and [the next](mixed-models.html), we
will discuss a few variations.

^#^^#^ Terminology

When discussing any form of regression, we think of predicting the value of one variable^[There are variations of regression with multiple outcomes,
but they are for very specialized circumstances and can generally be fit as several basic regression models instead.] based upon several other
variables.

The variable we are predicting can be called the "outcome", the "response" or the "dependent variable".

The variables upon which we are predicting can be called "predictors", "covariates", or "independent variables".

^#^^#^ Linear Regression

Linear regression (also known as Ordinary Least Squares (OLS) regression) is the most basic form of regression, where the response variable is
continuous. Technically the response variable can also be binary or categorical but there are better regression models for those types of
outcomes. Linear regression fits this model:

^$$^
  Y = \beta_0 + \beta_1X_1 + \beta_2X_2 + \cdots + \beta_pX_p + \epsilon
^$$^

- ^$^Y^$^ represents the outcome variable.
- ^$^X_1, X_2, \cdots, X_p^$^ represent the predictors, of which there are ^$^p^$^ total.
- ^$^\beta_0^$^ represents the intercept. If you have a subject for which every predictor is equal to zero, ^$^\beta_0^$^ represents their predicted
  outcome.
- The other ^$^\beta^$^'s are called the coefficients, and represent the relationship between each predictor and the response. We will cover their
  interpretation in detail later.
- ^$^\epsilon^$^ represents the error. Regression is a game of averages, but for any individual observation, the model will contain some error.

Linear regression models can be used to predict expected values on the response variable given values on the predictors, and ^$^\epsilon^$^
represents the difference between a prediction based on the model and what the actual value of the response variable is. Stata can be used to estimate
the regression coefficients in a model like the one above, and perform statistical tests of the null hypothesis that the coefficients are equal to
zero (and thus that predictor variables are not important in explaining the response). Note that the response ^$^Y^$^ is modeled as a linear
combination of the predictors and their coefficients.

Some introductory statistical classes distinguish between simple regression (with only a single predictor) and multiple regression (with more than one
predictor). While this is useful for developing the theory of regression, simple regression is not commonly used for real analysis, as it ignores one
of the main benefits of regression, controlling for other predictors (to be discussed later).

We will now fit a model, discussing assumptions afterwards, because almost all assumption checks can only occur once the model is fit!

^#^^#^^#^ Fitting the model

Stata's `regress` command fit the linear regression model. It is followed by the outcome variable followed by all predictors. For this example, let's
use the auto data and fit a relatively simple model, predicting `mpg` based on `gear_ratio` and `headroom`.

~~~~
<<dd_do>>
sysuse auto, clear
regress mpg gear_ratio headroom
<</dd_do>>
~~~~

There is a lot of important output here, so we will step through each piece.

First, the top left table is the ANOVA table. If you were to fit a regression model with a single [categorical
predictor](#including-categorical-predictors), this would be identical to running ANOVA via `oneway`. In general we don't need to interpret anything
here, as there are further measures of model fit in the regression frameworks.

Next, the top right part has a series of measures.

- Regression performs complete case analysis - any observations missing any variable involved in this model is ignored in the
  model. (See [multiple imputation](multiple-imputation.html) for details on getting around this.) Check "Number of obs" to ensure the number of
  observations is what you expect. Here, the data has 74 rows, so the regression model is using all the data (there is no missingness in `mpg`,
  `weight` or `displacement`).
- The F-test which follows ("F(2, 71)"^[The 2 and 71 are degrees of freedom. They don't typically add any interpretation.] and "Prob > F") is testing
  the null hypothesis that all coefficients are 0. In other words, if this test fails to reject, the conclusion is the model captures no
  relationships. In this case, do not continue interpreting the results; either your conclusion is that there is no relationship, or you need to
  return to the model design phase. If this test does reject, you can continue interpreting.
- The ^$^R^2^$^ ("R-squared") is a measure of model fit. It ranges from 0 to 1 and is a percentage, explaining what percent in the variation in the
  response is explained by the linear relationship with the predictors. What's considered a "large" ^$^R^2^$^ depends greatly on your field and the
  situation, in very general terms, .6 is good and above .8 is great. However, if you know that there are a lot of unmeasured variables, a much
  smaller ^$^R^2^$^ can be considered good as well.
- Mathematically, adding a new predictor to the model will increase the ^$^R^2^$^, regardless of how useless the variable is.^[The only exception is
  if the predictor being added is either constant or identical to another variable.] This makes ^$^R^2^$^ poor for model comparison, as it would
  always select the model with the most predictors. Instead, the adjusted ^$^R^2^$^ ("Adj R-Squared") accounts for this; it penalizes the ^$^R^2^$^ by
  the number of predictors in the model. Use the ^$^R^2^$^ to measure model fit, use the adjusted ^$^R^2^$^ for model comparison.
- The root mean squared error ("Root MSE", as known as RMSE) is a measure of the average difference between the observed outcome and the predicted
  outcome. It can be used as another measure of model fit, as it is on the scale of the outcome variable. So for this example, the RMSE is
  <<dd_display: %9.4f e(rmse)>> so the average error in the model is about <<dd_display: %9.1f e(rmse)>> mpg.

Finally, we get to the coefficient table. Each row represents a single predictor. The "\_cons" row is the intercept; it's Coef. of
<<dd_display: %9.4f _b[_cons]>> represents the average response *when all other predictors are 0*. This is usually not interesting; how many cars
weighing 0 lbs do you know of? So we'll ignore this and instead go over the other rows.

- "Coef.": These are the ^$^\beta^$^ from the above model. We interpret each as "For a 1 increase in the value of the covariate with all other
  predictors held constant, we would predict this change in the response, on average." For example, for every additional inch^[This is why it's
  important to familiarize yourself with the units in your data!] of headroom in a car (while its gear ratio is constant), it is predicted to have an
  average of <<dd_display: %9.4f abs(_b[headroom])>> lower mpg.
- "Std. Err.": This represents the error attached to the coefficient. This is rarely interpreted; but if it gets extremely large or extremely small
  (and the Coef. doesn't likewise go to extremes), its an indication there may be something wrong.
- "t": This is the standardized coefficient, calculated as Coef./Std. Err. We can't directly compare the Coef.'s because of the different scales, but
  we can examine the standardized coefficients to get a sense of which predictor has a larger impact. In this model, we see that the impact of weight
  is much more than the impact of displacement.
- "P>|t|": The p-value testing whether the coefficient is significantly different than 0. In this model, we see that both `gear_ratio` and `headroom`
  have significant p-values.
- "[95% Conf. interval]": A range of possible values.

Whenever we look at any model, a distinction needs to be drawn between statistical significance and practical significance. While these two
interpretations of significance often align, they are not guaranteed to. We often have statistical significance (a p-value less than .05) when there
is no practical significance (aka clinical significance, a difference that isn't scientifically interesting). This is mostly a function of sample
size; with a large sample even very small effects can have small p-values. Alternatively, a large practical significance with a low statistical
significance can occur with very noisy data or a small sample size, which might indicate further study with a larger sample is needed.

^#^^#^^#^ Including categorical predictors

Let's say we want to add `rep78`, a categorical variable with 5 levels, to the model. Naively, we simply add it:

~~~~
<<dd_do>>
regress mpg gear_ratio headroom rep78
<</dd_do>>
~~~~

We only get a single coefficient. Stata is treating `rep78` as continuous. When including a categorical predictor, Stata will create dummy variables
(variables which take on value 1 if the observation is in that category and 0 otherwise) and include all but one, which is the reference (or
baseline). Since we only see a single coefficient here, we know Stata did it incorrectly.

The issue is that Stata doesn't know we want to treat `rep78` as categorical. If we prefix the variable name with `i.`, Stata will know it is
categorical.

~~~~
<<dd_do>>
regress mpg gear_ratio headroom i.rep78
<</dd_do>>
~~~~

First, note that `headroom` no longer has a significant coefficient! This implies that `rep78` and `headroom` are correlated, and in the first model
where we did not include `rep78`, all of `rep78`'s effect was coming through `headroom`. Once we control for `rep78`, headroom is no longer
significant. We will discuss [multicollinearity later](#multicollinearity), as well as why this is
why [model selection is bad](#model-selection-is-bad).

Now we see 4 rows for `rep78`, each corresponding to a comparison between response 1 and the row. For example, the first row, 2, is saying that when
`rep78` is 2 compared to when it is 1 (with `gear_ratio` and `headroom` held at some fixed level), the average predicted response drops by
<<dd_display: %9.3f abs(_b[2.rep78])>> (though it is not statistical significant). The last row, 5, is saying that when `rep78` is 5 compare to when
it is 1 (with `gear_ratio` and `headroom` held at some fixed level, the average predicted response increases by <<dd_display: %9.3f _b[5.rep78]>>
(again, not statistically significant).

To see the other comparisons (does 2 differ from 4?), we can use the `margins` command.

~~~~
<<dd_do>>
margins rep78
margins rep78, pwcompare(pv)
<</dd_do>>
~~~~

The first `margins` call, without any options, displays the marginal means for each category - if every car had `rep78` at those levels, it's the
average predicted mileage of all cars. The t-test here is useless - it's only testing that the average mileage of the cars in each group is not 0!

The second `margins` call adds the `pwcompare(pv)` option, which performs pairwise test between each pair of `rep78` levels. This is similar to a
post-hoc test from ANOVA if you are familiar with it. The only statistical significance we find is 5 vs 3 and 5 vs 4, suggesting that 5 is dissimilar
from 3 and 4. (Confusingly, 3 and 4 are not dissimilar from 1 or 2, but 5 is similar to 1 and 2! These sort of things can happen; its best to focus
only on the comparisons that are of theoretical interest.)

By default, using `i.` makes the first level (lowest numerical value) as the reference category. You can adjust this by using `ib#.` instead, such as:

~~~~
<<dd_do>>
regress mpg headroom gear_ratio ib3.rep78
<</dd_do>>
~~~~

**This does not fit a different model.** Both models (with `i.rep78` and `ib3.rep78`) are identical, we're just seeing slight variations. If the
models do change (especially the model fit numbers in the top right), something has gone wrong.

^#^^#^^#^ Interactions

Each coefficient we've look at so far is only testing whether there is a relationship between the predictor and response when the other predictors are
held constant. What if we think the relationship changes based on the value of other predictors? For example, we might be interested in whether the
relationship between a car's headroom and its mileage depends on it's gear ratio. Perhaps we think that cars with higher gear ratio (a high gear ratio
is indicative of a sportier car) won't be as affected by headroom as a stand-in for size, because sportier cars generally are better made.

Mathematically an interaction is nothing more than a literal multiplication. For example, if our model has only two predictors,

^$$^
  Y = \beta_0 + \beta_1X_1 + \beta_2X_2 + \epsilon
^$$^

then to add an interaction between ^$^X_1^$^ and ^$^X_2^$^, we simply add a new multiplicative term.

^$$^
  Y = \beta_0 + \beta_1X_1 + \beta_2X_2 + \beta_3(X_1\times X_2) + \epsilon
^$$^

- ^$^\beta_1\^$^ represents the relationship between ^$^X_1^$^ and ^$^Y^$^ when ^$^X_2^$^ is identically equal to 0.
- ^$^\beta_2\^$^ represents the relationship between ^$^X_2^$^ and ^$^Y^$^ when ^$^X_1^$^ is identically equal to 0.
- ^$^\beta_3\^$^ represents **both**:
    - the change in the relationship between ^$^X_1^$^ and ^$^Y^$^ as ^$^X_2^$^ changes.
    - the change in the relationship between ^$^X_2^$^ and ^$^Y^$^ as ^$^X_1^$^ changes.

Adding these to the `regress` call is almost as easy. We'll use `#` or `##` instead. `#` includes only the interaction, whereas `##` includes both the
interaction and the main effects.

- `a#b`: Only the interaction
- `a##b`: Main effect for `a`, main effect for `b`, and the interaction.
- `a b a#b`: Same as `a##b`
- `a b a##b`: Same as `a##b`, except it'll be uglier because you're including main effects twice and one will be ignored.

~~~~
<<dd_do>>
regress mpg c.headroom##c.gear_ratio i.rep78
<</dd_do>>
~~~~

Note that we used `c.`, similar to `i.`. `c.` forces Stata to treat it as continuous. Stata assumes anything in an interaction is categorical, so we
need `c.` here! This can get pretty confusing, but it's never wrong to include `i.` or `c.` when specifying a regression.

Once we include an interaction, the relationship between the variables included in the interaction and the response are not constant - the
relationship depends on the value of the other interacted variables. This can be hard to visualize with the basic regression output, so we'll look at
`margins` again instead. We'll want to look at the relationship between `mpg` and `headroom` at a few different values of `gear_ratio` to get a sense
of the pattern. `gear_ratio` ranges from 2.19 to 3.89 (this can be obtained with `summarize` or `codebook`, just don't forget to [save the
results](summarizing-data.html#storing-and-restoring-estimation-commands) or re-run the `regress` command to gain access to the [postestimation
commands](summarizing-data.html#postestimation-commands) again), so let's look at the relationship at those extremes and at 3:

~~~~
<<dd_do>>
margins, dydx(headroom) at(gear_ratio = (2.19 3 3.89))
<</dd_do>>
~~~~

While none of the p-values are significant, let's pretend they were for the sake of discussion. Notice the pattern in the "dy/dx" column. With a low
gear ratio, the relationship between headroom and mpg is negative - a larger headroom car is predicted to have lower mileage. At the other end, with a
high gear ratio, the relationship is much closer to 0, and perhaps even slightly positive.

Follow this with a call to `marginsplot` for a great visualization:

~~~~
<<dd_do>>
marginsplot
<</dd_do>>
~~~~

<<dd_graph: replace>>

With low gear_ratio, there is a negative relationship between headroom and mileage - adding headroom to a low gear ratio car is predicted to decrease
mileage, on average. However, the effect decreases as gear ratio increases, and at high levels of gear ratio, there is no longer any relationship. You
can detect this by looking at the means (the points) and the confidence bands; here there is no relationship at all, but there is some suggestion that
the relationship we describe may be occurring.

Note that the choice of looking at the effect of headroom for different levels of gear ratio was arbitrary; we could have easily looked at the effect
of gear ratio for different levels of headroom (just swap what's the in the `dydx( )` and `at( )` options). The choice in a real modeling situation
should depend on which is more interesting.

^#^^#^^#^^#^ Centering

Some sources suggest centering continuous predictors before including them in an interaction. This can help slightly with interpretation (the main
effects are the relationship when the other variable involved in the interaction are at their mean, rather than at zero) but doesn't actually affect
model fit.

To center, use the following:

~~~~
<<dd_do>>
summ gear_ratio
gen gear_ratioc = gear_ratio - `r(mean)'
summ headroom
gen headroomc = headroom - `r(mean)'
summ gear_ratioc headroomc
regress mpg c.headroomc##c.gear_ratioc i.rep78
<</dd_do>>
~~~~

If you compare fit characteristics and the interaction coefficient (and other coefficients), you'll notice nothing has changed save the coefficient
for `headroomc` and `gear_ratioc`. If we were to re-run the `margins` commands from before, we'd see the same results.

^#^^#^^#^ Robust standard errors

The standard error associated with each coefficient are determined with the assumption that the model is "true" and that, were we given an infinite
sample size, the estimates ^$^\hat{\beta}^$^ would converge to the true ^$^\beta^$^. In many situations, this is clearly untrue.

If you believe this is untrue, the estimates will be unaffected, but their standard errors will be incorrect. We can adjust for this by using "robust"
standard errors, also known as Sandwich estimators or Huber-White estimators, with the `vce(robust)` option to `regress`.

~~~~
<<dd_do>>
regress mpg c.headroom##c.gear_ratio i.rep78, vce(robust)
<</dd_do>>
~~~~

Notice that compared to the [previous model](#interactions), the Coef estimates but the standard errors (and corresponding t-statistic, p-value and
confidence interval) are slightly different.

Typically, the robust standard errors should be slightly larger than the non-robust standard errors, but not always (as in this case). Generally, the
only situation where the robust standard errors will decrease is when the error variance is highest for observations near the average value of the
predictors. This does not often happen (generally the higher residuals occur in observations that could be considered outliers).

There has been some argument that robust standard errors should always be used, because if the model is correctly specified, the robust standard
errors and regular standard errors should be almost identical, so there is no harm in using them.

^#^^#^^#^ Assumptions

There are three main assumptions when running a linear regression. Some we can test, some we cannot (and need to rely on our knowledge of the data).

^#^^#^^#^^#^ Relationship is linear and additive

Recall the linear regression model:

^$$^
  Y = \beta_0 + \beta_1X_1 + \beta_2X_2 + \cdots + \beta_pX_p + \epsilon
^$$^

This very explicitly assumes that the relationship is linear (as opposed to something non-linear, such as quadratic or exponential) and additive (as
opposed to multiplicative). We can examine this assumption by looking at plots of the residuals (estimated errors):

~~~~
<<dd_do>>
rvfplot
<</dd_do>>
~~~~

<<dd_graph: replace>>

What we're seeing here is a scatterplot between the fitted values (the predicted values for each individual) and their errors (the difference between
the predicted values and observed values). If you can see a pattern in the scatterplot, that is evidence that this assumption is
violated. **Importantly**, not seeing any pattern is **not** evidence that the assumption is valid! You'll still need to cover this assumption with
theory and knowledge of the data.

This image, from Julian Faraway's [Linear Models with R](http://www.maths.bath.ac.uk/~jjf23/LMR/) book, demonstrates a lack of pattern (the first) and
a pattern (the third). (We will discuss the second plot [below](#errors-are-homogeneous)).

![](https://i.stack.imgur.com/rtn7e.png)

If this assumption is violated, you will need to reconsider the structure in your model, perhaps by adding a squared term (e.g. `reg y c.x c.x#c.x`).

^#^^#^^#^^#^^#^ Obtaining predicted values and residuals

In the [`rvfplot`](#relationship-is-linear-and-additive), we plotted residuals versus predicted values - neither of which we have in the data. If
there is some analysis beyond what `rvfplot` produces that you're interested in, the `predict` command can obtain these. The general syntax for
`predict` is:

```
predict <new var name>, <statistic>
```

There are quite a few options for the "statistic", but the two most commonly used ones are:

- `xb`: The linear prediction (also the default). This is the predicted value for each individual based on the model.
- `residuals`: The residuals. The difference between the predicted value and observed value.

In other words, we can replicate the above `rvfplot` via:

~~~~
<<dd_do>>
predict linearpredictor, xb
predict resids, residuals
twoway scatter resids linearpredictor
<</dd_do>>
~~~~

<<dd_graph: replace>>

(The two warnings about missing values are due to 4 cars not having a value of `rep78`. See [multiple imputation](multiple-imputation.html) for a
strategy for dealing with missing data.)

^#^^#^^#^^#^ Errors are homogeneous

"Homogeneity" is a fancy term for "uniform in distribution", whereas "heterogeneity" represents "not uniform in distribution". If we were to take a
truly random sample of all individuals in Michigan, the distribution of their heights would be homogeneous - it is reasonable to assume there is only
a single distribution at work there. If on the other hand, we took a random sample of basketball players and school children, this would definitely be
heterogeneous. The basketball players have a markedly difference distribution of heights that school children!

In linear regression, the homogeneity assumption is that the distribution of the errors is uniform. Violations would include errors changing as the
predictor increased, or several groups having very different noise in their measurements.

This is an assumption we can examine, again with the residuals vs fitted plot. We're looking for either a blatant deviation from a mean of 0, or an
increasing/decreasing variability on the y-axis over time. Refer back to the [image above](#relationship-is-linear-and-additive), looking at the
middle plot. As the fitted values increase, the error spreads out.

If this assumption is violated, you may consider restructuring your model as above, or transforming either your response or predictors using log
transforms.

^#^^#^^#^^#^ Independence

This last assumption is that each row of your data is independent. If you have repeated measures, this is violated. If you have subjects drawn from
groups (i.e. students in classrooms), this is violated. There is no way to test for this, it requires knowing the data set.

If this assumption is violated, consider fitting a [mixed model](mixed-models.html) instead.

^#^^#^^#^ Miscellaneous concerns

^#^^#^^#^^#^ Multicollinearity

Multicollinearity is an issue when 2 or more predictors are correlated. If only two are correlated, looking at their correlation (with `pwcorr` or
`correlate`) may provide some indication, but you can have many-way multicollinearity where each pairwise correlation is low. You can use the variance
inflation factor to try and identify if this is an issue.

~~~~
<<dd_do>>
estat vif
<</dd_do>>
~~~~

The rule of thumb is VIF > 10 or 1/VIF (called the tolerance) < .1 suggests that the variable is involved in multicollinearity and more exploration
may be needed. We've got a ton of multicollinearity here, so we'd need to explore more and perhaps exclude one of them.

Multicollinearity can be an issue because the more correlated predictors are, the more likely that their combined effect will be inappropriately
spread among them. For a very simple example, imagine that we have the model

^$$^
  Y = \beta_0 + \beta_1X_1 + \beta_2X_2 + \epsilon
^$$^

If ^$^X_1^$^ and ^$^X_2^$^ are uncorrelated, then we can estimate ^$^\beta_1^$^ and ^$^\beta_2^$^ without a problem. Consider the extreme situations
where ^$^X_1^$^ and ^$^X_2^$^ are perfectly correlated.^[Note that if you provide data with perfect correlation, Stata will drop one of them for
you. This in only a thought exercise. If it helps, imagine their correlation is 99% instead of perfect, and add "almost" as a qualifier to most
claims.] We can therefore rewrite the equation as

^$$^
  Y = \beta_0 + (\beta_1 + \beta_2)X_1 + \epsilon
^$$^

since with perfect correlation, ^$^X_1^$^ and ^$^X_2^$^ are identical.^[Technically there could be a scaling factors such that ^$^X_1 = aX_2 + b^$^,
but let's assume without loss of generality that ^$^a=1^$^ and ^$^b=0^$^.] Now, when we fit the model, we would have estimates of ^$^\beta_1^$^ and
^$^\beta_2^$^ which sum to the "truth", but the individual level of each of ^$^\beta_1^$^ and ^$^\beta_2^$^ could be anything. For example, if the
"true" ^$^\beta_1^$^ and ^$^\beta_2^$^ are 1 and 3, they sum to 4. We could get estimated coefficients of 1 and 3, or 3 and 1, or -20 and 24!

This is an extreme example, but in practice we can be close to this situation.

^#^^#^^#^^#^ Overfitting

Overfitting occurs when a model includes so many predictors that you can no longer generalize to the population. The rule of thumb is that you should
have no more than one predictor for every 10-20 observations. The smaller your sample size, the more conservative you should be. For example, a sample
size of 100 should use no more than 10-20 predictors. Recall that a categorical predictor with ^$^k^$^ different levels adds ^$^k-1^$^ predictors!

^#^^#^^#^^#^ Model Selection is bad

There is a literature on the idea of model selection, that is, an automated (or sometimes manual) way of testing many versions of a model with a
different subset of the predictors in an attempt to find the model that fits best. These are sometimes called "stepwise" procedures.

This method has a number of flaws, including

- Doing this is basically "p-value mining", that is, running a lot of tests till you find a p-value you like.
- Your likelihood of making a false positive is very high.
- As we [saw earlier](#including-categorical-predictors), adding a new variable can have an effect on existing predictors.

Instead of doing model selection, you should use your knowledge of the data to select a subset of the variables which are either a) of importance to
you, b) theoretically influential on the outcome (e.g. demographic variables) or c) what others (reviewers) would think are influential on the
outcome. Then you can fit a single model including all of this. The "subset" can be all predictors if the [sample size](#overfitting) is sufficient.

Note that adjustments to fix assumptions (e.g. transformations) or multicollinearity would not fall into the category of model selection and are fine
to use.

^#^^#^ Exercise 4

Reload the NHANES data.

```
webuse nhanes2, clear
```

[Fit a linear regression model](#fitting-the-model) predicting `lead` based upon `sex`, `race`, `age`, `weight`, `height`, and `region`. Make sure to
handle [categorical variables](#including-categorical-predictors) appropriately! Answer the following questions which may or may not require running
additional commands.

1. How well does the model fit?
2. Does one gender tend to have higher levels of lead?
3. Is the coefficient on age statistically significant? Do you think it is clinically interesting?
4. Looking at all the differences between regions, what conclusion can you draw?
5. Add an [interaction](#interactions) between gender and age. What is the interpretation here?
6. Do any [assumptions](#assumptions) appear violated?
7. Does [multicollinearity](#multicollinearity) appear to be a concern?

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

^#^^#^ Exercise 5

Reload the NHANES data.

```
webuse nhanes2, clear
```

This time we'll fit a [logistic regression model](#logistic-regression), prediciting diabetes status on `sex`, `race`, `age`, `weight`, `height`, and
`region`. As before, be sure to handle [categorical variables](#categorical-variables-and-interactions) appropriately.

1. Does the model [fit well](#logistic-goodness-of-fit)? Does the model classify well?
2. Ignoring any issues with model fit, [what predicts](#fitting-the-logistic-model) higher odds of having diabetes?
