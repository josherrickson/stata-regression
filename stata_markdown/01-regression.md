^#^ Ordinary Least Squares

Regression is a term for a wide range of very common statistical modeling designed to estimate the relationship between a set of variables. The nature
of the variables and the hypothesized relationship between the variables affect which choice of regression is to be used.

In this set of notes we will talk about generalized linear models, one of the two most commonly used forms of regression (the other being mixed
effects regression, which is the subject of [a follow-up set of notes]() FIX ME).

We will start with the most basic form of regression, linear regression, and build up from there.

^#^^#^ Terminology

When discussing any form of regression, we think of predicting the value of one variable^[There are variations of regression with multiple outcomes,
but they are for very specialized circumstances and can generally be fit as several basic regression models instead.] based upon several other
variables.

The variable we are predicting can be called the "outcome", the "response" or the "dependent variable".

The variables upon which we are predicting can be called "predictors", "covariates", or "independent variables".

The model we're going to start discussing is called "linear regression". You may also have this called "least squares regression" or "ordinary least
squares (OLS)". A lot of the time, if you see a reference to "regression" without specifying the type, they are referring to linear regression.

^#^^#^ Theory

Ordinay Least Squares regression is the most basic form of regression. This is suitable for situations where you have some number of predictor
variables and the goal is to establish a linear equation which predicts a continuous outcome. Technically the outcome need not be continuous, but
there are often better forms of regression to use for non-continuous outcomes. The term "linear equation" refers back to high school geometry and the
equation for a line,

^$$^
    y = mx + b
^$$^

In that framing, the value of y can be obtained for a given value of x, based upon the slope (m) and intercept (b). You can easily extend this to higher dimensions,

^$$^
    y = mx + nz + b
^$$^

Now the value of y also depends on z and it's slope (n).

Linear regression fits a model based on this equation of a line:

^$$^
  Y = \beta_0 + \beta_1X_1 + \beta_2X_2 + \cdots + \beta_pX_p + \epsilon
^$$^

- ^$^Y^$^ represents the outcome variable.
- ^$^X_1, X_2, \cdots, X_p^$^ represent the predictors, of which there are ^$^p^$^ total.
- ^$^\beta_0^$^ represents the intercept. ^$^\beta_0^$^ represents the predicted average outcome when all ^$^X_i^$^ are 0. Often this will be
  nonsensical and ignored (e.g., if ^$^X_1^$^ is age, it usually makes no sense to estimate the outcome when age is 0).
- The other ^$^\beta^$^'s are called the coefficients, and represent the relationship (slope) between each predictor and the response. We will cover
  their interpretation in detail later.
- ^$^\epsilon^$^ represents the error. Regression is a game of averages, but for any individual observation, the model will contain some error.

For example, the relationship between the weight of a car (in lbs) and the length of a car (in inches) is approximately^[This is based a database of
cars from 1978, and with some pretty harsh rounding of the coefficients, for demonstration purposes.]:

^$$^
    \textrm{weight} = -3000 + 33*\textrm{length}
^$$^

The intercept is meaningless - how many 0 length cars do you know? If we plug in a reasonable value for length, say 200 inches, we can solve for
weight:

^$$^
    \textrm{weight} = -3000 + 33*200 = 3600
^$$^

This is the predicted weight; for a given car of length 200, it won't be exactly 3600 lbs, but that difference is error (^$^\epsilon^$^).

Stata can be used to estimate the regression coefficients in a model like the one above, and perform statistical tests of the null hypothesis that the
coefficients are equal to zero (and thus that predictor variables are not important in explaining the response). Note that the response ^$^Y^$^ is
modeled as a linear combination of the predictors and their coefficients.

Some introductory statistical classes distinguish between simple regression (with only a single predictor) and multiple regression (with more than one
predictor). While this is useful for developing the theory of regression, simple regression is not commonly used for real analysis, as it ignores one
of the main benefits of regression, controlling for other predictors (to be discussed later).

We will now fit a model, discussing assumptions afterwards, because almost all assumption checks can only occur once the model is fit!

^#^^#^ Fitting the model

For demonstration purposes, we'll use the [2015 Residentical Energy Consumption Survey
(RECS)](https://www.eia.gov/consumption/residential/data/2015/index.php?view=microdata). This is a public data-set made available by the governmental
Energy Information Administation containing household level information about "energy characteristics on the housing unit, usage patterns, and
household demographics"^[https://www.eia.gov/consumption/residential/about.php]

~~~~
<<dd_do>>
import delimited https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv
<</dd_do>>
~~~~

Stata's `regress` command fit the linear regression model. The general syntax is

```
regress <outcome> <predictors>
```

Let's fit a model predicting dollar expenditure on electricity (`dollarel`) based upon the square footage of the house (`totsqft_en`) and the gender
of the respondent (`hhsex`). Before we fit the model, we want to briefly explore each variable involved.

~~~~
<<dd_do>>
summarize dollarel
histogram dollarel
<</dd_do>>
~~~~

<<dd_graph: replace>>

We see here that expenditure on electricty ranges from under $ 20 to over $ 8000, though the vast majority seem to be between a few hundred dollars
and $ 2000-3000. We will discuss a bit [later]() FIX ME how to handle right-skewed variables.

~~~~
<<dd_do>>
summarize totsqft_en
histogram totsqft_en
<</dd_do>>
~~~~

<<dd_graph: replace>>

Looks very similar to `dollarel`.

~~~~
<<dd_do>>
tab hhsex
<</dd_do>>
~~~~

For looking at the codebook online (Excel file [here](https://www.eia.gov/consumption/residential/data/2015/xls/codebook2015_public_v3.xlsx)), we see
that 1 is female and 2 is male. Let's generate a new variable to make this distinction more clear

~~~~
<<dd_do>>
generate female = hhsex == 1
tab female
<</dd_do>>
~~~~

Now `female` will be a 1 if the respondent is female and 0 otherwise.

~~~~
<<dd_do>>
regress dollarel totsqft_en female
<</dd_do>>
~~~~

There is a lot of important output here, so we will step through each piece.

First, the top left table is the ANOVA table. If you were to fit a regression model with a single [categorical
predictor](#including-categorical-predictors), this would be identical to running ANOVA via `oneway`. In general we don't need to interpret anything
here, as there are further measures of model fit in the regression frameworks.

Next, the top right part has a series of measures.

- Regression performs complete case analysis - any observations missing any variable involved in this model is ignored in the model. (See [multiple
  imputation](multiple-imputation.html) for details on getting around this.) Check "Number of obs" to ensure the number of observations is what you
  expect. Here, the data has 5,686 rows, so the regression model is using all the data (there is no missingness in the variables involved in the
  model^[There likely was, but the RECS does data imputation for you.].
- The F-test which follows ("F(<<dd_display: %9.0g e(df_m)>>, <<dd_display: %9.0g e(df_r)>>)"^[The <<dd_display: %9.0g e(df_m)>> and
  <<dd_display: %9.0g e(df_r)>> are degrees of freedom. They don't typically add any interpretation.] and "Prob > F") is testing the null hypothesis
  that all coefficients are 0. In other words, if this test fails to reject, the conclusion is the model captures no relationships. In this case, do
  not continue interpreting the results; your conclusion is that there is no linear relationship. If this test does reject, you can continue
  interpreting.
- The ^$^R^2^$^ ("R-squared") is a measure of model fit. It ranges from 0 to 1 and is a percentage, explaining what percent in the variation in the
  response is explained by the linear relationship with the predictors. What's considered a "large" ^$^R^2^$^ depends greatly on your field and the
  situation, in very general terms, .6 is good and above .8 is great. However, if you know that there are a lot of unmeasured variables, a much
  smaller ^$^R^2^$^ can be considered good as well. The ^$^R^2^$^ of <<dd_display: %9.2f e(r2)>> is low, though it's not so close to 0 as to be
  meaningless.
- Mathematically, adding a new predictor to the model will increase the ^$^R^2^$^, regardless of how useless the variable is.^[The only exception is
  if the predictor being added is either constant or identical to another variable, in which case the regression model won't estimate a coefficient
  for it anyways.] This makes ^$^R^2^$^ poor for model comparison, as it would always select the model with the most predictors. Instead, the adjusted
  ^$^R^2^$^ ("Adj R-Squared") accounts for this; it penalizes the ^$^R^2^$^ by the number of predictors in the model. Use the ^$^R^2^$^ to measure
  model fit, use the adjusted ^$^R^2^$^ for model comparison.
- The root mean squared error ("Root MSE", as known as RMSE) is a measure of the average difference between the observed outcome and the predicted
  outcome. It can be used as another measure of model fit, as it is on the scale of the outcome variable. So for this example, the RMSE is
  <<dd_display: %9.2f e(rmse)>> so the average error is about $<<dd_display: %9.2f e(rmse)>>. Recall that we saw before that expenditure ranged to
  $ 8000, so an error of $<<dd_display: %9.0f e(rmse)>> is low, but not insignificant.

Finally, we get to the coefficient table. Each row represents a single predictor. The "\_cons" row is the intercept; it's Coef. of
<<dd_display: %9.2f _b[_cons]>> represents the average response *when all other predictors are 0*. Given that square-footage cannot be 0, this is
meaningless and can be ignored. (Note that we cannot exclude the constant, we are simply ignoring it.)

- "Coef.": These are the ^$^\beta^$^ from the above model. We interpret each as "For a 1 increase in the value of the covariate with all other
  predictors held constant, we would predict this change in the response, on average." For example, for every additional square foot in a house (while
  the gender of the respondent is constant), the respondent is predicted to have an average of $<<dd_display:%9.2f abs(_b[totsqft_en])>> lower
  expenditure. It may make more sense to discuss this in terms of whole dollar changes; for every additional four square feet, we'd expect a $ 1
  increase in expenditure.
- "Std. Err.": This represents the error attached to the coefficient. This is rarely interpreted; but if it gets extremely large or extremely small
  (and the Coef. doesn't likewise go to extremes), its an indication there may be something wrong.
- "t": This is the standardized coefficient, calculated as Coef./Std. Err. We can't directly compare the Coef.'s because of the different scales, but
  we can examine the standardized coefficients to get a sense of which predictor has a larger impact. In this model, we see that the impact of square
  footage is much more than the impact of gender.
- "P>|t|": The p-value testing whether the coefficient is significantly different than 0. In this model, we see that square footage is significant,
  while there appears to be no gender difference.
- "[95% Conf. interval]": A range of possible values.

Whenever we look at any model, a distinction needs to be drawn between statistical significance and practical significance. While these two
interpretations of significance often align, they are not guaranteed to. We often have statistical significance (a p-value less than .05) when there
is no practical significance (aka clinical significance, a difference that isn't scientifically interesting). This is mostly a function of sample
size; with a large sample even very small effects can have small p-values. Alternatively, a large practical significance with a low statistical
significance can occur with very noisy data or a small sample size, which might indicate further study with a larger sample is needed.

In this example, the gender difference may qualify - the model is estimating that women respondents pay almost $ 10 more than men on average, however,
this coefficient is not statistically distinguishable from 0. The confidence interval, ranging from -30 to 50, it extremely wide. It's possible that
there is a different between the genders of the respondent, but the estimate is small and noisy - and our sample size is not sufficient. All we can
conclusively say is that we do not have enough evidence to claim there is a difference in gender.

^#^^#^ Including categorical predictors

Let's say we want to add `regionc` to the model. This is a variable that identifies which region (Northeast, Midwest, South, West) the respondent
lives in. It's reasonable to test whether the energy expenditures differs by region, regardless of the size of the home. Let's naively add it to the
model.

~~~~
<<dd_do>>
regress dollarel totsqft_en female regionc
<</dd_do>>
~~~~

We only get a single coefficient. What is the interpretation of it? There is none. Stata is treating region as continuous. Regression models cannot
use categorical predictors. Instead, the regression model requires a series a dummy variables (e.g. `northwest` - Is this respondent in the Northwest?
`south` - Is this respondent in the South?) for which each respondent has a single positive (1) response and the remainder negative (0) responses.

While we could create this ourselves^[You could use `tab regionc, generate(region)` to create it.], something we'd likely have to do in a software
like SPSS, Stata (and most modern software) can handle this automatically. The issue is that Stata doesn't know we want to treat `regionc` as
categorical. If we prefix the variable name with `i.`, Stata will know it is categorical.

First, let's look at the regions and add appropriate lables from the codebook.

~~~~
<<dd_do>>
tab regionc
label define region 1 "Northeast" 2 "Midwest" 3 "South" 4 "West"
label values regionc region
<</dd_do>>
~~~~

~~~~
<<dd_do>>
regress dollarel totsqft_en female i.regionc
<</dd_do>>
~~~~

This model is improved, considering the Adjusted ^$^R^2^$^ values. The coefficient (and significance) on square footage is not really changed, but
notice the magnitude of the coefficient on `female` has changed drastically. This implies that gender and region were correlated, and when we did not
control for region, gender was including it's effect. We will discuss [multicollinearity later](#multicollinearity), as well as why this is why [model
selection is bad](#model-selection-is-bad).

Now we see 3 rows for `regionc`, each corresponding to a comparison between region "Northeast" and the given row. When we include a categorical
variable, one group is excluded as the baseline. By default, Stata removes the first group (here 1, Northeast). So we can see that the Midwest
compared to the northeast has statistically significantly lower expenditure, with an average reduction of
$<<dd_display: %9.2f abs(_b[2.regionc])>>. Those in the South have higher averae expenditure compared to Northeast, and those in the West have lower
average expenditure.

We do not see a comparison of, for example, South versus West. To see the other comparisons we can use the `margins` command.

~~~~
<<dd_do>>
margins regionc
margins regionc, pwcompare(pv)
<</dd_do>>
~~~~

The first `margins` call, without any options, displays the marginal means for each category - if every respondent was from the given region, what is
the average predicted expenditure. The t-test here is useless - it's only testing that the average expenditure is non-zero. We see that the lowest average expenditure is in the Midest, the highest in the South.

The second `margins` call adds the `pwcompare(pv)` option, which performs pairwise test between each pair of regions. This is similar to a post-hoc
test from ANOVA if you are familiar with it. All regions are statistically significant from each other.

By default, using `i.` makes the first level (lowest numerical value) as the reference category. You can adjust this by using `ib#.` instead, such as:

~~~~
<<dd_do>>
regress dollarel totsqft_en female ib2.regionc
<</dd_do>>
~~~~

**This does not fit a different model.** Both models (with `i.regionc` and `ib2.regionc`) are identical, we're just seeing slight variations. If the
models do change (especially the model fit numbers in the top right), something has gone wrong. So what's the point of this - well sometimes you only
care about comparisons to a single baseline group. In that case, if you make that group the proper reference category, you don't need to use
`margins`.

^#^^#^ Interactions

Each coefficient we've look at so far is only testing whether there is a relationship between the predictor and response when the other predictors are
held constant. What if we think the relationship changes based on the value of other predictors? For example, we might be interested in whether the
relationship between square footage and expenditure differs by region.

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
regress dollarel c.totsqft_en##i.regionc female
<</dd_do>>
~~~~

Note that we used `c.`, similar to `i.`. `c.` forces Stata to treat it as continuous. Stata assumes anything in an interaction is categorical, so we
need `c.` here! This can get pretty confusing, but it's never wrong to include `i.` or `c.` when specifying a regression.

Once we include an interaction, the relationship between the variables included in the interaction and the response are not constant - the
relationship depends on the value of the other interacted variables. This can be hard to visualize with the basic regression output, so we'll look at
`margins` again instead. We'll want to look at the relationship between square footage and expenditure in each region.

~~~~
<<dd_do>>
margins region, dydx(totsqft_en)
<</dd_do>>
~~~~

The `dydx()` option specifies that instead of marginal means (as we had above), we want to look at marginal slopes - that is, the slope between square
footage and expenditure in each region. Recall that without the interaction the coefficient associated with square footage was approximately .25. Here
we see that in the South and West that relationship is actually steeper, while in the Northeast and Midwest it's shallower. Here the t-tests are
more interesting, testing whether the slop in each region is significantly different than zero.

Note that the slope in Northeast in the margins call is identical to the main effect of square footage. The `margins` command is not telling us
anything we could not have obtained or calculated from the regression output - it's just doing so with minimal effort and maximal clarity.

Let's test whether the slopes differ between regions.

~~~~
<<dd_do>>
margins region, dydx(totsqft_en) pwcompare(pv)
<</dd_do>>
~~~~

It can sometimes be tricky to look at these tests and determine what it is telling us, but what this is basically saying is that Northeast and Midwest
have the same slope, and South and West have the same slope. Then Northeast/Midwest is different than South/West. There's a bit of confusion because
West and Northeast are not statistically distinguishable (although .6 is extremely close to significance). This sort of thing happens often with
multiple pairwise comparison, it's best to try and focus on the overarching result instead of getting bogged down in details.

We can call `margins` with slightly different options to be able to produce an interaction plot. Rather than using the `dydx` option, we'll use the
`at` option to estimate marginal means at specific values of square footage.

~~~~
<<dd_do>>
margins region, at(totsqft_en = (1000 8000))
<</dd_do>>
~~~~

Follow this with a call to `marginsplot`:

~~~~
<<dd_do>>
marginsplot
<</dd_do>>
~~~~

<<dd_graph: replace>>

There isn't too much interesting here that we haven't identified before. Often there is, and this plot will be useful. You can use the `pwcompare(pv)`
option alongside the `at()` option to test for differences in region at specific values of square footage.

^#^^#^^#^ Centering

Some sources suggest centering continuous predictors before including them in an interaction. This will change the coefficients in the regression
output, but will not fit a different model. What may be usefl is the main effects of terms involved in the interaction are now when the other variable
is at it's mean, rather than at 0. For example, in the model above with the interaction of square footage and region, the coefficients on region are
the differences in region when square footage is 0 - not interesting. If we centered square footage, then the coefficients on region would be testing
for differences when square footage is at it's mean.

However, once again, **this is not fitting a different model**. The results will be identical. I'd always recommend looking at the margins and
interaction plot, even if you do center.

^#^^#^ Robust standard errors

The standard error associated with each coefficient are determined with the assumption that the model is "true" and that, were we given an infinite
sample size, the estimates ^$^\hat{\beta}^$^ would converge to the true ^$^\beta^$^. In many situations, this is clearly untrue.

If you believe this is untrue, the estimates will be unaffected, but their standard errors will be incorrect. We can adjust for this by using "robust"
standard errors, also known as Sandwich estimators or Huber-White estimators, with the `vce(robust)` option to `regress`.

~~~~
<<dd_do>>
regress dollarel c.totsqft_en##i.regionc female, vce(robust)
<</dd_do>>
~~~~

Notice that compared to the [previous model](#interactions), the Coef estimates are identical but the standard errors (and corresponding t-statistic,
p-value and confidence interval) are slightly different.

Typically, the robust standard errors will be larger than the non-robust standard errors, but not always. Generally, the only situation where the
robust standard errors will decrease is when the error variance is highest for observations near the average value of the predictors. This does not
often happen (generally the higher residuals occur in observations that could be considered outliers).

There has been some argument that robust standard errors should always be used, because if the model is correctly specified, the robust standard
errors and regular standard errors should be almost identical, so there is no harm in using them.

^#^^#^ Assumptions

There are three main assumptions when running a linear regression. Some we can test, some we cannot (and need to rely on our knowledge of the data).

^#^^#^^#^ Relationship is linear and additive

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

^#^^#^^#^^#^ Obtaining predicted values and residuals

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

^#^^#^^#^ Errors are homogeneous

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

^#^^#^^#^ Independence

This last assumption is that each row of your data is independent. If you have repeated measures, this is violated. If you have subjects drawn from
groups (i.e. students in classrooms), this is violated. There is no way to test for this, it requires knowing the data set.

If this assumption is violated, consider fitting a [mixed model](mixed-models.html) instead.

^#^^#^ Variable Transformations

Sometimes if one of the above assumptions is violated, it can be addressed by a simple variable transformation. The most common is taking the log of a
right-skewed variable.

~~~~
<<dd_do>>
generate lgdollarel = log(dollarel)
histogram lgdollarel
<</dd_do>>
~~~~

<<dd_graph: replace>>

~~~~
<<dd_do>>
regress lgdollarel c.totsqft_en##i.regionc female, vce(robust)
<</dd_do>>
~~~~

We see a slight improvement to the model, though expenditure did not start out too skewed.

Once we make this transformation, the intepretation of the coefficients change. By exponentiating the coefficients, we can determine the new
intepretation. (Calling `regress` or any estimation command after running one replays the results without having to re-calculate them.)

~~~~
<<dd_do>>
regress, eform("Exp(Coef.)")
<</dd_do>>
~~~~

The string in quotes ("Exp(Coef.)") is irrelevant and only for display purposes, `eform` is the important option. Now you can interpret each
coefficient as the percent change in the outcome for a 1-unit increase in the predictor. For example, a coefficient of 1.15 would indicate that for a
1-unit increase in x, you predict an average increaes of 15% in y. A coefficient of .89 would predict an average decrease of 11% in y.

^#^^#^ Miscellaneous concerns

^#^^#^^#^ Multicollinearity

Multicollinearity is an issue when 2 or more predictors are correlated. If only two are correlated, looking at their correlation (with `pwcorr` or
`correlate`) may provide some indication, but you can have many-way multicollinearity where each pairwise correlation is low. You can use the variance
inflation factor to try and identify if this is an issue.

~~~~
<<dd_do>>
estat vif
<</dd_do>>
~~~~

The rule of thumb is VIF > 10 or 1/VIF (called the tolerance) < .1 suggests that the variable is involved in multicollinearity and more exploration
may be needed.

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

^#^^#^^#^ Overfitting

Overfitting occurs when a model includes so many predictors that you can no longer generalize to the population. The rule of thumb is that you should
have no more than one predictor for every 10-20 observations. The smaller your sample size, the more conservative you should be. For example, a sample
size of 100 should use no more than 10-20 predictors. Recall that a categorical predictor with ^$^k^$^ different levels adds ^$^k-1^$^ predictors!

^#^^#^^#^ Model Selection is bad

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
