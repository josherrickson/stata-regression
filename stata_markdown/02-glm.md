^#^ Generalized Linear Models

If the outcome variable is not continuous, while OLS will usually be able to be fit, the results may be unexpected or undesired. For example, if the
response is a binary indicator, an OLS model fit may predict an individual has a negative response.

We can generalize the [model](ordinary-least-squares.html#theory) from ordinary least squares to allow a non-linear relationship between the
predictors and the outcome, which may fit different outcomes better.

Recall that the equation for OLS is

^$$^
  Y = \beta_0 + \beta_1X_1 + \beta_2X_2 + \cdots + \beta_pX_p + \epsilon
^$$^

We can modify this by allowing the left hand side to be a function of ^$^Y^$^,

^$$^
  f(Y) = \beta_0 + \beta_1X_1 + \beta_2X_2 + \cdots + \beta_pX_p + \epsilon
^$$^

Note that this is still linear in ^$^X^$^ (the right-hand side). Non-linear regession refers to something such as

^$$^
  Y = \beta_1X_1^{\beta_2X_2} + \epsilon
^$$^

Therefore, even though the function ^$^f()^$^ may not be linear, the model is still linear - hence "generalized linear model".

The function, ^$^f()^$^, is called the "link" function. If the link function is the identify function, ^$^f(x) = x^$^, the GLM simplifies to ordinary
least squares.

We'll talk about a few link functions and the regression models they define.

^#^^#^ Logistic Regression

Logistic regression is used when the outcome is dichotomous - either a positive outcome (1) or a negative outcome (0). For example, presence or
absence of some disease. The link function for logistic regression is logit,

^$$^
  \textrm{logit}(x) = \textrm{log}\Big(\frac{x}{1-x}\Big)
^$$^

^$$^
    \textrm{logit}\left(P(Y = 1 | X)\right) = \beta_0 + \beta_1X_1 + \cdots + \beta_pX_p + \epsilon.
^$$^

Note also that unlike in OLS, the left-hand side is not the observed outcome, but rather the probability of a positive outcome. So the goal of a
logistic model is not to predict whether an individual will have a positive outcome, but rather to predict their probability of a positive
outcome. This is a subtle difference, but worth pointing out since predicted values will be probabilities, not a binary response.

^#^^#^^#^ Fitting the logistic model

We can fit a logistic regression using the `logit` command in State. It works very similarly to `regress`. Let's run a model predicting the presence
of a cellar based on square footage, region and electricity expenditure.

~~~~
<<dd_do: quietly>>
import delimited https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv
generate female = hhsex == 1
label define region 1 "Northeast" 2 "Midwest" 3 "South" 4 "West"
label values regionc region
<</dd_do>>
~~~~

~~~~
<<dd_do>>
tab cellar
replace cellar = . if cellar == -2
logit cellar dollarel totsqft_en i.regionc female
<</dd_do>>
~~~~

When you try this yourself, you may notice that its not quite as fast as `regress`. That is because for OLS we have a "closed form solution" - we just
do some quick math and reach an answer. However, almost every other type of regression lacks a closed form solution, so instead we solve it
iteratively - Stata guesses at the best coefficients that minimize error^[Technically that maximizes likelihood, but that distinction is not important
for understanding.], and uses an algorithm to repeatedly improve those coefficients until the reduction in error is below some threshold.

From this output, we get the "Number of obs" again. Instead of an ANOVA table with a F-statistic to test model significance, there is instead a "chi2"
(^$^\chi^2^$^, pronounced "ky-squared" as in "Kyle"). In this model, we reject the null that all coefficients are identically 0.

When we move away from linear regression, we no longer get an ^$^R^2^$^ measure. There have been various pseudo-^$^R^2^$^'s suggested, and Stata
reports one here, but be careful assigning too much meaning to it. It is not uncommon to get pseudo-^$^R^2^$^ values that are negative or
above 1. We'll discuss measuring [goodness of fit](#logistic-goodness-of-fit) below.

The coefficients table is interpreted in almost the same way as with regression. We see that square footage and energy expenditure have significant
coefficient (positive and negative respectively), and there appears to be no gender effect. There are differences between regions.

However, we *cannot* nicely interpret these coefficients, which are known as the "log odds". All we can say is that "As square footage increases, the
probability of a house having a cellar increases."

To add any interpretability to these coefficients, we should instead look at the odds ratios. These are the exponentiated log odds. We can ask Stata
to produce these with the `or` option.

~~~~
<<dd_do>>
logit, or
<</dd_do>>
~~~~

Notice that the "chi2", "PseudoR2", "z" and "P>|z|" do not change - we're fitting the same model! We're just changing how the coefficients are
represented.

Odds ratios null hypothesis is at 1, not at 0. A value of 1 represents equal odds (or no change in odds). Odds ratios are always positive. So a
significant odds ratio will be away from 1, rather than away from 0 as in linear regression or the log odds. The interpretation of odds ratios can be
tricky, so let's be precise here.

For categorical predictors, the interpretation is fairly straightforward. The coefficient on females is <<dd_display: %9.4f exp(_b[female])>>. This
means that for every 1 female respondent who has a basement in their house, you would expect <<dd_display: %9.4f exp(_b[female])>> male respondents to
have a basement.

For continuous predictors, its the odds as the value of the predictor changes. Consider the coefficient on energy expenditure,
<<dd_display: %9.6f exp(_b[dollarel])>>. For every 1 house of expenditure ^$^e^$^ which has a cellar, you'd expect
<<dd_display: %9.6f exp(_b[dollarel])>> houses at expenditure ^$^e+1^$^ to have a cellar.

Those coefficients are really close to 1 due to scaling: a $1 increase or 1-sqft increase is irrelevant. Due to the non-linear relationship between
the predictors and the outcome, we cannot simply multiply the odds ratios. Instead, let's scale the variables and re-fit the model.

~~~~
<<dd_do>>
generate dollarel1000 = dollarel/1000
generate totsqft1000 = totsqft_en/1000
logit cellar dollarel1000 totsqft1000 i.regionc female, or
<</dd_do>>
~~~~

Note once again that the model fit characteristics haven't changed; we've fit the same model, just with different units. Now the interpretation are
more reasonable. As the expenditure increases by $1000, the odds of having a cellar decrease by <<dd_display: %9.0f 100*(1 - exp(_b[dollarel1000]))>>%.

For every additional 1000-square feet, the odds of having a cellar increases by <<dd_display: %9.0f 100*(exp(_b[totsqft1000])-1)>>%. In other words,
for every two 2000-square foot house that have a cellar, you'd expect five 3000-square foot houses to have cellars.

^#^^#^^#^ Categorical Variables and Interactions

Both [categorical variables](#including-categorical predictors) and [interactions](#interactions) can be included as they were in linear regression,
with the appropriate interpretation of coefficients/odds ratios.

^#^^#^^#^ `margins` and `predict`

The `margins` command works mostly the same, though it produces results on the probability scale, not the odds scale.

~~~~
<<dd_do>>
margins regionc
<</dd_do>>
~~~~

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

As we mentioned earlier, there are various issues with the Pseudo ^$^R^2^$^ reported by `logit`, so use it carefully. In fact, all measures of
goodness of fit in non-OLS models are problematic. However, here are two approaches commonly used in logistic regression.

The first is to look at a classification results: If we were to choose a threshold (say .2) and classify positive/negative outcomes against it (If a
predicted probability is below .2, classify as 0. If a predicted probability is above .2, classify as 1.). We don't know what the "correct" threshold
is (if you even believe there could be one), but we can test over a range of thresholds and see how well we classify.

~~~~
<<dd_do>>
lroc
<</dd_do>>
~~~~

<<dd_graph: replace>>

This is called a ROC curve (Receiver Operating Characteristic). Starting with a threshold of 1 (so no houses are predicted to have a cellar) at (0,0)
and continuing to a threshold of 0 (all houses are predicted to have a cellar), each point is plotted as sensitivity (percent of correctly predicted
positive responses) versus specificity (incorrecly predicted negative responses). With a threshold of 1, every house with a cellar is predicted to not
a cellar (so 0% correct) and every house without a cellar is predicted to have a cellar (0% correct). As the threshold increases, these values are
computed and plotted. The diagonal ^$^y=x^$^ line represents a completely uninformative model, and the ROC curve cannot pass below it. The closer it
is to the top left corner, the better predictive the model is. The AUC (area under curve) is another measure of model-fit: .5 would indicate no
information (ROC on the diagonal), 1 would indicate perfect fit (ROC to the top left corner). The AUC here is <<dd_display: %9.2f r(area)>> indicating
some predictive power.

The second is a more formal test. There are two variants, a Pearson ^$^\chi^2^$^ test and the Hosmer-Lemeshow test. Both are fit with the `estat gof`
command. Both are testing the hypothesis that the observed positive responses match predicted positive responses in subgroups of the
population. Therefore we do *not* want to reject these tests, and a large p-value is desired.

~~~~
<<dd_do>>
estat gof
<</dd_do>>
~~~~

We see here a p-value of <<dd_display: %9.3f `=1 - chi2(`r(df)', `r(chi2)')'>>, failing to reject the null, so there is no evidence that the model
fits poorly.

There is some concern that when the "number of covariate patterns" is close to the number of observations , the Pearson test is invalid. In this data,
we see that <<dd_display: %9.0f `r(m)'>> is indeed equal to <<dd_display: %9.0f `r(N)'>>. Instead, we can use the Hosmer-Lemeshow by passing the
`group(#)` option:

~~~~
<<dd_do>>
estat gof, group(10)
<</dd_do>>
~~~~

The p-value gets very significant.

Why did we choose 10 groups? It's just the standard. The only thing to keep in mind is that the Hosmer-Lemeshow test is only appropriate if the number
of groups is greater than the number of predictors (including the intercept). In this model, we had two predictors, so that's 3 total (including the
intercept), so 10 is OK. There is some discussion that this choice can bias results - there are examples out there where passing 9 to the option
rejects the test, whereas passing 11 passes.

Overall, as mentioned, take these goodness-of-fit measures with a grain of salt. Focus on interpreting the coefficients.

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

^#^^#^^#^ `logit` Miscellaneous.

The `logit` model supports the margins command just like `regress` does. It does not support `estat vif` because variance inflation factors are not
defined for logistic models.

Collinearity, overfitting, and model selection remain concerns in the logistic model.

Robust standard errors via `vce(robust)` are supported.

One other common cause of failed convergence is scaling - try scaling all your variables and see if that improves convergence.

^#^^#^^#^ `logit` vs `logistic`

Instead of `logit`, we could run the `logistic` command. The only difference is that `logistic` reports the odds ratio by default whereas `logit`
reports the log odds. My personal preference is `logit`, but there's no need to use one over the other.

^#^^#^^#^ Sample size concerns

When the percent of positive outcomes is close to 50%, the rules we [discussed for OLS](ordinary-least-squares.html#overfitting) hold, 10-20
observations per predictor. As the percent of positive outcomes deviates from 50%, you may need a much larger sample size - mostly to ensure a
reasonable number of both positive and negative outcomes. For example, if you expect 5% of individuals to have a positive outcome, and have a sample
size of 40, that's only 2 individuals with a positive outcome! Instead you should strive to have at least 10 or ideally over 100 individuals per
outcome.

^#^^#^^#^ Rare outcomes

If the percent of positive outcomes is extreme (e.g. 99% or 1%), logistic regression may fail to converge. Sometimes [Poisson
regression](#poisson-regresion) which we'll talk about next can be used in this situation. The estimated coefficients will be slightly biased, but
convergence may easier to achieve.

^#^^#^ Poisson regression

When the response variable is a count (number of occurences), then using a log link function produces what's known as Poisson regression.

^#^^#^^#^ Fitting the model

As with logistic regression, the Poisson regression command is simple and similar to `regress`. Let's predict the number of rooms in a house based
upon the variables we've been dealing with so far.

~~~~
<<dd_do>>
histogram totrooms
<</dd_do>>
~~~~

<<dd_graph: replace>>

~~~~
<<dd_do>>
poisson totrooms dollarel1000 totsqft1000 i.cellar i.regionc female
<</dd_do>>
~~~~

We see the same sort of header information, including a Chi2 test for model significance and another pseudo ^$^R^2^$^.

The coefficients are again not interpretable other than sign and magnitude, but we can report incidence-rate ratios (IRR) instead. The `irr` option
produces these, which are just the exponetiated coefficients.

~~~~
<<dd_do>>
poisson, irr
<</dd_do>>
~~~~

IRR's are slightly easier to interpret than OR's. Each represents a average percent change in the count of the outcome predicted when there is a 1
increase in the predictor variable.

For example, the IRR for square footage is <<dd_display: %9.3f exp(_b[totsqft1000])>> which translates to a <<dd_display: %9.1f 100*(exp(_b[totsqft1000])-1)>>%
predicted average increase in the number of rooms in a house which increases in sample size by 1,000 square feet.

^#^^#^^#^ Interactions, categorical variables, `margins`, `predict`

Interactions and categorical variables work the same. `margins` estimates the marginal means, which are the expected number of counts. `predict` by
default predicts the number of events.

^#^^#^^#^ Assumptions

Poisson regression has the same independence assumption. It also has a very strong assumption which is specific to the Poisson distribution - namely
that the mean and variance of a Poisson random variable are equal. In other words, as the mean of a Poisson variable increases, it becomes more spread
out in a linear fashion.

We can examine whether this may be true for a given variable.

~~~~
<<dd_do>>
summarize totrooms
<</dd_do>>
~~~~

Here the mean, <<dd_display: %9.2f r(mean)>> is very close to the variance, <<dd_display: %9.2f r(sd)>>^$^^2^$^ = <<dd_display: %9.2f r(Var)>>. This
is not always the case for count data. If this is not true, a Negative Binomial model may be more appropriate. It's extremely similar to Poisson,
except it allows the mean and variance to be decoupled by means of ^$^\alpha^$^, called the overdispersion factor.

We can fit it with the `nbreg` command.

~~~~
<<dd_do>>
nbreg totrooms dollarel1000 totsqft1000 i.cellar i.regionc female
<</dd_do>>
~~~~

The row for `alpha` is the estimate of the overdispersion factor. If this value is close to 0, the Poisson model is appropriate. That's what is
occcurring here - in fact it's so close to 0 that Stata refuses to even compute a standard error for it. The likelihood ratio test below it is
formally testing whether the Poisson model is appropriate; here the p-value of 1.0 let's us stick with Poisson. If this rejected, the negative
binomial model is more appropriate. The negative binomial model is interpreted in the same fashion as Poisson.

^#^^#^^#^ Exposure

Sometimes the count is limited by the exposure - for example, if you are counting the number of students in a class who fail an exam, this number will
likely be higher for classes with more students. We can adjust for this in the Poisson model by specifying the `exposure(___)` option.

In the energy data, let's say instead of the total number of rooms, we want to predict the number of bedrooms. Obviously the total number of rooms in
the house will greatly affect the number of bedrooms.

~~~~
<<dd_do>>
histogram bedrooms
<</dd_do>>
~~~~

<<dd_graph: replace>>

~~~~
<<dd_do>>
poisson bedrooms dollarel1000 totsqft1000 i.cellar i.regionc female, exposure(totrooms) irr
<</dd_do>>
~~~~

We interpret this exactly as before. Negative binomial models can likewise have `exposure(___)` set.

^#^^#^ Other regression models

There are two extensions to logistic regression, ordinal logistic and multinomial logistic.

Ordinal logistic is used when there are more than 2 outcome categories, and they are ordered (e.g. not sick (0), mildly sick (1), very sick
(2)). Using `ologit`, Stata estimates an underlying continuous distribution and returns the "cut points", allowing categorization.

If there are multiple groups but not ordered, e.g. race, use `mlogit` for multinomial logistic regression. It essentially fits a model predicting
membership in each group versus all other, with some restrictions across the models.
