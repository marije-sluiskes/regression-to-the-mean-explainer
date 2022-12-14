Marije Sluiskes

# Regression to the sample’s mean age in biological age prediction

## What is regression to the mean?

Regression to the mean is a statistical phenomenon that occurs whenever
two variables of interest are imperfectly correlated. Let’s call these
variables
![X](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X "X")
and
![Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y "Y").
On average, when a measurement of
![X](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X "X")
deviates from the mean of this variable, the corresponding measurement
of
![Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y "Y")
will be closer to its mean. This occurs whenever the correlation
coefficient between
![X](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X "X")
and
![Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y "Y")
is smaller than one.

It is easiest to understand this phenomenon by considering two extreme
cases. Assume that
![X](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X "X")
and
![Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y "Y")
are normally distributed with mean
![\\mu](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu "\mu")
and standard deviation
![\\sigma](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Csigma "\sigma").
(This is just for simplicity of the subsequent formulas, but if not met,
they can be adjusted without loss of generality.) Denote the correlation
coefficient between
![X](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X "X")
and
![Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y "Y")
by
![r](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;r "r").

Now, for a given subject
![i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;i "i"),
draw measurement
![x\_i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x_i "x_i")
from
![X](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X "X").
The expected value for measurement
![y\_i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y_i "y_i")
is then
![\\mu + r(x\_i - \\mu)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu%20%2B%20r%28x_i%20-%20%5Cmu%29 "\mu + r(x_i - \mu)").
If there is a perfect correlation between
![X](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X "X")
and
![Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y "Y")
(i.e. ![r = 1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;r%20%3D%201 "r = 1")),
![E\[y\_i\] = x\_i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%5By_i%5D%20%3D%20x_i "E[y_i] = x_i").
If they are completely independent,
![E\[y\_i\] = \\mu](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%5By_i%5D%20%3D%20%5Cmu "E[y_i] = \mu").

In reality, for any
![X](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X "X")
and
![Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y "Y"),
![r](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;r "r")
will often be somewhere in between these two extreme cases. Hence, if
you select subjects with extreme measurements of
![X](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X "X"),
the average of the measurements for
![Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y "Y")
for these subjects will be closer to the mean.

## Why is it relevant in the context of biological age prediction?

A commonly used approach to predict biological age is to perform
multiple linear regression on cross-sectional data: chronological age
![C](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;C "C")
is taken as the outcome variable and regressed on a set of markers
![X](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X "X")
that were measured at the same point in time as chronological age (i.e.,
a cross-sectional set-up). Then the model’s predicted chronological age
is considered to be informative of one’s biological aging
![B](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;B "B"):
![\\hat{B} = \\hat{C} = \\beta\_0 + \\sum\_{i = 1}^{m}\\beta\_ix\_i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Chat%7BB%7D%20%3D%20%5Chat%7BC%7D%20%3D%20%5Cbeta_0%20%2B%20%5Csum_%7Bi%20%3D%201%7D%5E%7Bm%7D%5Cbeta_ix_i "\hat{B} = \hat{C} = \beta_0 + \sum_{i = 1}^{m}\beta_ix_i"),
where
![m](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;m "m")
denotes the number of included markers. In other words, the residuals
(the differences between
![C](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;C "C")
and
![\\hat{C}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Chat%7BC%7D "\hat{C}"))
are interpreted as meaningful quantities in their own right.

The correlation between chronological age and the markers will not be
perfect. There is a (significant) source of random error. Hence, there
will be regression to the mean: predicted values for
![C](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;C "C")
will tend to regress toward the sample’s mean chronological age.
*Individuals younger than this sample mean age will obtain predicted
ages that are on average too high, and individuals older than this mean
age will receive predicted ages that are too low.* This phenomenon has
been recognized to be an issue in the context of cross-sectional
biological age prediction for decades: see e.g. [Dubina et
al. (1984)](https://www.sciencedirect.com/science/article/pii/0531556584900160)
or [Hochschild
(1989)](https://www.sciencedirect.com/science/article/pii/0531556589900028).

Though this phenomenon has primarily been discussed in the context of
(multiple) linear regression, it also holds for other types of
regression models using cross-sectional data to predict
chronological/biological age. In the example below, both a linear
regression model and a boosted regression model are used to illustrate
this.

## Illustration with simulated data

``` r
library(gbm)
library(ggplot2)
```

Consider a training data set of size
![n = 10,000](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;n%20%3D%2010%2C000 "n = 10,000").
Assume the model
![Y = \\beta X + \\epsilon](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y%20%3D%20%5Cbeta%20X%20%2B%20%5Cepsilon "Y = \beta X + \epsilon").
In the context of cross-sectional biological age prediction,
![X](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X "X")
can be considered some marker of chronological age and
![Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y "Y")
(some scaled version of) chronological age.

Fit two models on the training data: a simple linear regression model
(LR) and a generalized boosted regression model (GBM).

``` r
set.seed(18)

n = 10000
X <- rnorm(n,0,6)
eps <- rnorm(n,0,6)
beta = 2

Y <- beta*X + eps

# fit models
lr_fit <- lm(Y ~ X)
gbm_fit = gbm(Y ~ X, distribution = "gaussian", n.trees = 100)

summary(lr_fit)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ X)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -23.1606  -4.0610   0.0163   4.0387  21.0570 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.04766    0.06006  -0.793    0.428    
    ## X            1.97351    0.01006 196.219   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.006 on 9998 degrees of freedom
    ## Multiple R-squared:  0.7939, Adjusted R-squared:  0.7938 
    ## F-statistic: 3.85e+04 on 1 and 9998 DF,  p-value: < 2.2e-16

``` r
# get fitted values
lr_fitted <- fitted(lr_fit)
gbm_fitted<-predict.gbm(gbm_fit,as.data.frame(X))
```

Now plot the residuals
(![\\hat{Y} - Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Chat%7BY%7D%20-%20Y "\hat{Y} - Y"))
against
![Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y "Y").
For both models there is a downward sloping pattern visible: people with
higher chronological ages (higher
![Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y "Y"))
on average have lower predicted chronological ages (lower
![\\hat{Y}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Chat%7BY%7D "\hat{Y}")).
The lower the correlation between
![X](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X "X")
and
![Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y "Y"),
the stronger this effect.

``` r
df_fitted <- data.frame(Y, lr_fitted, gbm_fitted)

ggplot(aes(x = Y, y = lr_fitted - Y), data = df_fitted) +
  geom_abline(intercept = 0, slope = 0, col = "blue") + 
  geom_point() + 
  labs(title = "Linear regression model", y = "Predicted Y - Y") +
  theme_bw()
```

![](Explainer_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggplot(aes(x = Y, y = gbm_fitted - Y), data = df_fitted) +
  geom_abline(intercept = 0, slope = 0, col = "blue") + 
  geom_point() + 
  labs(title = "Generalized boosting regression model", y = "Predicted Y - Y") +
  theme_bw()
```

![](Explainer_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

Consider now a test set, sampled from the same population. Split the
test set in two groups, based on their value for
![Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y "Y").
This would be similar to comparing groups from the general population
defined by their chronological age (e.g., old / long-lived individuals
with young individuals from the same population).

Use the models fitted on the training data to obtain predictions for
![Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y "Y")
for the two groups. Now compare the values of the residuals between
these two groups.

``` r
set.seed(22)

nval=10000
x.val<-rnorm(nval,0,6)
y.val<-sapply(1:nval,function(i)beta*x.val[i]+rnorm(1,0,6))

#Long-lived group
y.val1<-y.val[y.val>0]
x.val1<-x.val[y.val>0]

#General population group
y.val0<-y.val[y.val<0]
x.val0<-x.val[y.val<0]

# MLR 
y.pred.val1<-sapply(1:length(x.val1),function(i)coef(lr_fit)[1]+coef(lr_fit)[2]*x.val1[i])
y.pred.val0<-sapply(1:length(x.val0),function(i)coef(lr_fit)[1]+coef(lr_fit)[2]*x.val0[i])

mlr.res1<-y.pred.val1-y.val1
mlr.res0<-y.pred.val0-y.val0

# GBM
y.predmatrix.val1 <-predict.gbm(gbm_fit, data.frame(X = x.val1))
y.predmatrix.val0 <-predict.gbm(gbm_fit, data.frame(X = x.val0))

gbm.res1<-y.predmatrix.val1-y.val1
gbm.res0<-y.predmatrix.val0-y.val0
```

Now compare the values of the residuals between these two groups.
Ideally, they should be similarly distributed and centered around zero:
after all, the random noise distribution is the same for the two groups,
namely a normal distribution with mean 0. However, due to the regression
to the mean phenomenon, those with a value for
![Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y "Y")
that is higher than the sample mean on average receive a value for
![\\hat{Y}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Chat%7BY%7D "\hat{Y}")
that is lower than
![Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y "Y"),
and those with a value for
![Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y "Y")
that is lower than the sample mean on average receive a value for
![\\hat{Y}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Chat%7BY%7D "\hat{Y}")
that is higher than
![Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y "Y").

When using a t-test to compare the difference in the mean of the
residuals between the two groups, a significant difference is detected.
This holds for both models.

``` r
dftest_res0 <- data.frame(mlr.res0, gbm.res0)
dftest_res1 <- data.frame(mlr.res1, gbm.res1)
```

``` r
t.test(mlr.res1,mlr.res0)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  mlr.res1 and mlr.res0
    ## t = -40.654, df = 9953.6, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -4.746503 -4.309839
    ## sample estimates:
    ## mean of x mean of y 
    ## -2.400986  2.127185

``` r
ggplot(aes(x = mlr.res0), data = dftest_res0) +
  geom_density(aes(col = "darkgreen")) +
  geom_density(aes(x = mlr.res1, col = "purple"), data = dftest_res1) + 
  labs(title = "Linear regression model", x = "residual") +
  theme_bw() +
  scale_colour_identity(guide = "legend", 
                        name = "legend",
                        labels = c("young group", "old group"))
```

![](Explainer_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
t.test(gbm.res1,gbm.res0)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  gbm.res1 and gbm.res0
    ## t = -42.36, df = 9952.4, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -4.924866 -4.489235
    ## sample estimates:
    ## mean of x mean of y 
    ## -2.525503  2.181548

``` r
ggplot(aes(x = gbm.res0), data = dftest_res0) +
  geom_density(aes(col = "darkgreen")) +
  geom_density(aes(x = mlr.res1, col = "purple"), data = dftest_res1) + 
  labs(title = "Generalized boosting regression model", x = "residual") +
  theme_bw() +
  scale_colour_identity(guide = "legend", 
                        name = "legend",
                        labels = c("young group", "old group"))
```

![](Explainer_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

This simple simulated scenario illustrates that it is not valid to
interpret differences between true and predicted chronological age as an
indication of biological aging when comparing groups defined by their
chronological age. All cross-sectional age clocks will have a tendency
to overestimate the age of younger individuals and underestimate the age
of older individuals. This cannot be interpreted as a sign of
accelerated or decelerated biological aging.
