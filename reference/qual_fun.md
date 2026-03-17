# calculate model performance metrics

function that calculates different estimations for model accuracy,
namely: root mean squared error (rmse), (Nash-Sutcliff) model efficiency
(nse), Pearson corelation coefficient (r), relative error (re), mean
absolute error (mae), and normalized mean absolute error (nmae). returns
a data.frame containing the six quality estimates

## Usage

``` r
qual_fun(O, P)
```

## Arguments

- O:

  data.frame containing observed values, first row is datetime

- P::

  data.frame containing predicted values, first row is datetime
