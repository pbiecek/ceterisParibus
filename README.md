# What-If Plots

[![Build Status](https://api.travis-ci.org/pbiecek/WhatIfPlots.png)](https://travis-ci.org/pbiecek/WhatIfPlots)

What-If Plots (*Ceteris Paribus Plots*) are designed to present model responses around a single point in the feature space. For example around a single prediction for an interesting observation. Plots are designed to work in a model-agnostic fashion, they are working for any Machine Learning model and allow for model comparisons.

The What-If Plots supplement the [breakDown Plots]( https://github.com/pbiecek/breakDown) that are designed to attribute features into parts of a single prediction.

Imagine a scenario in which a client gets low credit score and is wondering why the score is so low. *What can I do to get a higher score?*

What-If Plots show possible scenarios for model predictions allowing for changes in a single dimension (the *ceteris paribus* principle).

## Installation

To get started, install the latest version of **WhatIfPlots** from GitHub:

```
devtools::install_github("pbiecek/WhatIfPlots")
```

# Simple What-If Plot

The plot below shows What-If plots for an apartment data for a large apartment (130 m2) on 3-rd floor. On can read what would be the model prediction for smaller apartment (largest change) or one that is located higher.

<center><img width="600" src="misc/use_case_1.png"></center>

# Uniform What-If Plot

Since different variables may have different scales, we recommend to transform variables into quantiles. Then it is much easier to see how a particular variable looks in relation to others.

<center><img width="600" src="misc/use_case_2.png"></center>

Having all variables in a single quantile scale we may plot them together in a single plot.

<center><img width="600" src="misc/use_case_3.png"></center>

# Multimodel What-If Plot

We may compare responses for few models in a single plot.

<center><img width="600" src="misc/use_case_4.png"></center>

# Some equations

More formally, a What If plot for variable V and observations O is a set of points (X, Y) where

$$Y(X) := f(O_1, ..., O_{V-1}, O_{V} + X, O_{V+1}, ..., O_{p})$$


# More examples

https://pbiecek.github.io/WhatIfPlots/articles/whatIf1d.html

