# 4A03 Project

Excerpt from textbook (# 1.2 A Model-Building Strategy):

> ...There are three main steps in the process, each of which may be used several times:

> 1. model specification (or identification)
> 2. model fitting, and
> 3. model diagnostics

> In model specification, we look at the time plot of the series, compute many different statistics from the data, and also apply any knowledge of the subject matter in which the data arise. It should be emphasized that the model chosen at this point is tentative and subject to revision later on in the analysis. In choosing a model, __we shall attempt to adhere to the principle of parsimony; that is, the model used should require the smallest number of parameters that will adequately represent the time series. "__

### Model Proposed
* Once differencing
* Power transformed with Lambda = 1.55

### Requirement
"...The report should be from 5 to no more than 10 double-spaced single-column pages using a font size 10-12, not including images and graphs."

".... In an individual contribution form, which is to be completed at the end of the project, you will be asked to summarize the contribution of each of your group members and evaluate the strengths and weaknesses of the team. While the project grade is primarily a group grade, individual grades will be adjusted based on your own contribution and your peer assessments."

"... Time management is going to play an important role in the project. Create and follow a schedule that enables your group to successfully complete all written deliverables."

### March 6: 

W/o transforming the series, it already looks like it has a constant variance. Transformations I tried made the AIC worse in the model selection. So I went ahead and found my best model to be ARIMA(1,0,2)(1,1,1)12

ARIMA(1,0,2)(2,1,1)12 performs similarly too

-- Traky


### March 21st

Compared two transformation method:
                A. Differencing
                B. Log Trasformation
Both methods result a similar time series plot with flat tendency and fairly stable dispersion.
From the qq plot, it shows that method A have a right-skewed trend and method B have a left-skewed trend.

-- Line
