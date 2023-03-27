---
title: 'tehtuner: An R package to fit and tune models for the conditional average treatment effect'
tags:
  - R
  - causal inference
  - clinical trials
authors:
  - name: Jack M. Wolf
    orcid: 0000-0002-8919-8740
    affiliation: 1 # (Multiple affiliations must be quoted)
affiliations:
  - name: University of Minnesota Division of Biostatistics, USA
    index: 1
date: 26 March 2023
bibliography: inst/REFERENCES.bib
---

# Summary

Randomized clinical trials (RCTs) often test and describe the average treatment effect, or how the the candidate intervention is expected to increase or decrease an outcome of interest overall. However, secondary analyses may seek to identify ways in which underlying subject characteristics such as age or health status may modify the expected treatment effect, resulting in *treatment effect heterogeneity*.
This function is called the conditional average treatment effect (CATE) and is defined as
$$\tau(\boldsymbol{x})= \operatorname{E}\left(Y^1-Y^0|\boldsymbol{X}=\boldsymbol{x}\right)$$
where $\boldsymbol{x}$ is a vector of covariate measurements and $Y^1$ and $Y^0$ are the potential outcomes that would be observed under the treatment and control arms, respectively.
Information about the CATE can then be used to determine the optimal treatment on a subject-to-subject basis (personalized medicine) or identify sub-populations for whom additional interventions or support are needed.

`tehtuner` fits models to estimate the CATE using the Virtual Twins method [@foster_subgroup_2011] while controlling the method's behavior likelihood of falsely detecting treatment modifiers when all subjects would respond to treatment the same by implementing the permutation procedure proposed in @wolf_permutation_2022.
A key feature of Virtual Twins is that it estimates a simple model such as a regression tree which can be easily interpreted to understand the treatment as opposed to other popular data-adaptive methods which trade in interpretability for model flexibility. 
This is accomplished through a two-step procedure which first uses a flexible method such as random forests to estimate each subject's anticipated response under each treatment (Step 1) and then models the difference in these response estimates through a simple model such as a regression tree (Step 2).

# Statement of need

Although there are several readily available R packages that can estimate the CATE [@hill_bayesian_2011; @vieille_avirtualtwins_2018; @tibshirani_grf_2022]; there are few, if any, at the time of writing which both estimate an interpretable model *and* guarantee controlled behavior when there is no treatment effect heterogeneity. While @vieille_avirtualtwins_2018 does provide an implementation of the original Virtual Twins manuscript, it does not support the methods in Steps 1 and 2 evaluated and recommended in @deng_practical_2023 such as Super Learner [@van_der_laan_super_2007] in addition to being prone to overfitting when the CATE is constant.

At this current time, `tehtuner` supports linear models fit via the LASSO [@tibshirani_regression_1996], MARS [@friedman_multivariate_1991], random forests [@breiman_random_2001], and super learner [@van_der_laan_super_2007] in Step 1 and linear  models tuned via the LASSO [@tibshirani_regression_1996], regression and classification trees [@breiman_classification_2017], and conditional inference trees [@hothorn_unbiased_2006] in Step 2. Comparative evaluations of these methods can be found in @wolf_permutation_2022 and @deng_practical_2023.

# Example Usage

The primary function is `tunevt()`, which first fits the Step 1 model and then fits the Step 2 model with an appropriate penalty parameter to ensure that the probability of incorrectly detecting any treatment effect modifiers when there is no treatment effect heterogeneity is a user-specified value.

The following code fits a tuned Virtual Twins model using a random forest in Step 1 and a regression tree in Step 2 while setting the probability of falsely detecting treatment effect heterogeneity at 20%. 
The fitted model for the CATE can be found in `$vtmod` and used to identify, in this case, three distinct subgroups with differential treatment effects.

```r
library(tehtuner)

vt_mod <- tunevt(
  data = tehtuner_example, Y = "Y", Trt = "Trt", 
  step1 = "randomforest", step2 = "rtree", alpha0 = 0.2, 
  p_reps = 100, ntree = 50
)

vt_mod$vtmod
#> n= 1000 
#> 
#> node), split, n, deviance, yval
#>       * denotes terminal node
#> 
#> 1) root 1000 18372.4300  1.6830340  
#>   2) V1< -3.008541 511  5643.7030 -0.3942647 *
#>   3) V1>=-3.008541 489  8219.4140  3.8537890  
#>     6) V3>=0.000282894 19   448.7299 -5.3806930 *
#>     7) V3< 0.000282894 470  6084.9480  4.2270980 *
```


# Acknowledgements

The author would like to thank their advisors, Drs. David M. Vock and Joseph S. Koopmeiners, for their support developing the methods implemented in this work.
This work was by the National Institute on Drug Abuse (Award Number R01DA046320).
The content is solely the responsibility of the author and does not necessarily represent the official views of the National Institute on Drug Abuse.

# References
