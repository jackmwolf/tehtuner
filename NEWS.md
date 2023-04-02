# tehtuner

# tehtuner 0.3.0

Adds support for classification trees in Step 2 by setting 
`step2 = 'classtree'` with a given threshold of `threshold`.

Adds the `print.tunevt` method.

# tehtuner 0.2.1

Fixes a bug where `zbar` was calculated using the mean difference in the first
column of the data instead of using the location of the variable Y.

# tehtuner 0.2.0

Adds the `parallel` option to `tunevt` to support parallel backends.

# tehtuner 0.1.1

This patch reconciles an invalid URI in the `tunevt` documentation's references.

# tehtuner 0.1.0

This is a new package that implements the Virtual Twins algorithm for subgroup
identification (Foster et al., 2011) while controlling the probability of falsely detecting
differential treatment effects when the conditional treatment effect is constant
across the population of interest. These methods were originally presented in 
Wolf et al. (2022).

## References

- Foster, J. C., Taylor, J. M., & Ruberg, S. J. (2011). 
  Subgroup identification from randomized clinical trial data. 
  _Statistics in Medicine, 30_(24), 2867–2880. 
  [https://doi.org/10.1002/sim.4322](https://doi.org/10.1002/sim.4322)

- Wolf, J. M., Koopmeiners, J. S., & Vock, D. M. (2022). A permutation procedure
  to detect heterogeneous treatment effects in randomized clinical trials while
  controlling the type-I error rate. *Clinical Trials*.
  [https://doi.org/10.1177/17407745221095855](https://doi.org/10.1177/17407745221095855)

## Key function

-   `tunevt()` fits a Virtual Twins model using user-specified Step 1 and Step 2
    models with parameter selection to control the probability of a false
    discovery.
