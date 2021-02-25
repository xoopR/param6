param6
================

## Why param6?

Multiple bottlenecks in `distr6` all led to the `ParameterSet` object,
which has considerable problems. `param6` deals with this by making use
of:

-   `list` instead of `data.table`
-   `set6` and `Rcpp` for speed of containedness checks
-   Symbolic representation for increasing speed of support checks
-   S3 for `prm` parameter objects
-   Removing `ParameterSetCollections` in favour of prefixes and
    extraction

## paradox vs. distr6 vs. param6

Below are some benchmarks comparing key features of param6, distr6, and
paradox.

### Construction times

param6 and paradox include separate parameter objects whereas distr6
does not, therefore these are separately compared and then together.

#### param6::prm vs paradox::ParamX

``` r
microbenchmark(
  paradox = paradox::ParamDbl$new("a"),
  param6 = param6::prm("a", "reals")
)
```

    ## Unit: microseconds
    ##     expr     min      lq      mean   median       uq       max neval cld
    ##  paradox 530.340 581.893  2856.005 685.5035 956.8250  188503.6   100   a
    ##   param6 106.951 120.406 17325.349 146.1760 183.9735 1647753.6   100   a

#### param6::ParameterSet vs paradox::ParamSet vs distr6::ParameterSet

First without values (this isn’t possible in distr6):

``` r
microbenchmark(
  paradox = {
    ParamSet$new(list(
      ParamDbl$new("a")
    ))
  }, 
  distr6 = {
    distr6::ParameterSet$new(
      id = "a",
      support = set6::Reals$new(),
      value = 1
    )
  },
  param6 = {
    param6::ParameterSet$new(list(
      prm("a", "reals")
    )) 
  }
)
```

    ## Unit: microseconds
    ##     expr      min        lq      mean    median        uq       max neval cld
    ##  paradox 1609.038 2174.3760 3513.4166 2781.0720 3747.9320 15659.522   100  b 
    ##   distr6 2472.554 3672.8845 6497.1024 5364.0035 7760.7680 49922.386   100   c
    ##   param6  363.630  522.1065  909.8822  605.4135  836.1085  6982.207   100 a

With values:

``` r
microbenchmark(
  paradox = {
    ps <- ParamSet$new(list(
      ParamDbl$new("a")
    ))
    ps$values$a <- 1
  }, 
  distr6 = {
    distr6::ParameterSet$new(
      id = "a",
      support = set6::Reals$new(),
      value = 1
    )
  },
  param6 = {
    param6::ParameterSet$new(list(
      prm("a", "reals", 1)
    )) 
  }
)
```

    ## Unit: microseconds
    ##     expr      min       lq      mean   median        uq       max neval cld
    ##  paradox 6105.521 7105.520 9939.8702 8267.198 10744.329 38730.537   100   c
    ##   distr6 2248.061 2618.910 4086.1559 2935.913  3888.729 21699.133   100  b 
    ##   param6  454.811  544.949  888.8888  633.952   863.704  5595.308   100 a

### Getting and setting values

``` r
paradox <- ParamSet$new(list(
  ParamDbl$new("a")
))
paradox$values$a <- 1

param6 <- param6::ParameterSet$new(list(
  prm("a", "reals", 1)
)) 

distr6 <- distr6::ParameterSet$new(
  id = "a",
  support = set6::Reals$new(),
  value = 1
)
```

Getting values:

``` r
microbenchmark(
  paradox = paradox$values$a,
  distr6 = distr6$getParameterValue("a"),
  param6 = param6$values$a
)
```

    ## Unit: microseconds
    ##     expr     min       lq      mean   median      uq      max neval cld
    ##  paradox   2.259   2.7495   5.81384   4.9830   7.493   22.859   100  a 
    ##   distr6 224.898 228.6225 313.32112 255.9815 300.272 1346.424   100   b
    ##   param6   2.167   2.5280   4.80121   3.5695   5.660   25.868   100  a

And setting:

``` r
vals = list(a = 2)
microbenchmark(
  paradox = {paradox$values = vals},
  distr6 = distr6$setParameterValue(lst = vals),
  param6 = {param6$values = vals}
)
```

    ## Unit: microseconds
    ##     expr      min        lq      mean   median        uq       max neval cld
    ##  paradox 4443.723 6170.4430 8538.9734 7403.095 9851.6190 24445.143   100   c
    ##   distr6 1748.316 2178.0500 4642.6509 2783.577 3716.7730 89812.954   100  b 
    ##   param6  219.380  316.5885  628.3729  385.912  544.5435  6749.543   100 a

### Parameter set collections

param6 removes the idea of a collection by instead embracing prefixes
and extraction methods.

``` r
paradox1 <- ParamSet$new(list(
  ParamDbl$new("a")
))
paradox1$values$a <- 1
paradox2 <- ParamSet$new(list(
  ParamDbl$new("b")
))
paradox2$values$b <- 2

param61 <- param6::ParameterSet$new(list(
  prm("a", "reals", 1)
)) 
param62 <- param6::ParameterSet$new(list(
  prm("b", "reals", 2)
)) 

distr61 <- distr6::ParameterSet$new(
  id = "a",
  support = set6::Reals$new(),
  value = 1
)
distr62 <- distr6::ParameterSet$new(
  id = "a",
  support = set6::Reals$new(),
  value = 2
)
```

``` r
microbenchmark(
  paradox = ParamSetCollection$new(list(paradox1, paradox2)),
  distr6 = ParameterSetCollection$new(D1 = distr61, D2 = distr62),
  param6 = param6:::c.ParameterSet(param61, param62)
)
```

    ## Unit: microseconds
    ##     expr      min        lq      mean   median        uq        max neval cld
    ##  paradox 3434.875 4233.1565 7824.6972 5396.075 7550.0195 156833.891   100   b
    ##   distr6  327.489  417.6195  651.8163  462.862  584.7045   4962.294   100  a 
    ##   param6  775.039  987.4275 1458.6529 1246.534 1584.7415   6533.939   100  a

Rep is a nice extra feature for replicating a parameter set with
identical properties. This isn’t currently possible in paradox or
distr6, benchmark below compare excluding full process and including.

``` r
paradox <- lapply(lapply(1:100, function(i) ParamDbl$new(paste0("Pre", i, "__a"))), function(.x) ParamSet$new(list(.x)))

distr6 <- rep(list(distr6::ParameterSet$new(
  id = "a",
  support = set6::Reals$new(),
  value = 1
)), 100)
names(distr6) <- paste0("Pre__", 1:100)

microbenchmark(
  paradox = ParamSetCollection$new(paradox),
  distr6 = ParameterSetCollection$new(lst = distr6),
  param6 = param6:::rep.ParameterSet(param6, 100, "Pre")
)
```

    ## Unit: microseconds
    ##     expr        min          lq        mean      median         uq         max
    ##  paradox 158895.858 200484.9905 300432.9309 234279.4435 328672.810 1695932.106
    ##   distr6    449.279    548.6530    859.9568    661.9135    943.319    3711.874
    ##   param6    684.358    807.7755   1415.5362    904.0350   1108.981   10954.299
    ##  neval cld
    ##    100   b
    ##    100  a 
    ##    100  a

``` r
microbenchmark(
  paradox = {
    paradox <- lapply(lapply(1:100, function(i) 
      ParamDbl$new(paste0("Pre", i, "__a"))), function(.x) ParamSet$new(list(.x)))
    ParamSetCollection$new(paradox)
  },
  distr6 = {
    distr6 <- rep(list(distr6::ParameterSet$new(
      id = "a",
      support = set6::Reals$new(),
      value = 1
    )), 100)
    names(distr6) <- paste0("Pre__", 1:100)
    ParameterSetCollection$new(lst = distr6)
  },
  param6 = {
    param6 <- param6::ParameterSet$new(list(
      prm("a", "reals", 1)))
    param6:::rep.ParameterSet(param6, 100, "Pre")
  }
)
```

    ## Unit: milliseconds
    ##     expr        min         lq       mean     median         uq        max
    ##  paradox 377.244803 522.665487 667.640479 586.522011 652.450550 2337.03302
    ##   distr6   2.922534   3.710481   5.965636   4.785811   6.843199   22.44207
    ##   param6   1.308180   1.528151   2.682907   1.974937   2.846436   11.05868
    ##  neval cld
    ##    100   b
    ##    100  a 
    ##    100  a

## Going Forward

param6 provides a promising alternative to other parameter interfaces by
embracing symbolic representation and C++. The examples above do not
show the full range of features including transformations, dependencies,
custom checks, and tag properties. The examples are also on minimal
parameters, I have seen over 100x improvement on distr6, and this is
only the beginning.

Next steps will include incorporating Rcpp fully in set6 for further
speed improvements and then with param6.
