param6
================

## Why param6?

Multiple bottlenecks in `distr6` all led to the `ParameterSet` object.
The original designs had been planned so that a quick sets and parameter
sets interface could be designed and then later abstracted. The former
now exists in `set6` but the latter is still causing considerable
issues. The solution could have been `paradox` however this is as slow
as `ParameterSet`. `param6` is currently planned to be a very
lightweight design with two classes: `ParameterSet` and
`ParameterSetCollection` which can be built using R6 and Rcpp. A
critical difference is that `data.table` will no longer be used and
instead all information stored in named lists of fields.

### data.table vs. lists

Contrast two classes, one using `data.table` and the other `list`

``` r
library(R6)
library(microbenchmark)
class_lst <- R6Class("class_lst",
  public = list(
    initialize = function(...) {
      private$.values <- list(...)
      invisible(self)
    }
  ),
  
  active = list(
    values = function(x) {
      if (missing(x)) {
        return(private$.values)
      } else {
        private$.values <- x
      }
    }
  ),
  
  private = list(
    .values = list()
  ))

class_dt <- R6Class("class_dt",
  public = list(
    initialize = function(...) {
      private$.values <- data.table::data.table(values = list(...))
      invisible(self)
    }
  ),
  
  active = list(
    values = function(x) {
      if (missing(x)) {
        return(private$.values)
      } else {
        private$.values$values <- x
      }
    }
  ),
  
  private = list(
    .values = data.table::data.table()
  ))
```

First testing construction. `class_lst` should be much faster as
`class_dt` has to construct the `data.table`. This is an expected
overhead that can be ignored if speed gains are made once constructed.

``` r
microbenchmark(
  class_lst$new(1:100),
  class_dt$new(1:100)
)
```

    ## Unit: microseconds
    ##                  expr     min       lq      mean   median      uq        max
    ##  class_lst$new(1:100)  70.605  92.1885  115.4059 105.5525 119.650    280.376
    ##   class_dt$new(1:100) 249.527 290.2715 1926.9178 311.9535 375.539 150252.755
    ##  neval cld
    ##    100   a
    ##    100   a

Similarly we expect getting of values to be fairly similar on this small
scale

``` r
obj_lst <- class_lst$new(1:100)
obj_dt <- class_dt$new(1:100)

microbenchmark(
  obj_lst$values,
  obj_dt$values
)
```

    ## Unit: microseconds
    ##            expr   min    lq    mean median    uq    max neval cld
    ##  obj_lst$values 1.745 1.775 1.91648 1.8335 1.931  6.533   100   a
    ##   obj_dt$values 1.740 1.774 2.21580 1.8255 1.995 30.998   100   a

Finally setting values:

``` r
microbenchmark(
  {obj_lst$values = 1:10},
  {obj_dt$values = 1:10}
)
```

    ## Unit: microseconds
    ##                           expr    min     lq     mean  median      uq      max
    ##  {     obj_lst$values = 1:10 }  9.195 10.475 24.11598 12.0505 12.8315  435.799
    ##   {     obj_dt$values = 1:10 } 37.623 40.118 64.37647 41.8365 49.5910 1364.087
    ##  neval cld
    ##    100  a 
    ##    100   b

## paradox vs. distr6 vs. param6

Now comparing the preliminary param6 designs that use named lists and
set6 to the other two packages. Each will be compared with identical
parameters with no overheads like dependencies and checks. For a fair
comparison paradox values will be set at the same time.

First comparing construction without value setting, note that distr6
requires values.

``` r
library(paradox)
library(set6)
```

    ## 
    ## -----------------------------

    ##  set6 v0.1.8
    ## 
    ## Get started: ?set6
    ## Changelog:   set6News()

    ## -----------------------------

``` r
library(param6)
microbenchmark(
  {
    ParamSet$new(
      list(
        # Set of Reals.
        ParamDbl$new("a"),
        # Set of letters
        ParamFct$new("b", levels = letters),
        # Logical
        ParamLgl$new("c"),
        # Set of positive integers
        ParamInt$new("d", lower = 0)
      )
    )
  },
  
  {
  distr6::ParameterSet$new(
    id = list("a", "b", "c", "d"),
    support = list(Reals$new(), Set$new(elements = letters), LogicalSet$new(), PosIntegers$new()),
    value = list(a = 1, b = "a", c = TRUE, d = 1)
  )
  },
  
  {
    param6::ParameterSet$new(
      a = reals,
      b = Set$new(elements = letters),
      c = logical,
      d = pos_integers
    )
  }
)
```

    ## Registered S3 method overwritten by 'distr6':
    ##   method                     from  
    ##   as.data.table.ParameterSet param6

    ## Unit: milliseconds
    ##                                                                                                                                                                                                                             expr
    ##                                                                                     {     ParamSet$new(list(ParamDbl$new("a"), ParamFct$new("b", levels = letters),          ParamLgl$new("c"), ParamInt$new("d", lower = 0))) }
    ##  {     distr6::ParameterSet$new(id = list("a", "b", "c", "d"), support = list(Reals$new(),          Set$new(elements = letters), LogicalSet$new(), PosIntegers$new()),          value = list(a = 1, b = "a", c = TRUE, d = 1)) }
    ##                                                                                                             {     param6::ParameterSet$new(a = reals, b = Set$new(elements = letters),          c = logical, d = pos_integers) }
    ##       min       lq     mean   median       uq       max neval cld
    ##  5.257324 5.731020 8.067463 6.122838 8.082752  90.41462   100   b
    ##  5.351287 5.804845 8.762145 6.533771 8.585447 104.30130   100   b
    ##  1.003414 1.159612 2.016185 1.273620 1.800692  36.75729   100  a

Comparing with setting values.

``` r
microbenchmark(
  {
    paradox <- ParamSet$new(
      list(
        # Set of Reals.
        ParamDbl$new("a"),
        # Set of letters
        ParamFct$new("b", levels = letters),
        # Logical
        ParamLgl$new("c"),
        # Set of positive integers
        ParamInt$new("d", lower = 0)
      )
    )
    paradox$values = list(a = 1, b = "a", c = TRUE, d = 1)
  },
  
  {
  distr6 <- distr6::ParameterSet$new(
    id = list("a", "b", "c", "d"),
    support = list(Reals$new(), Set$new(elements = letters), LogicalSet$new(), PosIntegers$new()),
    value = list(a = 1, b = "a", c = TRUE, d = 1)
  )
  },
  
  {
    param6 <- param6::ParameterSet$new(
      a = reals ~ 1,
      b = Set$new(elements = letters) ~ "a",
      c = logical ~ TRUE,
      d = pos_integers ~ 1
    )
  }
)
```

    ## Unit: milliseconds
    ##                                                                                                                                                                                                                                                expr
    ##                         {     paradox <- ParamSet$new(list(ParamDbl$new("a"), ParamFct$new("b",          levels = letters), ParamLgl$new("c"), ParamInt$new("d",          lower = 0)))     paradox$values = list(a = 1, b = "a", c = TRUE, d = 1) }
    ##  {     distr6 <- distr6::ParameterSet$new(id = list("a", "b", "c",          "d"), support = list(Reals$new(), Set$new(elements = letters),          LogicalSet$new(), PosIntegers$new()), value = list(a = 1,          b = "a", c = TRUE, d = 1)) }
    ##                                                                                                 {     param6 <- param6::ParameterSet$new(a = reals ~ 1, b = Set$new(elements = letters) ~          "a", c = logical ~ TRUE, d = pos_integers ~ 1) }
    ##       min       lq     mean   median       uq      max neval cld
    ##  5.476341 5.922344 7.791594 6.573791 9.143246 27.85085   100   b
    ##  5.324970 5.792965 7.255303 6.355505 8.053247 14.88148   100   b
    ##  1.344963 1.518876 2.131033 1.638418 2.125412 10.65108   100  a

Comparing getting:

``` r
microbenchmark(
  paradox$values$c,
  distr6$getParameterValue("c"),
  param6$values$c
)
```

    ## Unit: microseconds
    ##                           expr     min       lq      mean   median       uq
    ##               paradox$values$c   2.202   2.5010   7.07980   3.7580   7.7050
    ##  distr6$getParameterValue("c") 427.338 432.9650 687.60384 458.9865 557.7095
    ##                param6$values$c   2.304   2.5155   6.12115   2.9420   7.1595
    ##        max neval cld
    ##    154.950   100  a 
    ##  10589.256   100   b
    ##     65.977   100  a

And setting:

``` r
vals = list(a = 1, b = "a", c = FALSE, d = 1000)
microbenchmark(
  {paradox$values = vals},
  distr6$setParameterValue(lst = vals),
  {param6$values = vals}
)
```

    ## Unit: microseconds
    ##                                  expr      min       lq      mean    median
    ##         {     paradox$values = vals }  243.962  293.426  911.4730  369.8255
    ##  distr6$setParameterValue(lst = vals) 4379.804 5134.441 8151.4545 5882.1870
    ##          {     param6$values = vals }  310.344  368.215  525.6492  416.9055
    ##        uq       max neval cld
    ##   485.313 49556.183   100  a 
    ##  9804.657 45075.253   100   b
    ##   518.811  4190.226   100  a

## Going Forward

distr6 parameter sets are clearly problematic and should be removed
ASAP. param6 provides a promising alternative. Restricting this package
to only two classes makes it a good use-case for C++ classes and linkage
via Rcpp.

### Next Steps:

1.  Draw up final designs for param6 using distr6 and paradox
2.  Implement ParameterSet in C++
3.  Interface with Rcpp - identify parts of Rcpp and R6 that may require
    further development
