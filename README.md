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
    ##                  expr     min       lq     mean   median       uq       max
    ##  class_lst$new(1:100)  65.350  87.1975 1789.712 106.4980 156.7575 154577.53
    ##   class_dt$new(1:100) 245.743 323.9840  632.891 391.8425 523.1960  10280.97
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
    ##            expr   min    lq    mean median     uq    max neval cld
    ##  obj_lst$values 1.749 1.765 2.06841 1.7815 1.8315 28.467   100   a
    ##   obj_dt$values 1.726 1.763 1.84403 1.7795 1.8140  6.580   100   a

Finally setting values:

``` r
microbenchmark(
  {obj_lst$values = 1:10},
  {obj_dt$values = 1:10}
)
```

    ## Unit: microseconds
    ##                           expr    min      lq     mean  median      uq     max
    ##  {     obj_lst$values = 1:10 }  9.277 11.2920 17.45571 14.1300 21.1910  64.259
    ##   {     obj_dt$values = 1:10 } 36.699 40.5135 73.67498 44.0915 86.1435 441.320
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

    ## Registered S3 methods overwritten by 'distr6':
    ##   method                               from  
    ##   as.data.table.ParameterSet           param6
    ##   as.data.table.ParameterSetCollection param6

    ## Unit: milliseconds
    ##                                                                                                                                                                                                                             expr
    ##                                                                                     {     ParamSet$new(list(ParamDbl$new("a"), ParamFct$new("b", levels = letters),          ParamLgl$new("c"), ParamInt$new("d", lower = 0))) }
    ##  {     distr6::ParameterSet$new(id = list("a", "b", "c", "d"), support = list(Reals$new(),          Set$new(elements = letters), LogicalSet$new(), PosIntegers$new()),          value = list(a = 1, b = "a", c = TRUE, d = 1)) }
    ##                                                                                                             {     param6::ParameterSet$new(a = reals, b = Set$new(elements = letters),          c = logical, d = pos_integers) }
    ##       min       lq      mean   median        uq       max neval cld
    ##  5.267048 6.195205 11.750514 7.479071 11.614162 115.57197   100   b
    ##  5.433093 6.904050 12.421713 9.056404 13.509445  94.59755   100   b
    ##  1.075794 1.301413  2.564688 1.538518  2.505325  36.65623   100  a

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
    ##       min       lq      mean   median        uq       max neval cld
    ##  5.562362 6.516697 11.163608 8.600394 11.574094  60.48960   100   b
    ##  5.354928 6.210357 12.516330 7.423178 10.076752 148.17964   100   b
    ##  1.405570 1.564940  4.084472 1.736196  3.395454  70.68572   100  a

Comparing getting:

``` r
microbenchmark(
  paradox$values$c,
  distr6$getParameterValue("c"),
  param6$values$c
)
```

    ## Unit: microseconds
    ##                           expr     min       lq      mean  median       uq
    ##               paradox$values$c   2.224   2.8035   6.36992   5.023   8.2060
    ##  distr6$getParameterValue("c") 427.091 447.7830 759.02892 505.582 624.8225
    ##                param6$values$c   2.277   2.6615   6.67919   5.829   8.4600
    ##        max neval cld
    ##     45.050   100  a 
    ##  13008.899   100   b
    ##     29.812   100  a

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
    ##                                  expr      min        lq      mean    median
    ##         {     paradox$values = vals }  249.999  346.2740  2475.891  402.4500
    ##  distr6$setParameterValue(lst = vals) 4949.253 6281.8420 14989.545 8933.0670
    ##          {     param6$values = vals }  350.875  450.5635  1411.362  563.9035
    ##          uq       max neval cld
    ##    690.4195 120944.55   100  a 
    ##  14669.8155 168123.95   100   b
    ##    981.5985  31612.51   100  a

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
