#' @name support_dictionary
#' @title Support Dictionary
#' @description [Dictionary] for parameter supports
#' @details See [Dictionary] for full details of how to add other [set6::Set]
#' objects as supports to this dictionary.
#' @examples
#' support_dictionary$keys
#' support_dictionary$items
#' @export
NULL
load_support <- function() {
    support_dictionary <- Dictionary$new(list(
        universal = Universal$new(),
        logicals = Logicals$new(),
        naturals = Naturals$new(),
        posnaturals = PosNaturals$new(),
        integers = Integers$new(),
        posintegers = PosIntegers$new(),
        negintegers = NegIntegers$new(),
        rationals = Rationals$new(),
        posrationals = PosRationals$new(),
        negrationals = NegRationals$new(),
        reals = Reals$new(),
        posreals = PosReals$new(),
        negreals = NegReals$new(),
        extendedreals = ExtendedReals$new(),
        complex = Complex$new(),
        proportion = Interval$new(0, 1),
        nlogicals = Logicals$new()^"n",
        nnaturals = Naturals$new()^"n",
        nposnaturals = PosNaturals$new()^"n",
        nintegers = Integers$new()^"n",
        nposintegers = PosIntegers$new()^"n",
        nnegintegers = NegIntegers$new()^"n",
        nrationals = Rationals$new()^"n",
        nposrationals = PosRationals$new()^"n",
        nnegrationals = NegRationals$new()^"n",
        nreals = Reals$new()^"n",
        nposreals = PosReals$new()^"n",
        nnegreals = NegReals$new()^"n",
        nextendedreals = ExtendedReals$new()^"n",
        ncomplex = Complex$new()^"n",
        nproportion = Interval$new(0, 1)^"n"
    ), types = "Set")
}
