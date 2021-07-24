#' @name support_dictionary
#' @title Support Dictionary
#' @description [dictionar6::Dictionary] for parameter supports
#' @details See [dictionar6::Dictionary] for full details of how to add other
#' [set6::Set] objects as supports to this dictionary.
#' @examples
#' support_dictionary$keys
#' support_dictionary$items
#' @export
NULL
load_support <- function() {
    support_dictionary <- dct(
        universal = Universal$new(),
        logicals = Logicals$new(),
        naturals = Naturals$new(),
        posnaturals = PosNaturals$new(),
        integers = Integers$new(),
        posintegers = PosIntegers$new(),
        negintegers = NegIntegers$new(),
        posintegers0 = PosIntegers$new(zero = TRUE),
        negintegers0 = NegIntegers$new(zero = TRUE),
        rationals = Rationals$new(),
        posrationals = PosRationals$new(),
        negrationals = NegRationals$new(),
        posrationals0 = PosRationals$new(zero = TRUE),
        negrationals0 = NegRationals$new(zero = TRUE),
        reals = Reals$new(),
        posreals = PosReals$new(),
        negreals = NegReals$new(),
        posreals0 = PosReals$new(zero = TRUE),
        negreals0 = NegReals$new(zero = TRUE),
        extreals = ExtendedReals$new(),
        complex = Complex$new(),
        proportion = Interval$new(0, 1),
        nlogicals = Logicals$new()^"n",
        nnaturals = Naturals$new()^"n",
        nposnaturals = PosNaturals$new()^"n",
        nintegers = Integers$new()^"n",
        nposintegers = PosIntegers$new()^"n",
        nnegintegers = NegIntegers$new()^"n",
        nposintegers0 = PosIntegers$new(zero = TRUE)^"n",
        nnegintegers0 = NegIntegers$new(zero = TRUE)^"n",
        nrationals = Rationals$new()^"n",
        nposrationals = PosRationals$new()^"n",
        nnegrationals = NegRationals$new()^"n",
        nposrationals0 = PosRationals$new(zero = TRUE)^"n",
        nnegrationals0 = NegRationals$new(zero = TRUE)^"n",
        nreals = Reals$new()^"n",
        nposreals = PosReals$new()^"n",
        nnegreals = NegReals$new()^"n",
        nposreals0 = PosReals$new(zero = TRUE)^"n",
        nnegreals0 = NegReals$new(zero = TRUE)^"n",
        nextreals = ExtendedReals$new()^"n",
        ncomplex = Complex$new()^"n",
        nproportion = Interval$new(0, 1)^"n",
        types = "Set"
    )
}
