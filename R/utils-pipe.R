#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


#' Assignment pipe
#'
#' See \code{magrittr::\link[magrittr:compound]{\%<>\%}} for details.
#'
#' @name %<>%
#' @rdname compound
#' @keywords internal
#' @export
#' @importFrom magrittr %<>%
#' @param lhs An object which serves both as the initial value and as target.
#' @param rhs a function call using the magrittr semantics.
NULL
