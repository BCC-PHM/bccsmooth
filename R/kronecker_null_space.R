#' Structure matrix and null-space constraints for a Kronecker interaction
#'
#' Internal helper used to build space-time, space-age and age-time
#' interaction terms. Forms the Kronecker product of two structure
#' matrices and returns the eigenvectors spanning its null space. Those
#' are the patterns the interaction can take at zero penalty, which would
#' otherwise duplicate the main effects. Passing them to `extraconstr`
#' forces the interaction to be a pure deviation.
#'
#' The order of `X` and `Y` sets the cell ordering: `X` is the outer index
#' and `Y` the inner one, so the interaction id must be built as
#' `(x_id - 1) * nrow(Y) + y_id`.
#'
#' The eigen decomposition is of the full product, which is fine at ward
#' level but slow and memory-hungry for large grids (e.g. LSOA x age).
#'
#' @param X Outer structure matrix, e.g. `Qs` (ICAR).
#' @param Y Inner structure matrix, e.g. `Qe` (RW1 across age bands).
#' @returns An unnamed list: `[[1]]` the structure matrix `kronecker(X, Y)`
#'   for `Cmatrix`, and `[[2]]` the constraint matrix `A_delta` (one row
#'   per null-space eigenvector, one column per cell) for `extraconstr`.
#'   Use `nrow(A_delta)` as `rankdef`.
#' @keywords internal
kronecker_null_space = function(X,Y){
  k_product = kronecker(X, Y)
  eig        = eigen(k_product, symmetric = TRUE)
  A_delta   = t(eig$vectors[, abs(eig$values) < 1e-7])

  return(list(k_product, A_delta))
}
