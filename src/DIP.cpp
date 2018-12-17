
#include <R.h>
#include <Rinternals.h>

#include <cmath>

#include "utils.h"

template<typename T>
SEXP gaussian_filter(SEXP x, int r, double sd, SEXP weights)
{
	int nrow = nrows(x);
	int ncol = ncols(x);
	int size = static_cast<int>(pow((2 * static_cast<double>(r)) + 1, 2));
	double gamma[size];
	double alpha, beta;
	int ix = 0;
	T * pX = DataPtr<T>(x);
	SEXP x_new;
	PROTECT(x_new = allocMatrix(REALSXP, nrow, ncol));
    double * pX_new = REAL(x_new);
	for ( int i = 0; i < nrow; ++i ) {
		for ( int j = 0; j < ncol; ++j ) {
			if ( !DataValid<T>(pX[j * nrow + i]) )
			{
				pX_new[j * nrow + i] = NA_REAL;
				++ix;
				continue;
			}
			else
			{
				pX_new[j * nrow + i] = 0;
			}
			int k = 0;
			double gamma_sum = 0;
			for ( int ii = -r; ii <= r; ++ii ) {
				for ( int jj = -r; jj <= r; ++jj ) {
					int oi = i + ii;
					int oj = j + jj;
					bool valid = true;
					if ( oi < 0 || oi > nrow - 1 )
						valid = false;
					if ( oj < 0 || oj > ncol - 1 )
						valid = false;
					if ( !DataValid<T>(pX[oj * nrow + oi]) )
						valid = false;
					alpha = exp(-static_cast<double>(ii * ii + jj * jj) / (2 * pow(sd, 2)));
					if ( weights == R_NilValue )
						beta = 1;
					else
						beta = REAL(weights)[ix * size + k];
					if ( valid )
						gamma[k] = alpha * beta;
					else
						gamma[k] = 0;
					gamma_sum += gamma[k];
					++k;
				}
			}
			k = 0;
			for ( int ii = -r; ii <= r; ++ii ) {
				for ( int jj = -r; jj <= r; ++jj ) {
					int oi = i + ii;
					int oj = j + jj;
					if ( gamma[k] > 0 ) {
						double gamma_norm = gamma[k] / gamma_sum;
						pX_new[j * nrow + i] += gamma_norm * pX[oj * nrow + oi];
					}
					++k;
				}
			}
			++ix;
		}
	}
	UNPROTECT(1);
	return x_new;
}

template<typename T>
SEXP bilateral_weights(SEXP x, int r) {
	int nrow = nrows(x);
	int ncol = ncols(x);
	int size = pow((2 * r) + 1, 2);
	int ix = 0;
	T * pX = DataPtr<T>(x);
	SEXP beta;
	PROTECT(beta = allocMatrix(REALSXP, size, LENGTH(x)));
    double * pBeta = REAL(beta);
	for ( int i = 0; i < nrow; ++i ) {
		for ( int j = 0; j < ncol; ++j ) {
			int k = 0;
			for ( int ii = -r; ii <= r; ++ii ) {
				for ( int jj = -r; jj <= r; ++jj ) {
					int oi = i + ii;
					int oj = j + jj;
					bool valid = true;
					if ( oi < 0 || oi > nrow - 1 )
						valid = false;
					if ( oj < 0 || oj > ncol - 1 )
						valid = false;
					if ( !DataValid<T>(pX[oj * nrow + oi]) )
						valid = false;
					if ( valid )
					{
						pBeta[ix * size + k] = fabs(pX[j * nrow + i]
							- pX[oj * nrow + oi]);
						++k;
					}
					else
					{
						pBeta[ix * size + k] = NA_REAL;
						++k;	
					}
				}
			}
			double max_beta = 0;
			double min_beta = pBeta[ix * size];
			for ( int l = 0; l < size; ++l ) {
				double beta_l = pBeta[ix * size + l];
				if ( beta_l > max_beta )
					max_beta = beta_l;
				if ( beta_l < min_beta && beta_l > 0 )
					min_beta = beta_l;
			}
			double lambda = (max_beta - min_beta) / 2;
			lambda = lambda * lambda;
			for ( int l = 0; l < size; ++l ) {
				if ( ISNA(pBeta[ix * size + l]) )
					pBeta[ix * size + l] = 0;
				else
					pBeta[ix * size + l] = exp(-pow(pBeta[ix * size + l], 2) / (2 * lambda));
			}
			++ix;
		}
	}
	UNPROTECT(1);
	return beta;
}


extern "C" {

	SEXP gaussianFilter(SEXP x, SEXP r, SEXP sd) {
		if ( TYPEOF(x) == INTSXP )
			return gaussian_filter<int>(x, asInteger(r), asReal(sd), R_NilValue);
		else if ( TYPEOF(x) == REALSXP )
			return gaussian_filter<double>(x, asInteger(r), asReal(sd), R_NilValue);
		else
			return R_NilValue;
	}

	SEXP bilateralFilter(SEXP x, SEXP r, SEXP sd) {
		SEXP x_new, weights;
		if ( TYPEOF(x) == INTSXP )
		{
			PROTECT(weights = bilateral_weights<int>(x, asInteger(r)));
			PROTECT(x_new = gaussian_filter<int>(x, asInteger(r), asReal(sd), weights));
			UNPROTECT(2);
		}
		else if ( TYPEOF(x) == REALSXP )
		{
			PROTECT(weights = bilateral_weights<double>(x, asInteger(r)));
			PROTECT(x_new = gaussian_filter<double>(x, asInteger(r), asReal(sd), weights));
			UNPROTECT(2);
		}
		else
		{
			x_new = R_NilValue;
		}
		return x_new;
	}

}

