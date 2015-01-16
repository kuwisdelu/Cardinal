
#include <R.h>
#include <math.h>

void gaussian_filter(double * x, double * x_new, int * nrow, int * ncol,
	double * sd, int * r, double * beta)
{
	int window = pow((2 * (*r)) + 1, 2);
	double gamma_raw[window];
	int ix = 0;
	for ( int i = 0; i < *nrow; ++i ) {
		for ( int j = 0; j < *ncol; ++j ) {
			int k = 0;
			double gamma_sum = 0;
			for ( int ii = -(*r); ii <= *r; ++ii ) {
				for ( int jj = -(*r); jj <= *r; ++jj ) {
					double alpha = exp(-(double)(ii*ii + jj*jj) / (2.0 * pow(*sd, 2)));
					gamma_raw[k] = alpha * beta[ix * window + k];
					gamma_sum += gamma_raw[k];
					++k;
				}
			}
			k = 0;
			for ( int ii = -(*r); ii <= *r; ++ii ) {
				for ( int jj = -(*r); jj <= *r; ++jj ) {
					int temp_i = i + ii;
					int temp_j = j + jj;
					if ( temp_i < 0 || temp_i > *nrow - 1 ) {
						temp_i = i;
					}
					if ( temp_j < 0 || temp_j > *ncol - 1 ) {
						temp_j = j;
					}
					if ( x[(temp_j * (*nrow)) + temp_i] < 0 ) {
						temp_i = i;
						temp_j = j;
					}
					double gamma = gamma_raw[k] / gamma_sum;
					x_new[(j * (*nrow)) + i] += gamma * x[(temp_j * (*nrow)) + temp_i];
					++k;
				}
			}
			++ix;
		}
	}
}

void bilateral_weights(double * x, double * beta, int * nrow, int * ncol, int * r) {
	int window = pow((2 * (*r)) + 1, 2);
	int ix = 0;
	for ( int i = 0; i < *nrow; ++i ) {
		for ( int j = 0; j < *ncol; ++j ) {
			int k = 0;
			for ( int ii = -(*r); ii <= *r; ++ii ) {
				for ( int jj = -(*r); jj <= *r; ++jj ) {
					int temp_i = i + ii;
					int temp_j = j + jj;
					if ( temp_i < 0 || temp_i > *nrow - 1 ) {
						temp_i = i;
					}
					if ( temp_j < 0 || temp_j > *ncol - 1 ) {
						temp_j = j;
					}
					if ( x[(temp_j * (*nrow)) + temp_i] < 0 ) {
						temp_i = i;
						temp_j = j;
					}
					beta[ix * window + k] = fabs(x[(j * (*nrow)) + i]
						- x[(temp_j * (*nrow)) + temp_i]);
					++k;
				}
			}
			double max_beta = 0;
			double min_beta = beta[ix * window];
			for ( int l = 0; l < window; ++l ) {
				if ( beta[ix * window + l] > max_beta )
					max_beta = beta[ix * window + l];
				if ( beta[ix * window + l] < min_beta )
					min_beta = beta[ix * window + l];
			}
			double lambda = (max_beta - min_beta) / 2.0;
			if ( lambda < 1e-9 ) {
				lambda = 1.0;
			}
			lambda = lambda * lambda;
			for ( int l = 0; l < window; ++l ) {
				beta[ix * window + l] = exp(-pow(beta[ix * window + l], 2) / (2.0 * lambda));
			}
			++ix;
		}
	}
}

void bilateral_filter(double * x, double * x_new, int * nrow, int * ncol,
	double * sd, int * r, double * beta)
{
	bilateral_weights(x, beta, nrow, ncol, r);
	gaussian_filter(x, x_new, nrow, ncol, sd, r, beta);
}

