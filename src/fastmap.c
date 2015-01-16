
#include "fastmap.h"

#include <R.h>
#include <R_ext/Utils.h>
#include <math.h>

double dist_euclidean(const void * objects, int i, int j) {
	const struct matrix * x = (struct matrix *) objects;
	double d2 = 0;
	for ( int k = 0; k < x->ncol; ++k ) {
		d2 += pow(x->data[(k * x->nrow) + i] - x->data[(k * x->nrow) + j], 2);
	}
	return sqrt(d2);
}

double dist_fastmap(const void * objects, const struct matrix * x_new,
	int i, int j, double (*dist) (const void *, int, int))
{
	double d2 = pow(dist(objects, i, j), 2) - pow(dist_euclidean((void *) x_new, i, j), 2);
	if ( d2 > 0 ) {
		return sqrt(d2);	
	} else {
		return 0;
	}
}

void choose_distant_objects(const void * objects, const struct matrix * x_new,
	struct pivot_array * pivot_objects, int nobjects, int col,
	int niter, double (*dist) (const void *, int, int))
{
	int o_a = 0;
	int o_b;
	int o_max;
	double dist_max;
	double dist_temp;
	for ( int i = 0; i < niter; ++i ) {
		o_max = 0;
		dist_max = 0;
		for ( int j = 0; j < nobjects; ++j ) {
			dist_temp = dist_fastmap(objects, x_new, o_a, j, dist);
			if ( dist_temp > dist_max ) {
				o_max = j;
				dist_max = dist_temp;
			}
		}
		o_b = o_max;
		dist_max = 0;
		for ( int k = 0; k < nobjects; ++k ) {
			dist_temp = dist_fastmap(objects, x_new, o_b, k, dist);
			if ( dist_temp > dist_max ) {
				o_max = k;
				dist_max = dist_temp;
			}	
		}
		o_a = o_max;
	}
	pivot_objects->a[col] = o_a;
	pivot_objects->b[col] = o_b;
}

void fastmap(const void * objects, struct matrix * x_new,
	struct pivot_array * pivot_objects, int nobjects, int ncomp, int nstart, int niter,
	double tol, double (*dist) (const void *, int, int))
{
	double x_temp[nobjects];
	for ( int col = nstart - 1; col < ncomp; ++col ) {
		void R_CheckUserInterrupt(void);
		choose_distant_objects(objects, x_new, pivot_objects, nobjects, col, niter, dist);
		double d_ab = dist_fastmap(objects, x_new, pivot_objects->a[col],
			pivot_objects->b[col], dist);
		if ( d_ab > tol ) {
			for ( int i = 0; i < nobjects; i++ ) {
				double d_ai = dist_fastmap(objects, x_new, i, pivot_objects->a[col], dist);
				double d_bi = dist_fastmap(objects, x_new, i, pivot_objects->b[col], dist);
				x_temp[i] = (pow(d_ai, 2) + pow(d_ab, 2) - pow(d_bi, 2)) / (2.0 * d_ab);
			}
			for ( int i = 0; i < nobjects; i++ ) {
				x_new->data[(col * x_new->nrow) + i] = x_temp[i];
			}
		} else {
			for ( int i = 0; i < nobjects; i++ ) {
				x_new->data[(col * x_new->nrow) + i] = 0;
			}
		}
	}
}

void fastmap_euclidean(double * input, double * output, int * pivot_array,
	int * nrow, int * ncol, int * ncomp)
{
	struct matrix objects = { input, *nrow, *ncol };
	struct matrix x_new = { output, *nrow, *ncomp };
	struct pivot_array pivot_objects = { pivot_array, pivot_array + *ncomp, *ncomp };
	fastmap((void *) &objects, &x_new, &pivot_objects, *nrow, *ncomp, 1, 1, 1e-6,
		dist_euclidean);
}

