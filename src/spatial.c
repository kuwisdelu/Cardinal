
#include <R.h>
#include <math.h>
#include "fastmap.h"

struct spectra_spatial {
	double * intensities;
	int nfeatures;
	int npixels;
	int * neighbors;
	int nneighbors;
	double * w;
	double * alpha;
	double * beta;
};

double d_spatial_featurewise(const void * data, int i, int j, int feature) {
	const struct spectra_spatial * spectra = (struct spectra_spatial *) data;
	double d = 0;
	for ( int k = 0; k < spectra->nneighbors; ++k ) {
		int n_i = spectra->neighbors[(k * spectra->npixels) + i];
		int n_j = spectra->neighbors[(k * spectra->npixels) + j];
		double beta_i = spectra->beta[(k * spectra->npixels) + i];
		double beta_j = spectra->beta[(k * spectra->npixels) + j];
		double w = spectra->w[feature];
		double alpha_ij = spectra->alpha[k] * sqrt(beta_i * beta_j);
		double s = pow(spectra->intensities[(n_i * spectra->nfeatures) + feature] -
			spectra->intensities[(n_j * spectra->nfeatures) + feature], 2);
		d += w * alpha_ij * s;
	}
	return d;
}

double dist_spatial(const void * objects, int i, int j) {
	const struct spectra_spatial * spectra = (struct spectra_spatial *) objects;
	double dist = 0;
	for ( int k = 0; k < spectra->nfeatures; ++k ) {
		dist += d_spatial_featurewise(objects, i, j, k);
	}
	return sqrt(dist);
}

double score_spatial(const void * objects, int i, int c, double * centroids, double * sd) {
	const struct spectra_spatial * spectra = (struct spectra_spatial *) objects;
	double gamma[spectra->nneighbors];
	double dgamma = 0;
	for ( int j = 0; j < spectra->nneighbors; ++j ) {
		gamma[j] = spectra->alpha[j] * spectra->beta[(j * spectra->npixels) + i];
		dgamma += gamma[j];
	}
	double score = 0;
	for ( int j = 0; j < spectra->nneighbors; ++j ) {
		int n_i = spectra->neighbors[(j * spectra->npixels) + i];
		double s = 0;
		for ( int k = 0; k < spectra->nfeatures; ++k ) {
			s += spectra->w[k] * pow(spectra->intensities[(n_i * spectra->nfeatures) + k]
				- centroids[(c * spectra->nfeatures) + k], 2) / pow(sd[k], 2);
		}
		score += gamma[j] * s / dgamma;
	}
	return score;	
}

void discriminant_scores_spatial(double * intensities, int * nfeatures, int * npixels,
	int * neighbors, int * nneighbors, double * w, double * alpha, double * beta,
	double * centroids, int * ncentroids, double * sd,
	double * discriminant_scores)
{
	struct spectra_spatial spectra = { intensities, *nfeatures, *npixels, neighbors,
		*nneighbors, w, alpha, beta };
	for ( int c = 0; c < *ncentroids; ++c ) {
		for ( int i = 0; i < *npixels; ++i ) {
			discriminant_scores[(c * (*npixels)) + i] = score_spatial((void *)
				&spectra, i, c, centroids, sd);
		}
	}
}

void fastmap_spatial(double * intensities, int * nfeatures, int * npixels, int * neighbors,
	int * nneighbors, double * w, double * alpha, double * beta, double * x_scores,
	int * pivot_array, int * ncomp)
{
	struct spectra_spatial spectra = { intensities, *nfeatures, *npixels, neighbors,
		*nneighbors, w, alpha, beta };
	struct matrix x_new = { x_scores, *npixels, *ncomp };
	struct pivot_array pivot_objects = { pivot_array, pivot_array + *ncomp, *ncomp };
	fastmap((void *) &spectra, &x_new, &pivot_objects, *npixels, *ncomp, 1, 1, 1e-6,
		dist_spatial);
}
