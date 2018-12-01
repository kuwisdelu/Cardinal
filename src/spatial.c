
#include <R.h>
#include <math.h>

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
