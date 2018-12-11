
#include <R.h>
#include <Rdefines.h>

#include <cmath>

#include "utils.h"

template<typename T>
SEXP find_neighbors(SEXP coord, SEXP r, SEXP groups)
{
	int nrow = nrows(coord);
	int ncol = ncols(coord);
	T * pCoord = DataPtr<T>(coord);
	double * pR = REAL(r);
	int * pGroups = INTEGER(groups);
	bool is_neighbor[nrow];
	SEXP ret;
	PROTECT(ret = NEW_LIST(nrow));
	for ( int i = 0; i < nrow; ++i ) {
		int num_neighbors = 0;
		for ( int ii = 0; ii < nrow; ++ii ) {
			is_neighbor[ii] = true;
			if ( pGroups[i] != pGroups[ii] ) {
				is_neighbor[ii] = false;
				continue;
			}
			for ( int j = 0; j < ncol; ++j ) {
				double d = pCoord[j * nrow + i] - pCoord[j * nrow + ii];
				if ( fabs(d) > pR[j] )
					is_neighbor[ii] = false;
			}
			if ( is_neighbor[ii] )
				num_neighbors++;
		}
		SEXP neighbors;
		PROTECT(neighbors = NEW_INTEGER(num_neighbors));
		int * pNeighbors = INTEGER(neighbors);
		int ix = 0;
		for ( int ii = 0; ii < nrow && ix < num_neighbors; ++ii ) {
			if ( is_neighbor[ii] ) {
				pNeighbors[ix] = ii + 1;
				ix++;
			}
		}
		SET_VECTOR_ELT(ret, i, neighbors);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ret;
}

template<typename T>
SEXP find_spatial_blocks(SEXP coord, SEXP r, SEXP groups, SEXP block_info)
{
	int ngroups = LENGTH(block_info);
	int nrow = nrows(coord);
	int ncol = ncols(coord);
	T * pCoord = DataPtr<T>(coord);
	double * pR = REAL(r);
	int * pGroups = INTEGER(groups);
	bool within_block[nrow];
	SEXP ret;
	PROTECT(ret = NEW_LIST(ngroups));
	for ( int k = 0; k < ngroups; ++k ) {
		SEXP info = VECTOR_ELT(block_info, k);
		SEXP limits = VECTOR_ELT(info, 0);
		int * pGrid = INTEGER(VECTOR_ELT(info, 1));
		int nblocks = nrows(VECTOR_ELT(info, 1));
		SEXP blocks;
		PROTECT(blocks = NEW_LIST(nblocks));
		for ( int l = 0; l < nblocks; ++l ) {
			int blocksize = 0;
			for ( int i = 0; i < nrow; ++i ) {
				within_block[i] = true;
				if ( pGroups[i] != k + 1 ) {
					within_block[i] = false;
					continue;
				}
				for  ( int j = 0; j < ncol; ++j ) {
					double * pLimits = REAL(VECTOR_ELT(limits, j));
					int g = pGrid[j * nblocks + l];
					double lower = pLimits[g - 1];
					double upper = pLimits[g];
					if ( pCoord[j * nrow + i] <= lower - pR[j] )
						within_block[i] = false;
					if ( pCoord[j * nrow + i] > upper + pR[j] )
						within_block[i] = false;
				}
				if ( within_block[i] )
					blocksize++;
			}
			SEXP members;
			PROTECT(members = NEW_INTEGER(blocksize));
			int * pMembers = INTEGER(members);
			int ix = 0;
			for ( int ii = 0; ii < nrow && ix < blocksize; ++ii ) {
				if ( within_block[ii] ) {
					pMembers[ix] = ii + 1;
					ix++;
				}
			}
			SET_VECTOR_ELT(blocks, l, members);
			UNPROTECT(1);
		}
		SET_VECTOR_ELT(ret, k, blocks);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ret;
}

SEXP which_spatial_blocks(SEXP neighbors, SEXP blocks)
{
	int npixels = LENGTH(neighbors);
	int nblocks = LENGTH(blocks);
	SEXP which;
	PROTECT(which = NEW_INTEGER(npixels));
	int * pWhich = INTEGER(which);
	int * pNeighbors, * pBlock;
	for ( int i = 0; i < npixels; ++i ) {
		pWhich[i] = NA_INTEGER;
		for ( int j = 0; j < nblocks; ++j ) {
			int num_neighbors = LENGTH(VECTOR_ELT(neighbors, i));
			int blocksize = LENGTH(VECTOR_ELT(blocks, j));
			pNeighbors = INTEGER(VECTOR_ELT(neighbors, i));
			pBlock = INTEGER(VECTOR_ELT(blocks, j));
			bool neighborhood_ok = true;
			for ( int k = 0; k < num_neighbors; ++k ) {
				bool id_ok = false;
				for ( int l = 0; l < blocksize; ++l ) {
					if ( pNeighbors[k] == pBlock[l] ) {
						id_ok = true;
						break;
					}
				}
				if ( !id_ok ) {
					neighborhood_ok = false;
					break;
				}
			}
			if ( neighborhood_ok ) {
				pWhich[i] = j + 1;
				break;
			}
		}
	}
	UNPROTECT(1);
	return which;
}

template<typename T>
SEXP get_spatial_offsets(SEXP coord, SEXP neighbors, int k)
{
	int nrow = LENGTH(neighbors);
	int ncol = ncols(coord);
	int n = nrows(coord);
	T * pCoord = DataPtr<T>(coord);
	int * ii = INTEGER(neighbors);
	SEXP offsets;
	PROTECT(offsets = allocMatrix(DataType<T>(), nrow, ncol));
	T * pOffsets = DataPtr<T>(offsets);
	for ( int i = 0; i < nrow; ++i )
		for ( int j = 0; j < ncol; ++j )
			pOffsets[j * nrow + i] = pCoord[j * n + ii[i]] - pCoord[j * n + k];
	UNPROTECT(1);
	return offsets;
}

template<typename T1, typename T2>
SEXP get_spatial_weights(SEXP x, SEXP offsets, double sigma, bool bilateral)
{
	int npixels = nrows(offsets);
	int ndims = ncols(offsets);
	SEXP w, alpha, beta;
	PROTECT(w = NEW_LIST(2));
	PROTECT(alpha = NEW_NUMERIC(npixels));
	PROTECT(beta = NEW_NUMERIC(npixels));
	double * pAlpha = REAL(alpha);
	double * pBeta = REAL(beta);
	T2 * pOffsets = DataPtr<T2>(offsets);
	int k; // center pixel
	bool is_center;
	double d1, d2, sigma2 = sigma * sigma;
	for ( int i = 0; i < npixels; ++i ) {
		d2 = 0;
		is_center = true;
		for ( int j = 0; j < ndims; ++j ) {
			d1 = pOffsets[j * npixels + i];
			d2 += d1 * d1;
			if ( pOffsets[j * npixels + i] != 0 )
				is_center = false;
		}
		pAlpha[i] = exp(-d2 / (2 * sigma2));
		if ( is_center )
			k = i;
	}
	if ( bilateral )
	{
		int nfeatures = nrows(x);
		T1 * pX = DataPtr<T1>(x);
		double lambda, max_d2 = R_NegInf, min_d2 = R_PosInf;
		for ( int i = 0; i < npixels; ++i ) {
			d2 = 0;
			for ( int j = 0; j < nfeatures; ++j ) {
				d1 = pX[i * nfeatures + j] - pX[k * nfeatures + j];
				d2 += d1 * d1;
			}
			if ( d2 > max_d2 )
				max_d2 = d2;
			if ( d2 < min_d2 )
				min_d2 = d2;
			pBeta[i] = d2;
		}
		lambda = (sqrt(max_d2) - sqrt(min_d2)) / 2;
		lambda = lambda * lambda;
		for ( int i = 0; i < npixels; ++i )
			pBeta[i] = exp(-pBeta[i] / (2 * lambda));
	}
	else
	{
		for ( int i = 0; i < npixels; ++i )
			pBeta[i] = 1;
	}
	SET_VECTOR_ELT(w, 0, alpha);
	SET_VECTOR_ELT(w, 1, beta);
	UNPROTECT(3);
	return w;
}

template<typename T1, typename T2>
double get_spatial_distance(SEXP x, SEXP y, SEXP x_offsets, SEXP y_offsets,
	SEXP x_weights, SEXP y_weights, double tol_dist)
{
	int ndims = ncols(x_offsets);
	int nfeatures = nrows(x);
	int nx = ncols(x);
	int ny = ncols(y);
	double * x_alpha = REAL(VECTOR_ELT(x_weights, 0));
	double * x_beta = REAL(VECTOR_ELT(x_weights, 1));
	double * y_alpha = REAL(VECTOR_ELT(y_weights, 0));
	double * y_beta = REAL(VECTOR_ELT(y_weights, 1));
	T1 * pX = DataPtr<T1>(x);
	T1 * pY = DataPtr<T1>(y);
	T2 * pX_offsets = DataPtr<T2>(x_offsets);
	T2 * pY_offsets = DataPtr<T2>(y_offsets);
	double d1, d2, alpha_i, beta_i, a_i, dist1, dist2 = 0;
	for ( int ix = 0; ix < nx; ++ix ) {
		for ( int iy = 0; iy < ny; ++iy ) {
			d2 = 0;
			for ( int k = 0; k < ndims; ++k ) {
				d1 = pX_offsets[k * nx + ix] - pY_offsets[k * ny + iy];
				d2 += d1 * d1;
			}
			if ( d2 < tol_dist ) {
				alpha_i = x_alpha[ix] * y_alpha[iy];
				beta_i = x_beta[ix] * y_beta[iy];
				a_i = sqrt(alpha_i * beta_i);
				for ( int j = 0; j < nfeatures; ++j ) {
					dist1 = pX[ix * nfeatures + j] - pY[iy * nfeatures + j];
					dist2 += a_i * dist1 * dist1;
				}
			}
		}
	}
	return sqrt(dist2);
}

template<typename T1, typename T2>
SEXP get_spatial_zscores(SEXP x, SEXP ref, SEXP weights, SEXP sd)
{
	int nfeatures = nrows(x);
	int npixels = ncols(x);
	int nrefs = ncols(ref);
	double * alpha = REAL(VECTOR_ELT(weights, 0));
	double * beta = REAL(VECTOR_ELT(weights, 1));
	double * sdev = REAL(sd);
	T1 * pX = DataPtr<T1>(x);
	T2 * pRef = DataPtr<T2>(ref);
	SEXP scores;
	PROTECT(scores = NEW_NUMERIC(nrefs));
	double * pScores = REAL(scores);
	double a_i, dist, wsum = 0;
	for ( int i = 0; i < npixels; ++i )
		wsum += alpha[i] * beta[i];
	for ( int k = 0; k < nrefs; ++k ) {
		pScores[k] = 0;
		for ( int i = 0; i < npixels; ++i ) {
			a_i = alpha[i] * beta[i] / wsum;
			double score_i = 0;
			for ( int j = 0; j < nfeatures; ++j ) {
				dist = pX[i * nfeatures + j] - pRef[k * nfeatures + j];
				score_i += (dist * dist) / (sdev[j] * sdev[j]);
			}
			pScores[k] += a_i * score_i;
		}
	}
	UNPROTECT(1);
	return scores;
}

extern "C" {

	SEXP findNeighbors(SEXP coord, SEXP r, SEXP group) {
		if ( TYPEOF(coord) == INTSXP )
			return find_neighbors<int>(coord, r, group);
		else if ( TYPEOF(coord) == REALSXP )
			return find_neighbors<double>(coord, r, group);
		else
			return R_NilValue;
	}

	SEXP findSpatialBlocks(SEXP coord, SEXP r, SEXP group, SEXP block_info) {
		if ( TYPEOF(coord) == INTSXP )
			return find_spatial_blocks<int>(coord, r, group, block_info);
		else if ( TYPEOF(coord) == REALSXP )
			return find_spatial_blocks<double>(coord, r, group, block_info);
		else
			return R_NilValue;
	}

	SEXP whichSpatialBlocks(SEXP neighbors, SEXP blocks) {
		return which_spatial_blocks(neighbors, blocks);
	}

	SEXP spatialOffsets(SEXP coord, SEXP neighbors, SEXP k) {
		if ( TYPEOF(coord) == INTSXP )
			return get_spatial_offsets<int>(coord, neighbors, asInteger(k));
		else if ( TYPEOF(coord) == REALSXP )
			return get_spatial_offsets<double>(coord, neighbors, asInteger(k));
		else
			return R_NilValue;
	}

	SEXP spatialWeights(SEXP x, SEXP offsets, SEXP sigma, SEXP bilateral) {
		if ( TYPEOF(x) == INTSXP && TYPEOF(offsets) == INTSXP )
			return get_spatial_weights<int,int>(x, offsets, asReal(sigma), asLogical(bilateral));
		else if ( TYPEOF(x) == INTSXP && TYPEOF(offsets) == REALSXP )
			return get_spatial_weights<int,double>(x, offsets, asReal(sigma), asLogical(bilateral));
		else if ( TYPEOF(x) == REALSXP && TYPEOF(offsets) == INTSXP )
			return get_spatial_weights<double,int>(x, offsets, asReal(sigma), asLogical(bilateral));
		else if ( TYPEOF(x) == REALSXP && TYPEOF(offsets) == REALSXP )
			return get_spatial_weights<double,double>(x, offsets, asReal(sigma), asLogical(bilateral));
		else
			return R_NilValue;
	}

	SEXP spatialDistance(SEXP x, SEXP y, SEXP x_offsets, SEXP y_offsets,
		SEXP x_weights, SEXP y_weights, SEXP tol_dist)
	{
		if ( TYPEOF(x) == INTSXP && TYPEOF(x_offsets) == INTSXP )
			return ScalarReal(get_spatial_distance<int,int>(x, y, x_offsets, y_offsets, x_weights, y_weights, asReal(tol_dist)));
		else if ( TYPEOF(x) == INTSXP && TYPEOF(x_offsets) == REALSXP )
			return ScalarReal(get_spatial_distance<int,double>(x, y, x_offsets, y_offsets, x_weights, y_weights, asReal(tol_dist)));
		else if ( TYPEOF(x) == REALSXP && TYPEOF(x_offsets) == INTSXP )
			return ScalarReal(get_spatial_distance<double,int>(x, y, x_offsets, y_offsets, x_weights, y_weights, asReal(tol_dist)));
		else if ( TYPEOF(x) == REALSXP && TYPEOF(x_offsets) == REALSXP )
			return ScalarReal(get_spatial_distance<double,double>(x, y, x_offsets, y_offsets, x_weights, y_weights, asReal(tol_dist)));
		else
			return R_NilValue;
	}

	SEXP spatialZScores(SEXP x, SEXP ref, SEXP weights, SEXP sd) {
		if ( TYPEOF(x) == INTSXP && TYPEOF(ref) == INTSXP )
			return get_spatial_zscores<int,int>(x, ref, weights, sd);
		else if ( TYPEOF(x) == INTSXP && TYPEOF(ref) == REALSXP )
			return get_spatial_zscores<int,double>(x, ref, weights, sd);
		else if ( TYPEOF(x) == REALSXP && TYPEOF(ref) == INTSXP )
			return get_spatial_zscores<double,int>(x, ref, weights, sd);
		else if ( TYPEOF(x) == REALSXP && TYPEOF(ref) == REALSXP )
			return get_spatial_zscores<double,double>(x, ref, weights, sd);
		else
			return R_NilValue;
	}

}

