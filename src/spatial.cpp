
#include "Cardinal.h"

#include <cmath>

#define RADIAL		1
#define MANHATTAN 	2
#define MINKOWSKI	3
#define CHEBYSHEV	4

template<typename T>
SEXP find_neighbors(SEXP coord, SEXP r, SEXP groups, SEXP dist)
{
	int nrow = Rf_nrows(coord);
	int ncol = Rf_ncols(coord);
	T * pCoord = DataPtr<T>(coord);
	int * pGroups = INTEGER(groups);
	bool is_neighbor[nrow];
	double R = Rf_asReal(r);
	int dist_type = Rf_asInteger(dist);
	SEXP ret;
	PROTECT(ret = Rf_allocVector(VECSXP, nrow));
	for ( int i = 0; i < nrow; ++i ) {
		int num_neighbors = 0;
		for ( int ii = 0; ii < nrow; ++ii ) {
			is_neighbor[ii] = true;
			if ( pGroups[i] != pGroups[ii] ) {
				is_neighbor[ii] = false;
				continue;
			}
			double d, d2 = 0;
			for ( int j = 0; j < ncol; ++j ) {
				d = pCoord[j * nrow + i] - pCoord[j * nrow + ii];
				switch ( dist_type ) {
					case RADIAL:
						d2 += d * d;
						break;
					case MANHATTAN:
						d2 += fabs(d);
						break;
					case MINKOWSKI:
						d2 += pow(fabs(d), ncol);
					case CHEBYSHEV:
						d2 = fabs(d) > d2 ? fabs(d) : d2;
						break;
				}
			}
			switch ( dist_type ) {
				case RADIAL:
					is_neighbor[ii] = sqrt(d2) <= R ? true : false;
					break;
				case MANHATTAN:
					is_neighbor[ii] = d2 <= R ? true : false;
					break;
				case MINKOWSKI:
					is_neighbor[ii] = pow(d2, 1.0 / ncol) <= R ? true : false;
					break;
				case CHEBYSHEV:
					is_neighbor[ii] = d2 <= R ? true : false;
					break;
			}
			if ( is_neighbor[ii] )
				num_neighbors++;
		}
		SEXP neighbors;
		PROTECT(neighbors = Rf_allocVector(INTSXP, num_neighbors));
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
	int nrow = Rf_nrows(coord);
	int ncol = Rf_ncols(coord);
	T * pCoord = DataPtr<T>(coord);
	double * pR = REAL(r);
	int * pGroups = INTEGER(groups);
	bool within_block[nrow];
	SEXP ret;
	PROTECT(ret = Rf_allocVector(VECSXP, ngroups));
	for ( int k = 0; k < ngroups; ++k ) {
		SEXP info = VECTOR_ELT(block_info, k);
		SEXP limits = VECTOR_ELT(info, 0);
		int * pGrid = INTEGER(VECTOR_ELT(info, 1));
		int nblocks = Rf_nrows(VECTOR_ELT(info, 1));
		SEXP blocks;
		PROTECT(blocks = Rf_allocVector(VECSXP, nblocks));
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
			PROTECT(members = Rf_allocVector(INTSXP, blocksize));
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
	PROTECT(which = Rf_allocVector(INTSXP, npixels));
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
	int ncol = Rf_ncols(coord);
	int n = Rf_nrows(coord);
	T * pCoord = DataPtr<T>(coord);
	int * ii = INTEGER(neighbors);
	SEXP offsets;
	PROTECT(offsets = Rf_allocMatrix(DataType<T>(), nrow, ncol));
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
	int npixels = Rf_nrows(offsets);
	int ndims = Rf_ncols(offsets);
	SEXP w, alpha, beta;
	PROTECT(w = Rf_allocVector(VECSXP, 2));
	PROTECT(alpha = Rf_allocVector(REALSXP, npixels));
	PROTECT(beta = Rf_allocVector(REALSXP, npixels));
	double * pAlpha = REAL(alpha);
	double * pBeta = REAL(beta);
	T2 * pOffsets = DataPtr<T2>(offsets);
	int k = 0; // center pixel
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
		int nfeatures = Rf_nrows(x);
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
SEXP get_spatial_distance(SEXP x, SEXP ref, SEXP offsets, SEXP ref_offsets,
	SEXP weights, SEXP ref_weights, SEXP neighbors, double tol_dist)
{
	int ndims = Rf_ncols(ref_offsets);
	int nfeatures = Rf_nrows(x);
	int npixels = LENGTH(neighbors);
	T1 * pX = DataPtr<T1>(x);
	T1 * pRef = DataPtr<T1>(ref);
	SEXP dist;
	PROTECT(dist = Rf_allocVector(REALSXP, npixels));
	double * pDist = REAL(dist);
	for ( int i = 0; i < npixels; ++i ) {
		SEXP nb = VECTOR_ELT(neighbors, i);
		int * pNb = INTEGER(nb);
		SEXP wt = VECTOR_ELT(weights, i);
		double * alpha = REAL(VECTOR_ELT(wt, 0));
		double * beta = REAL(VECTOR_ELT(wt, 1));
		double * ref_alpha = REAL(VECTOR_ELT(ref_weights, 0));
		double * ref_beta = REAL(VECTOR_ELT(ref_weights, 1));
		T2 * pOffsets = DataPtr<T2>(VECTOR_ELT(offsets, i));
		T2 * pRef_offsets = DataPtr<T2>(ref_offsets);
		int nx = Rf_nrows(VECTOR_ELT(offsets, i));
		int ny = Rf_nrows(ref_offsets);
		double d1, d2, alpha_i, beta_i, a_i, dist1, dist2 = 0;
		for ( int ix = 0; ix < nx; ++ix ) {
			int ii = pNb[ix] - 1;
			for ( int iy = 0; iy < ny; ++iy ) {
				d2 = 0;
				for ( int k = 0; k < ndims; ++k ) {
					d1 = pOffsets[k * nx + ix] - pRef_offsets[k * ny + iy];
					d2 += d1 * d1;
				}
				if ( d2 < tol_dist ) {
					alpha_i = alpha[ix] * ref_alpha[iy];
					beta_i = beta[ix] * ref_beta[iy];
					a_i = sqrt(alpha_i * beta_i);
					for ( int j = 0; j < nfeatures; ++j ) {
						dist1 = pX[ii * nfeatures + j] - pRef[iy * nfeatures + j];
						dist2 += a_i * dist1 * dist1;
					}
				}
			}
		}
		pDist[i] = sqrt(dist2);
	}
	UNPROTECT(1);
	return dist;
}

template<typename T1, typename T2>
SEXP get_spatial_scores(SEXP x, SEXP ref, SEXP weights, SEXP sd)
{
	int nfeatures = Rf_nrows(x);
	int npixels = Rf_ncols(x);
	int nrefs = Rf_ncols(ref);
	double * alpha = REAL(VECTOR_ELT(weights, 0));
	double * beta = REAL(VECTOR_ELT(weights, 1));
	double * sdev = REAL(sd);
	T1 * pX = DataPtr<T1>(x);
	T2 * pRef = DataPtr<T2>(ref);
	SEXP scores;
	PROTECT(scores = Rf_allocVector(REALSXP, nrefs));
	double * pScores = REAL(scores);
	double a_i, dist, auc = 0;
	for ( int i = 0; i < npixels; ++i )
		auc += alpha[i] * beta[i];
	for ( int k = 0; k < nrefs; ++k ) {
		pScores[k] = 0;
		for ( int i = 0; i < npixels; ++i ) {
			a_i = alpha[i] * beta[i] / auc;
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

template<typename T>
SEXP get_spatial_filter(SEXP x, SEXP weights, SEXP neighbors)
{
	int nr = Rf_nrows(x);
	int nc = Rf_ncols(x);
	T * pX = DataPtr<T>(x);
	SEXP nb, wt, y;
	PROTECT(y = Rf_allocMatrix(REALSXP, nr, nc));
	double * pY = REAL(y);
	double a_k, auc;
	for ( int i = 0; i < nr; ++i ) {
		wt = VECTOR_ELT(weights, i);
		double * alpha = REAL(VECTOR_ELT(wt, 0));
		double * beta = REAL(VECTOR_ELT(wt, 1));
		nb = VECTOR_ELT(neighbors, i);
		int K = LENGTH(nb);
		int * ii = INTEGER(nb);
		auc = 0;
		for ( int k = 0; k < K; ++k )
			auc += alpha[k] * beta[k];
		for ( int j = 0; j < nc; ++j )
			pY[j * nr + i] = 0;
		for ( int k = 0; k < K; ++k ) {
			a_k = alpha[k] * beta[k] / auc;
			for ( int j = 0; j < nc; ++j )
				pY[j * nr + i] += a_k * pX[j * nr + (ii[k] - 1)];
		}
	}
	UNPROTECT(1);
	return y;
}

extern "C" {

	SEXP findNeighbors(SEXP coord, SEXP r, SEXP group, SEXP dist) {
		if ( TYPEOF(coord) == INTSXP )
			return find_neighbors<int>(coord, r, group, dist);
		else if ( TYPEOF(coord) == REALSXP )
			return find_neighbors<double>(coord, r, group, dist);
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
			return get_spatial_offsets<int>(coord, neighbors, Rf_asInteger(k));
		else if ( TYPEOF(coord) == REALSXP )
			return get_spatial_offsets<double>(coord, neighbors, Rf_asInteger(k));
		else
			return R_NilValue;
	}

	SEXP spatialWeights(SEXP x, SEXP offsets, SEXP sigma, SEXP bilateral) {
		if ( TYPEOF(x) == INTSXP && TYPEOF(offsets) == INTSXP )
			return get_spatial_weights<int,int>(x, offsets, Rf_asReal(sigma), Rf_asLogical(bilateral));
		else if ( TYPEOF(x) == INTSXP && TYPEOF(offsets) == REALSXP )
			return get_spatial_weights<int,double>(x, offsets, Rf_asReal(sigma), Rf_asLogical(bilateral));
		else if ( TYPEOF(x) == REALSXP && TYPEOF(offsets) == INTSXP )
			return get_spatial_weights<double,int>(x, offsets, Rf_asReal(sigma), Rf_asLogical(bilateral));
		else if ( TYPEOF(x) == REALSXP && TYPEOF(offsets) == REALSXP )
			return get_spatial_weights<double,double>(x, offsets, Rf_asReal(sigma), Rf_asLogical(bilateral));
		else
			return R_NilValue;
	}

	SEXP spatialDistance(SEXP x, SEXP ref, SEXP offsets, SEXP ref_offsets,
		SEXP weights, SEXP ref_weights, SEXP neighbors, SEXP tol_dist)
	{
		if ( TYPEOF(x) == INTSXP && TYPEOF(ref_offsets) == INTSXP )
			return get_spatial_distance<int,int>(x, ref, offsets, ref_offsets, weights, ref_weights, neighbors, Rf_asReal(tol_dist));
		else if ( TYPEOF(x) == INTSXP && TYPEOF(ref_offsets) == REALSXP )
			return get_spatial_distance<int,double>(x, ref, offsets, ref_offsets, weights, ref_weights, neighbors, Rf_asReal(tol_dist));
		else if ( TYPEOF(x) == REALSXP && TYPEOF(ref_offsets) == INTSXP )
			return get_spatial_distance<double,int>(x, ref, offsets, ref_offsets, weights, ref_weights, neighbors, Rf_asReal(tol_dist));
		else if ( TYPEOF(x) == REALSXP && TYPEOF(ref_offsets) == REALSXP )
			return get_spatial_distance<double,double>(x, ref, offsets, ref_offsets, weights, ref_weights, neighbors, Rf_asReal(tol_dist));
		else
			return R_NilValue;
	}

	SEXP spatialScores(SEXP x, SEXP ref, SEXP weights, SEXP sd) {
		if ( TYPEOF(x) == INTSXP && TYPEOF(ref) == INTSXP )
			return get_spatial_scores<int,int>(x, ref, weights, sd);
		else if ( TYPEOF(x) == INTSXP && TYPEOF(ref) == REALSXP )
			return get_spatial_scores<int,double>(x, ref, weights, sd);
		else if ( TYPEOF(x) == REALSXP && TYPEOF(ref) == INTSXP )
			return get_spatial_scores<double,int>(x, ref, weights, sd);
		else if ( TYPEOF(x) == REALSXP && TYPEOF(ref) == REALSXP )
			return get_spatial_scores<double,double>(x, ref, weights, sd);
		else
			return R_NilValue;
	}

	SEXP spatialFilter(SEXP x, SEXP weights, SEXP neighbors) {
		if ( TYPEOF(x) == INTSXP )
			return get_spatial_filter<int>(x, weights, neighbors);
		else if ( TYPEOF(x) == REALSXP )
			return get_spatial_filter<double>(x, weights, neighbors);
		else
			return R_NilValue;
	}

}

