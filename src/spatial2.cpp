
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
SEXP find_spatial_blocks(SEXP coord, SEXP r, SEXP groups, SEXP binfo)
{
	int ngroups = LENGTH(binfo);
	int nrow = nrows(coord);
	int ncol = ncols(coord);
	T * pCoord = DataPtr<T>(coord);
	double * pR = REAL(r);
	int * pGroups = INTEGER(groups);
	bool within_block[nrow];
	SEXP ret;
	PROTECT(ret = NEW_LIST(ngroups));
	for ( int k = 0; k < ngroups; ++k ) {
		SEXP info = VECTOR_ELT(binfo, k);
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

extern "C" {

	SEXP findNeighbors(SEXP coord, SEXP r, SEXP group) {
		if ( TYPEOF(coord) == INTSXP )
			return find_neighbors<int>(coord, r, group);
		else if ( TYPEOF(coord) == REALSXP )
			return find_neighbors<double>(coord, r, group);
		else
			return R_NilValue;
	}

	SEXP findSpatialBlocks(SEXP coord, SEXP r, SEXP group, SEXP binfo) {
		if ( TYPEOF(coord) == INTSXP )
			return find_spatial_blocks<int>(coord, r, group, binfo);
		else if ( TYPEOF(coord) == REALSXP )
			return find_spatial_blocks<double>(coord, r, group, binfo);
		else
			return R_NilValue;
	}

}

