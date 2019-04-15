
#include "Cardinal.h"

template<typename T>
SEXP dyn_align(SEXP similarity, SEXP score, SEXP tracking, SEXP gap)
{
	// gap penalty
	double gap_penalty = Rf_asReal(gap);
	// matrix pointers
	T * pSimilarity = DataPtr<T>(similarity);
	int nrow = Rf_nrows(score);
	int ncol = Rf_ncols(score);
	double * pScore = REAL(score);
	int * pTracking = INTEGER(tracking);
	SEXP ret, x, y;
	PROTECT(ret = Rf_allocVector(VECSXP, 2));
	PROTECT(x = Rf_allocVector(INTSXP, nrow - 1));
	PROTECT(y = Rf_allocVector(INTSXP, ncol - 1));
	SET_VECTOR_ELT(ret, 0, x);
	SET_VECTOR_ELT(ret, 1, y);
	UNPROTECT(2);
	int * pX = INTEGER(x);
	int * pY = INTEGER(y);
	// 0 = above, 1 = left, 2 = diagonal
	double direction_score[] = {0, 1, 2};
	// fill the matrix
	for (int j = 1; j < ncol; ++j)
	{
		for (int i = 1; i < nrow; ++i)
		{
			direction_score[0] = pScore[(nrow * j) + i -1] + gap_penalty; // above
			direction_score[1] = pScore[(nrow * (j - 1)) + i] + gap_penalty; // left
			direction_score[2] = pScore[(nrow * (j - 1)) + i - 1] +
				pSimilarity[((nrow - 1) * (j - 1)) + i - 1]; // diagonal
			if ( (direction_score[0] >= direction_score[1]) &&
				(direction_score[0] >= direction_score[2]) )
			{
				pScore[(nrow * j) + i] = direction_score[0];
				pTracking[(nrow * j) + i] = 0;
			}
			if ( (direction_score[1] >= direction_score[0]) &&
				(direction_score[1] >= direction_score[2]) )
			{
				pScore[(nrow * j) + i] = direction_score[1];
				pTracking[(nrow * j) + i] = 1;
			}
			if ( (direction_score[2] >= direction_score[0]) &&
				(direction_score[2] >= direction_score[1]) )
			{
				pScore[(nrow * j) + i] = direction_score[2];
				pTracking[(nrow * j) + i] = 2;
			}
		}
	}
	for ( int i = 0; i < LENGTH(x); ++i ) pX[i] = 0;
	for ( int j = 0; j < LENGTH(y); ++j ) pY[j] = 0;
	// traceback
	int i = nrow - 1;
	int j = ncol - 1;
	while ( i > 0 && j > 0 ) {
		if ( pTracking[(nrow * j) + i] == 0) {
			i = i - 1;
		}
		if ( pTracking[(nrow * j) + i] == 1) {
			j = j - 1;
		}
		if ( pTracking[(nrow * j) + i] == 2) {
			pY[j - 1] = i;
			pX[i - 1] = j;
			i = i - 1;
			j = j - 1;
		}
	}
	UNPROTECT(1);
	return ret;
}


extern "C" {

	SEXP dynAlign(SEXP similarity, SEXP score, SEXP tracking, SEXP gap)
	{
		switch(TYPEOF(similarity)) {
			case INTSXP:
				return dyn_align<int>(similarity, score, tracking, gap);
			case REALSXP:
				return dyn_align<double>(similarity, score, tracking, gap);
		}
		return R_NilValue;
	}

}


