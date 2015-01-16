
#include <R.h>

void dynamicAlign(double * score_mat, int * tracking_mat, double * similarity_mat,
	int * nrow, int * ncol, double * gap, int * xmatch, int * ymatch)
{
	// 0 = above, 1 = left, 2 = diagonal
	double direction_score[] = {0, 1, 2};
	// fill the matrix
	for (int j = 1; j < *ncol; ++j)
	{
		for (int i = 1; i < *nrow; ++i)
		{
			direction_score[0] = score_mat[(*nrow * j) + i -1] + *gap; // above
			direction_score[1] = score_mat[(*nrow * (j - 1)) + i] + *gap; // left
			direction_score[2] = score_mat[(*nrow * (j - 1)) + i - 1] +
				similarity_mat[((*nrow - 1) * (j - 1)) + i - 1]; // diagonal
			if ( (direction_score[0] >= direction_score[1]) &&
				(direction_score[0] >= direction_score[2]) )
			{
				score_mat[(*nrow * j) + i] = direction_score[0];
				tracking_mat[(*nrow * j) + i] = 0;
			}
			if ( (direction_score[1] >= direction_score[0]) &&
				(direction_score[1] >= direction_score[2]) )
			{
				score_mat[(*nrow * j) + i] = direction_score[1];
				tracking_mat[(*nrow * j) + i] = 1;
			}
			if ( (direction_score[2] >= direction_score[0]) &&
				(direction_score[2] >= direction_score[1]) )
			{
				score_mat[(*nrow * j) + i] = direction_score[2];
				tracking_mat[(*nrow * j) + i] = 2;
			}
		}
	}
	// traceback
	int i = *nrow - 1;
	int j = *ncol - 1;
	while ( i > 0 && j > 0 ) {
		if ( tracking_mat[(*nrow * j) + i] == 0) {
			i = i - 1;
		}
		if ( tracking_mat[(*nrow * j) + i] == 1) {
			j = j - 1;
		}
		if ( tracking_mat[(*nrow * j) + i] == 2) {
			xmatch[j - 1] = i;
			ymatch[i - 1] = j;
			i = i - 1;
			j = j - 1;
		}
	}
}
