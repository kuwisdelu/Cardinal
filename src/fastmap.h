
#ifndef FASTMAP
#define FASTMAP

struct matrix {
	double * data;
	int nrow;
	int ncol;
};

struct pivot_array {
	int * a;
	int * b;
	int ncol;
};

// standard euclidean distance between 'i' and 'j' (not squared distance)
double dist_euclidean(const void * objects, int i, int j);

// fastmap distance between objects 'i' and 'j' given fastmap components
// in 'x' and a distance measure given by the specified function 'dist'
double dist_fastmap(const void * objects, const struct matrix * x_new,
	int i, int j, double (*dist) (const void *, int, int));

// selects distant objects for the fastmap pivot array at column 'col'
// using 'niter' iterations with the specified function 'dist' and
// inserts their object ids into the given array 'pivot_objects'
void choose_distant_objects(const void * objects, const struct matrix * x_new,
	struct pivot_array * pivot_objects, int nobjects, int col,
	int niter, double (*dist) (const void *, int, int));

// projects 'objects' to 'ncomp' number of fastmap components in matrix 'x'
// beginning at the 'nstart'-th component (usually 'nstart'=1, else assumes
// the 'nstart' - 1 components are already calculated), using 'niter' iterations
// to find pivot objects, where if distance between pivot objects are
// less than 'tol' then all subsequent fastmap components are set to 0
void fastmap(const void * objects, struct matrix * x_new,
	struct pivot_array * pivot_objects, int nobjects, int ncomp, int nstart, int niter,
	double tol, double (*dist) (const void *, int, int));

// fastmap for euclidean 'input' where 'input' has 'nrow' rows and 'ncol' columns
// where the rows are the objects, and 'output' are the 'ncomp' fastmap components
// and the object ids of the pivot objects are output into 'pivot_array'
void fastmap_euclidean(double * input, double * output, int * pivot_array,
	int * nrow, int * ncol, int * ncomp);

#endif
