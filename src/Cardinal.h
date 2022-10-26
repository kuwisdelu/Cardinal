
#ifndef CARDINAL
#define CARDINAL

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include "utils.h"

extern "C" {

    // DIP

    SEXP gaussianFilter(SEXP x, SEXP r, SEXP sd);

    SEXP bilateralFilter(SEXP x, SEXP r, SEXP sd);

    // imzML

    SEXP readImzML(SEXP filepath);

    SEXP writeImzML(SEXP metadata, SEXP tmpl, SEXP filepath);

    // spatial

    SEXP findNeighbors(SEXP coord, SEXP r, SEXP group, SEXP dist);

    SEXP findSpatialBlocks(SEXP coord, SEXP r, SEXP group, SEXP block_info);

    SEXP whichSpatialBlocks(SEXP neighbors, SEXP blocks);

    SEXP spatialOffsets(SEXP coord, SEXP neighbors, SEXP k);

    SEXP spatialWeights(SEXP x, SEXP offsets, SEXP sigma, SEXP bilateral);

    SEXP spatialDistance(SEXP x, SEXP ref, SEXP offsets, SEXP ref_offsets,
           SEXP weights, SEXP ref_weights, SEXP neighbors, SEXP tol_dist);

    SEXP spatialScores(SEXP x, SEXP centers, SEXP weights,
                        SEXP neighbors, SEXP sd);

    SEXP spatialFilter(SEXP x, SEXP neighbors, SEXP weights);

}

#endif
