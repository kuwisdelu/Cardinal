
#include <R.h>
#include <Rdefines.h>

#include <stdio.h>
#include <stdlib.h>

#include "utils.h"

// defines copied from Analyze 7.5 documentation

#define DT_NONE					0
#define DT_UNKNOWN				0
#define DT_BINARY				1
#define DT_UNSIGNED_CHAR		2
#define DT_SIGNED_SHORT			4
#define DT_SIGNED_INT			8
#define DT_FLOAT				16
#define DT_COMPLEX				32
#define DT_DOUBLE				64
#define DT_RGB					128
#define DT_ALL					255

// structs copied from Analyze 7.5 documentation

struct header_key
{	
	int sizeof_hdr;  			/* 0 + 4   */ 
	char data_type[10];  		/* 4 + 10  */ 
	char db_name[18];  			/* 14 + 18 */ 
	int extents;   				/* 32 + 4  */ 
	short int session_error;  	/* 36 + 2  */ 
	char regular;    			/* 38 + 1  */ 
	char hkey_un0;   			/* 39 + 1  */
}; // total 40 bytes

struct image_dimension
{
	unsigned short int dim[8];                 /* 0 + 16           */ 
	unsigned short int unused8;                /* 16 + 2           */ 
	unsigned short int unused9;                /* 18 + 2           */ 
	unsigned short int unused10;               /* 20 + 2           */ 
	unsigned short int unused11;               /* 22 + 2           */ 
	unsigned short int unused12;               /* 24 + 2           */ 
	unsigned short int unused13;               /* 26 + 2           */ 
	unsigned short int unused14;               /* 28 + 2           */ 
	unsigned short int datatype;               /* 30 + 2           */ 
	unsigned short int bitpix;                 /* 32 + 2           */ 
	unsigned short int dim_un0;                /* 34 + 2           */ 
	float pixdim[8];                  /* 36 + 32          */
	float vox_offset;                 /* 68 + 4           */ 
	float funused1;                   /* 72 + 4           */ 
	float funused2;                   /* 76 + 4           */ 
	float funused3;                   /* 80 + 4           */ 
	float cal_max;                    /* 84 + 4           */ 
	float cal_min;                    /* 88 + 4           */ 
	float compressed;                 /* 92 + 4           */ 
	float verified;                   /* 96 + 4           */ 
	int glmax, glmin;                 /* 100 + 8          */
}; // total 108 bytes

struct data_history
{
	char descrip[80];                 /* 0 + 80           */ 
	char aux_file[24];                /* 80 + 24          */ 
	char orient;                      /* 104 + 1          */ 
	char originator[10];              /* 105 + 10         */ 
	char generated[10];               /* 115 + 10         */ 
	char scannum[10];				  /* 125 + 10         */ 
	char patient_id[10];              /* 135 + 10         */ 
	char exp_date[10];				  /* 145 + 10         */
	char exp_time[10];				  /* 155 + 10         */ 
	char hist_un0[3];				  /* 165 + 3          */ 
	int views;                        /* 168 + 4          */ 
	int vols_added;                   /* 172 + 4          */ 
	int start_field;                  /* 176 + 4          */ 
	int field_skip;                   /* 180 + 4          */ 
	int omax, omin;                   /* 184 + 8          */ 
	int smax, smin;                   /* 192 + 8          */
}; // total 200 bytes

struct dsr 
{  
	struct header_key hk;             /* 0 + 40            */ 
	struct image_dimension dime;      /* 40 + 108          */ 
	struct data_history hist;         /* 148 + 200         */ 
};     /* total= 348 bytes */

void swap_hdr(struct dsr * pheader)
{
	SWAP(pheader->hk.sizeof_hdr);
	SWAP(pheader->hk.extents);
	SWAP(pheader->hk.session_error);
	SWAP(pheader->dime.dim[0]);
	SWAP(pheader->dime.dim[1]);
	SWAP(pheader->dime.dim[2]);
	SWAP(pheader->dime.dim[3]);
	SWAP(pheader->dime.dim[4]);
	SWAP(pheader->dime.dim[5]);
	SWAP(pheader->dime.dim[6]);
	SWAP(pheader->dime.dim[7]);
	SWAP(pheader->dime.unused8);
	SWAP(pheader->dime.unused9);
	SWAP(pheader->dime.unused10);
	SWAP(pheader->dime.unused11);
	SWAP(pheader->dime.unused12);
	SWAP(pheader->dime.unused13);
	SWAP(pheader->dime.unused14);
	SWAP(pheader->dime.datatype);
	SWAP(pheader->dime.bitpix);
	SWAP(pheader->dime.dim_un0);
	SWAP(pheader->dime.pixdim[0]);
	SWAP(pheader->dime.pixdim[1]);
	SWAP(pheader->dime.pixdim[2]);
	SWAP(pheader->dime.pixdim[3]);
	SWAP(pheader->dime.pixdim[4]);
	SWAP(pheader->dime.pixdim[5]);
	SWAP(pheader->dime.pixdim[6]);
	SWAP(pheader->dime.pixdim[7]);
	SWAP(pheader->dime.vox_offset);
	SWAP(pheader->dime.funused1);
	SWAP(pheader->dime.funused2);
	SWAP(pheader->dime.funused3);
	SWAP(pheader->dime.cal_max);
	SWAP(pheader->dime.cal_min);
	SWAP(pheader->dime.compressed);
	SWAP(pheader->dime.verified);
	SWAP(pheader->dime.glmax);
	SWAP(pheader->dime.glmin);
};

template<typename CType, typename RType>
SEXP readSimpleIntensityArray(const char * filename, int nrow, int ncol)
{
	FILE * pfile = fopen(filename, "rb");
	if ( pfile == NULL ) return R_NilValue;
	SEXP data;
	PROTECT(data = allocMatrix(DataType<RType>(), nrow, ncol));
	RType * pdata = DataPtr<RType>(data);
	CType tmp[nrow];
	for ( int j = 0; j < ncol; j++ )
	{
		fread(tmp, sizeof(CType), nrow, pfile);
		for ( int i = 0; i < nrow; i++ )
		{
			pdata[(nrow * j) + i] = (RType) tmp[i];
		}
	}
	fclose(pfile);
	UNPROTECT(1);
	return data;
}

extern "C"
{

	SEXP readAnalyzeHDR(SEXP filepath)
	{
		// read header file
		const char * filename = CHAR(STRING_ELT(filepath, 0));
		FILE * pfile = fopen(filename, "rb");
		if ( pfile == NULL ) return R_NilValue;
		struct dsr header;
		fread(&header, sizeof(struct dsr), 1, pfile);
		fclose(pfile);

		// test endianness
		SEXP endian;
		PROTECT(endian = NEW_STRING(1));
		if ( header.hk.sizeof_hdr == 348L )
		{
			SET_STRING_ELT(endian, 0, mkChar("native"));
		} else {
			SET_STRING_ELT(endian, 0, mkChar("swap"));
			swap_hdr(&header);
		}

		// assign header_key elements
		SEXP hk, hk_names;
		PROTECT(hk = NEW_LIST(3));
		PROTECT(hk_names = NEW_STRING(3));
		SET_VECTOR_ELT(hk, 0, ScalarInteger(header.hk.sizeof_hdr));
		SET_STRING_ELT(hk_names, 0, mkChar("sizeof_hdr"));
		SET_VECTOR_ELT(hk, 1, ScalarInteger(header.hk.extents));
		SET_STRING_ELT(hk_names, 1, mkChar("extents"));
		SET_VECTOR_ELT(hk, 2, ScalarRaw(header.hk.regular));
		SET_STRING_ELT(hk_names, 2, mkChar("regular"));
		setAttrib(hk, R_NamesSymbol, hk_names);

		// prepare dim and pixdim for image_dimension
		SEXP dim, pixdim;
		PROTECT(dim = NEW_INTEGER(8));
		PROTECT(pixdim = NEW_NUMERIC(8));
		for ( int i = 0; i < 8; i++ ) {
			INTEGER(dim)[i] = header.dime.dim[i];
			REAL(pixdim)[i] = header.dime.pixdim[i];
		}

		// assign image_dimension elements
		SEXP dime, dime_names;
		PROTECT(dime = NEW_LIST(11));
		PROTECT(dime_names = NEW_STRING(11));
		SET_VECTOR_ELT(dime, 0, dim);
		SET_STRING_ELT(dime_names, 0, mkChar("dim"));
		SET_VECTOR_ELT(dime, 1, ScalarInteger(header.dime.datatype));
		SET_STRING_ELT(dime_names, 1, mkChar("datatype"));
		SET_VECTOR_ELT(dime, 2, ScalarInteger(header.dime.bitpix));
		SET_STRING_ELT(dime_names, 2, mkChar("bitpix"));
		SET_VECTOR_ELT(dime, 3, pixdim);
		SET_STRING_ELT(dime_names, 3, mkChar("pixdim"));
		SET_VECTOR_ELT(dime, 4, ScalarReal(header.dime.vox_offset));
		SET_STRING_ELT(dime_names, 4, mkChar("vox_offset"));
		SET_VECTOR_ELT(dime, 5, ScalarReal(header.dime.cal_max));
		SET_STRING_ELT(dime_names, 5, mkChar("cal_max"));
		SET_VECTOR_ELT(dime, 6, ScalarReal(header.dime.cal_min));
		SET_STRING_ELT(dime_names, 6, mkChar("cal_min"));
		SET_VECTOR_ELT(dime, 7, ScalarReal(header.dime.compressed));
		SET_STRING_ELT(dime_names, 7, mkChar("compressed"));
		SET_VECTOR_ELT(dime, 8, ScalarReal(header.dime.verified));
		SET_STRING_ELT(dime_names, 8, mkChar("verified"));
		SET_VECTOR_ELT(dime, 9, ScalarReal(header.dime.glmax));
		SET_STRING_ELT(dime_names, 9, mkChar("glmax"));
		SET_VECTOR_ELT(dime, 10, ScalarReal(header.dime.glmin));
		SET_STRING_ELT(dime_names, 10, mkChar("glmin"));
		setAttrib(dime, R_NamesSymbol, dime_names);
		
		// assign data_history elements
		SEXP hist, hist_names;
		PROTECT(hist = NEW_LIST(1));
		PROTECT(hist_names = NEW_STRING(1));
		SET_VECTOR_ELT(hist, 0, ScalarRaw(header.hist.orient));
		SET_STRING_ELT(hist_names, 0, mkChar("orient"));
		setAttrib(hist, R_NamesSymbol, hist_names);

		// assign substructures into single list
		SEXP outlist, outlist_names;
		PROTECT(outlist = NEW_LIST(3));
		PROTECT(outlist_names = NEW_STRING(3));
		SET_VECTOR_ELT(outlist, 0, hk);
		SET_STRING_ELT(outlist_names, 0, mkChar("hk"));
		SET_VECTOR_ELT(outlist, 1, dime);
		SET_STRING_ELT(outlist_names, 1, mkChar("dime"));
		SET_VECTOR_ELT(outlist, 2, hist);
		SET_STRING_ELT(outlist_names, 2, mkChar("hist"));
		setAttrib(outlist, R_NamesSymbol, outlist_names);
		setAttrib(outlist, install("endian"), endian);

		// clean up and return
		UNPROTECT(11);
		return outlist;
	}

	SEXP readAnalyzeT2M(SEXP filepath, SEXP n)
	{
		const char * filename = CHAR(STRING_ELT(filepath, 0));
		FILE * pfile = fopen(filename, "rb");
		if ( pfile == NULL ) return R_NilValue;
		SEXP outmz;
		PROTECT(outmz = NEW_NUMERIC(INTEGER(n)[0]));
		float mz[INTEGER(n)[0]];
		fread(mz, sizeof(float), INTEGER(n)[0], pfile);
		fclose(pfile);
		for ( int i = 0; i < INTEGER(n)[0]; i++ ) REAL(outmz)[i] = (double) mz[i];
		UNPROTECT(1);
		return outmz;
	}

	SEXP readAnalyzeIMG(SEXP filepath, SEXP dim, SEXP datatype)
	{
		const char * filename = CHAR(STRING_ELT(filepath, 0));
		int type = INTEGER(datatype)[0];
		int nrow = INTEGER(dim)[0];
		int ncol = INTEGER(dim)[1];
		if ( type == DT_UNSIGNED_CHAR )
		{
			return readSimpleIntensityArray<char, int>(filename, nrow, ncol);
		}
		else if ( type == DT_SIGNED_SHORT )
		{
			return readSimpleIntensityArray<short, int>(filename, nrow, ncol);
		}
		else if ( type == DT_SIGNED_INT )
		{
			return readSimpleIntensityArray<int, int>(filename, nrow, ncol);
		}
		else if ( type == DT_FLOAT )
		{
			return readSimpleIntensityArray<float, double>(filename, nrow, ncol);
		}
		else if ( type == DT_DOUBLE )
		{
			return readSimpleIntensityArray<double, double>(filename, nrow, ncol);
		}
		else return R_NilValue;
	}

}
