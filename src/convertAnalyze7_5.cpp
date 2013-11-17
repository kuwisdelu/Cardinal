
#include <R.h>

#include <iostream>
#include <string>
#include <cstring>
#include <fstream>
#include <stdlib.h>

using namespace std;

// all these structs copied from Analyze 7.5 documentation
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
	short int dim[8];                 /* 0 + 16           */ 
	short int unused8;                /* 16 + 2           */ 
	short int unused9;                /* 18 + 2           */ 
	short int unused10;               /* 20 + 2           */ 
	short int unused11;               /* 22 + 2           */ 
	short int unused12;               /* 24 + 2           */ 
	short int unused13;               /* 26 + 2           */ 
	short int unused14;               /* 28 + 2           */ 
	short int datatype;               /* 30 + 2           */ 
	short int bitpix;                 /* 32 + 2           */ 
	short int dim_un0;                /* 34 + 2           */ 
	float pixdim[8];                  /* 36 + 32          */
	float vox_offset;                 /* 68 + 4           */ 
	float funused1;                   /* 72 + 4           */ 
	float funused2;                   /* 76 + 4           */ 
	float funused3;                   /* 80 + 4           */ 
	float cal_max;                    /* 84 + 4           */ 
	float cal_min;                    /* 88 + 4           */ 
	float compressed;                 /* 92 + 4           */ 
	float verified;                   /* 96 + 4           */ 
	int glmax,glmin;                  /* 100 + 8          */
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

struct DSR 
{  
	header_key hk;             /* 0 + 40            */ 
	image_dimension dime;      /* 40 + 108          */ 
	data_history hist;         /* 148 + 200         */ 
};     /* total= 348 bytes */

// reads header file into a C struct
void ReadHeader(char* header_file,DSR &header_info) 
{
	fstream filestr(header_file, fstream::in | fstream::out | fstream::binary);
	filestr.read(reinterpret_cast<char*>(&header_info),sizeof(header_info));
	filestr.close();
}

// reads T2M file into a set of mz_values (single precision floats)
bool ReadT2M(char* t2m_file,float* mz_values,long int n_mz) 
{
	fstream filestr(t2m_file, fstream::in | fstream::out | fstream::binary);
	filestr.read(reinterpret_cast<char*>(mz_values),n_mz*sizeof(float));
	char u;
	filestr.read(&u,1);
	bool x = filestr.eof();
	filestr.close();
	return x;
}

// reads a block of the image file corresponding to a spectrum for a single pixel, and advances file pointer
bool ReadIMG(fstream* IMG_FILE,short int* intensities,long int n_intensities)
{
	IMG_FILE->read(reinterpret_cast<char*>(intensities),n_intensities*sizeof(short int));
	return IMG_FILE->eof();
}

extern "C" {
	void read_MALDI_hdr(int * n_mz, int * pixel_x, int * pixel_y, int * pixdim_x,
		int * pixdim_y, char ** argv)
	{
		DSR header_info;
		// read in arguments
		char header_file[100];
		strcpy(header_file, argv[0]);
		strcat(header_file,".hdr");
		ReadHeader(header_file,header_info);
		//write key data from header file into parameters
		n_mz[0] = (unsigned short)header_info.dime.dim[1];
		pixel_x[0] = header_info.dime.dim[2];
		pixel_y[0] = header_info.dime.dim[3];
		pixdim_x[0] = header_info.dime.pixdim[1];
		pixdim_y[0] = header_info.dime.pixdim[1];
	}

	void read_MALDI_t2m(int * n_mz, double * mz_values, char ** argv) {
		// isolate the R-passed parameters from ReadT2M parameters
		long int n_mz_dummy = (long) *n_mz;
		float * mz_values_dummy = new float[n_mz_dummy];
		char t2m_file[100];
		// get the m/z-values
		strcpy(t2m_file, argv[0]);
		strcat(t2m_file,".t2m");
		if ( !ReadT2M(t2m_file, mz_values_dummy, n_mz_dummy) ) {
			Rprintf("T2M file is wrong size!\n");
		}
		// pass m/z-values back to R
		for(int i=0; i < n_mz_dummy; ++i) {
			mz_values[i] = (double)mz_values_dummy[i];
		}
		free(mz_values_dummy);
	}

	void read_MALDI_img(int * n_mz, int * pixel_x, int * pixel_y, int * intensities,
		char ** argv)
	{
		// isolate the R-passed parameters from ReadIMG parameters
		long int n_mz_dummy = (long) *n_mz;
		long int intensities_length = (long)(n_mz_dummy * (*pixel_x) * (*pixel_y));
		short int * intensities_dummy = new short[intensities_length];
		short int * intensities_dummy_head = intensities_dummy;
		// get the intensities from the image file
		char base_file[100], image_file[100];
		strcpy(base_file, argv[0]);
		strcpy(image_file, argv[0]);
		strcat(image_file,".img");
		fstream image_stream(image_file, fstream::in | fstream::out | fstream::binary); 
		fstream * IMG_FILE = &image_stream;
		for ( int j=0; j < *pixel_y; ++j )
		{
			for ( int i=0; i < *pixel_x; ++i )
			{
				if ( ReadIMG(IMG_FILE, intensities_dummy, n_mz_dummy) ) {
					Rprintf("Something wrong with IMG file!\n");
				}
				intensities_dummy = intensities_dummy + (*n_mz);
			}
		}
		// check that we got everything
		char u;
		IMG_FILE->read(&u,1);
		if ( !IMG_FILE->eof() ) {
			Rprintf("Didn't read the whole file\n");
		}
		IMG_FILE->close();
		// pass intensities back to R
		for(long int i=0; i < intensities_length; ++i) {
			intensities[i] = (int)intensities_dummy_head[i];
		}
		free(intensities_dummy_head);
	}
}





