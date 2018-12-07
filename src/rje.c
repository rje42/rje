#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdbool.h>
#include <math.h>

void indexBox_c (int *upp, int *lwr, int *dim, int *len, int *out) {
  int i, j, k;
  int prod = 1;
  int totlen = 1;
  out[0] = 0;
  int offset = 0;

  for (i = 0; i < *len; i++) {
    offset += prod*lwr[i];
    for (j = 0; j < upp[i] - lwr[i] + 1; j++) {
      for (k = 0; k < totlen; k++) {
	out[k + totlen*j] = out[k] + prod*j;
      }
    }
    prod *= dim[i];
    totlen *= (upp[i] - lwr[i] + 1);
  }
  for (k = 0; k < totlen; k++) out[k] += offset;
}

void doone (double *x, int *dim, int k, int rmv) {
  int i1, i2, i3;
  int before = 1, before2, after = 1;
  long double y = 0.0;

  for (i1=0; i1 < rmv-1; i1++) {
    before *= dim[i1];
  }
  before2 = before*dim[rmv-1];
  for (i1=rmv; i1 < k; i1++) {
    after *= dim[i1];
  }

  for (i3 = 0; i3 < after; i3++) {
    for (i1 = 0; i1 < before; i1++) {
      y = 0.0;
      for (i2 = 0; i2 < dim[rmv-1]; i2++) {
        y += x[i1+before*i2+before2*i3];
      }
      x[i1+before*i3] = y;
    }
  }

  return;
}

/* Marginalize an array

  double *x    - array of doubles to be marginalized
  int    *dim  - vector of integers giving dimension of x
  int    *k    - length of dim
  int    *rmv  - vector of integers giving dimensions to be marginalized
  int    *nrmv - length of rmv
 
 */
void marginTable_c (double *x, int *dim, int *k, int *rmv, int *nrmv) {
  for (int i=0; i < nrmv[0]; i++) {
    if (dim[rmv[i]-1] > 1) doone(x, dim, k[0], rmv[i]);

    k[0] -= 1;
    for (int j=rmv[i]-1; j < k[0]; j++) {
      dim[j] = dim[j+1];
    }
    for (int j=i+1; j < nrmv[0]; j++) {
      rmv[j] -= (rmv[j] > rmv[i] ? 1 : 0);
    }
  }
}

/* given dim[0] by dim[1] array 'x', normalize 
    so that columns sum to 1. */
void propTable0 (double *x, int *dim) {
  long double y = 0.0;

  for (int i=0; i < dim[1]; i++) {
    y = 0.0;
    
    for (int j=0; j < dim[0]; j++) {
      y += x[dim[0]*i + j];
    }
    for (int j=0; j < dim[0]; j++) {
      x[dim[0]*i + j] /= y;
    }
  }
}

/* repeat blocks of length 'every' 'each' times */
void rep_int(int every, int each, int len, int *out) {
  int *out2;
  out2 = malloc(len*sizeof(int));

  for (int i=0; i<len; i++) out2[i] = out[i];

  for (int i=0; i<len; i += every) {
    for (int j=0; j<each; j++) {
      for (int k=0; k<every; k++) {
	out[i*each+j*every+k] = out2[i+k];
      }
    }
  }

  free(out2);
}

/* get indices of an array after permuting the dimensions (so
   generalized transpose) */
void permIndex (int *perm, int *dim, int *ndim, int *out) {
  int cp[ndim[0]+1], state[ndim[0]];
  cp[0] = 1;
  for (int i=0; i<ndim[0]; i++) {
    cp[i+1] = cp[i]*dim[i];
    state[i] = 0;
  }

  int loc = 0;

  out[0] = 0;

  for (int i=1; i<cp[ndim[0]]; i++) {
    for (int j=0; j<ndim[0]; j++) {
      if (state[perm[j]] < dim[perm[j]]-1) {
	loc += cp[perm[j]];
	state[perm[j]] += 1;
	break;
      }
      else {
	loc -= cp[perm[j]+1]-cp[perm[j]];
	state[perm[j]] = 0;
      }
    }

    out[i] = loc;
  }
}

/* Gives indices which match subset of dimensions in an array.

   Given an ordered subset of variables and a complete vector of
   dimensions of those variables, returns a vectors of integers
   showing which elements of a full array correspond to entries in a
   relevant subset vector.

   For example, if a full 4-way array has dimensions 2*2*2*2 and
   visible margin for variables 2 and 4, then the function returns
   [0,0,1,1,0,0,1,1,2,2,3,3,2,2,3,3].  This is because in a 2*2*2*2
   array, the 1st, 2nd, 5th and 6th entries correspond to states in
   which the second and fourth dimension indices are 0.  Similarly,
   the 3rd, 4th, 7th and 8th all have second dimension 1 and fourth
   dimension 0.

   int *perm     : pointer to an integer vector of the variables present
   int *permlen  : pointer to an integer giving length of perm.
   int *dim      : pointer to an integer vector giving the dimensions
                   of the full array
   int *ndim     : pointer to an integer giving length of dim (i.e. the
                   number of dimensions in the full array
   int *out      : pointer to an integer array to store the output;
                   should have length given by the product of the
                   entries in dim.  First index
 */

void patternRepeat_c (int *perm, int *permlen, int *dim, int *ndim, int *out) {

  int startlen = 1;
  int reorder = 0;
  int loc = 0;

  /* if necessary, first perumte the indices as given to correct order */
  for (int i=0; i<permlen[0]; i++) {
    startlen *= dim[perm[i]];
    if (perm[i+1] < perm[i] && i<permlen[0]-1) reorder = 1;
  }

  if (reorder > 0) {
    /* if variables in 'perm' were not in order, we need to fix this */
    int inc[ndim[0]], perm2[permlen[0]], permdim[permlen[0]], ord[permlen[0]];
    bool keep_order = (out[0] == 0);

    for (int i=0; i<ndim[0]; i++) inc[i] = 0;

    /* make a copy of permutation, and record present variables as
       'inc' */
    for (int i=0; i<permlen[0]; i++) {
      inc[perm[i]] = 1;
      permdim[i] = dim[perm[i]];
      perm2[i] = perm[i];
    }
    loc = 0;

    for (int i=0; i<permlen[0]; i++) {
      /* get to next index present (in order) */
      while (inc[loc] == 0) loc++;

      for (int j=0; j<permlen[0]; j++) {
	if (perm2[j] == loc) {
	  /* copy ordered location to perm, record order as ord */
	  perm[i] = loc;
	  ord[i] = j;
	  loc++;
	  break;
	}
      }
    }
    if (keep_order) {
      /* if given order of variables is to be respected, use permIndex
	 to get initial vector of indices */
      permIndex(ord, permdim, permlen, out);
    }
    else {
      /* else just initialize output as 0,1,2,...  */
      for (int i=0; i<startlen; i++) {
	out[i] = i;
      }
    }

  }
  else {
    /* just initialize output as 0,1,2,...  */
    for (int i=0; i<startlen; i++) {
      out[i] = i;
    }
  }
  int totlen = startlen;

  /* add 1 for R indices starting at 1 */
  for (int i=0; i< startlen; i++) {
    out[i] += 1;
  }

  /* now the main business */
  int cp = 1;  /* total length up to current index */
  loc = 0; /* current position out of dimensions provided */
  for (int i=0; i < ndim[0]; i++) {
    if (loc < permlen[0] && i==perm[loc]) {
      /* if dimension is included, then move to next */
      loc++;
    }
    else {
      /* else repeat along index */
      rep_int(cp, dim[i], totlen, out);
      totlen *= dim[i];
    }
    cp *= dim[i];
  }
}

/* Fast Hadamard transform
 
 double *x    - vector to be transformed
int    *k    - number of dimensions (log2 length of x)

*/
void hadamard_c (double *x, int *k) {
  int p1,p2;
  double tmp;
  for (int i=0; i < k[0]; i++) {
    int pos = 0;

    for (int ind2=0; ind2 < pow(2,k[0]-i-1); ind2++) {
      for (int ind1=0; ind1 < pow(2,i); ind1++) {
        p1 = ind1 + ind2*pow(2,i+1);
        p2 = p1 + pow(2,i);
        
/*        Rprintf("%i: %i %i %i %i\n", i, ind1, ind2, p1, p2); */
        
        if (p1 >= pow(2,k[0])) 
        {
          printf("error p1\n");
          return;
        }
        if (p2 >= pow(2,k[0]))         
          {
          printf("error p2\n");
          return;
        }

        tmp = x[p1] - x[p2];
        x[p1] += x[p2];
        x[p2] = tmp;
        pos += 1;
      }
      pos += pow(2,i+1);
    }
  }
}

