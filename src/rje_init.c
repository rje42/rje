#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void indexBox_c(void *, void *, void *, void *, void *);
extern void marginTable_c(void *, void *, void *, void *, void *);
extern void patternRepeat_c(void *, void *, void *, void *, void *);
extern void propTable0(void *, void *);
extern void hadamard_c(void *, void *);

static const R_CMethodDef CEntries[] = {
  {"indexBox_c",      (DL_FUNC) &indexBox_c,      5},
  {"marginTable_c",   (DL_FUNC) &marginTable_c,   5},
  {"patternRepeat_c", (DL_FUNC) &patternRepeat_c, 5},
  {"propTable0",      (DL_FUNC) &propTable0,      2},
  {"hadamard_c",      (DL_FUNC) &hadamard_c,      2},
  {NULL, NULL, 0}
};

void R_init_rje(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
