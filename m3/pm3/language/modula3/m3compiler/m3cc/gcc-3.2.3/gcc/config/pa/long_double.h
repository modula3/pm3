/* Definitions of long double support for GNU compiler.
   Copyright (C) 2000 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#define LONG_DOUBLE_TYPE_SIZE 128

/* Define library calls for quad FP operations.  These are all part of the
   PA32 and PA64 ABIs.  */
#define ADDTF3_LIBCALL "_U_Qfadd"
#define SUBTF3_LIBCALL "_U_Qfsub"
#define MULTF3_LIBCALL "_U_Qfmpy"
#define DIVTF3_LIBCALL "_U_Qfdiv"
#define NEGTF2_LIBCALL "_U_Qfneg"
#define SQRTTF2_LIBCALL "_U_Qfsqrt"
#define ABSTF2_LIBCALL "_U_Qfabs"
#define SMINTF3_LIBCALL "_U_Qfmin"
#define SMAXTF3_LIBCALL "_U_Qfmax"
#define EXTENDSFTF2_LIBCALL "_U_Qfcnvff_sgl_to_quad"
#define EXTENDDFTF2_LIBCALL "_U_Qfcnvff_dbl_to_quad"
#define TRUNCTFSF2_LIBCALL "_U_Qfcnvff_quad_to_sgl"
#define TRUNCTFDF2_LIBCALL "_U_Qfcnvff_quad_to_dbl"
#define FLOATSITF2_LIBCALL "_U_Qfcnvxf_sgl_to_quad"
#define FLOATDITF2_LIBCALL "_U_Qfcnvxf_dbl_to_quad"
/* We need to put a wrapper function around _U_Qfcnvfxt_quad_to_sgl so that
   we can massage its return value for PA64.  */
#define FIX_TRUNCTFSI2_LIBCALL \
  (TARGET_64BIT ? "__U_Qfcnvfxt_quad_to_sgl" : "_U_Qfcnvfxt_quad_to_sgl")
#define FIXUNS_TRUNCTFSI2_LIBCALL "_U_Qfcnvfxt_quad_to_usgl"
#define FIX_TRUNCTFDI2_LIBCALL "_U_Qfcnvfxt_quad_to_dbl"
#define EQTF2_LIBCALL "_U_Qfeq"
#define NETF2_LIBCALL "_U_Qfne"
#define GTTF2_LIBCALL "_U_Qfgt"
#define GETF2_LIBCALL "_U_Qfge"
#define LTTF2_LIBCALL "_U_Qflt"
#define LETF2_LIBCALL "_U_Qfle"


#define INIT_TARGET_OPTABS						\
  do {									\
    add_optab->handlers[(int) TFmode].libfunc				\
      = gen_rtx_SYMBOL_REF (Pmode, ADDTF3_LIBCALL);			\
    sub_optab->handlers[(int) TFmode].libfunc				\
      = gen_rtx_SYMBOL_REF (Pmode, SUBTF3_LIBCALL);			\
    smul_optab->handlers[(int) TFmode].libfunc				\
      = gen_rtx_SYMBOL_REF (Pmode, MULTF3_LIBCALL);			\
    sdiv_optab->handlers[(int) TFmode].libfunc				\
      = gen_rtx_SYMBOL_REF (Pmode, DIVTF3_LIBCALL);			\
    smin_optab->handlers[(int) TFmode].libfunc				\
      = gen_rtx_SYMBOL_REF (Pmode, SMINTF3_LIBCALL);			\
    smax_optab->handlers[(int) TFmode].libfunc				\
      = gen_rtx_SYMBOL_REF (Pmode, SMAXTF3_LIBCALL);			\
    sqrt_optab->handlers[(int) TFmode].libfunc				\
      = gen_rtx_SYMBOL_REF (Pmode, SQRTTF2_LIBCALL);			\
    abs_optab->handlers[(int) TFmode].libfunc				\
      = gen_rtx_SYMBOL_REF (Pmode, ABSTF2_LIBCALL);			\
    neg_optab->handlers[(int) TFmode].libfunc				\
      = gen_rtx_SYMBOL_REF (Pmode, NEGTF2_LIBCALL);			\
    extendsftf2_libfunc = gen_rtx_SYMBOL_REF (Pmode, EXTENDSFTF2_LIBCALL); \
    extenddftf2_libfunc = gen_rtx_SYMBOL_REF (Pmode, EXTENDDFTF2_LIBCALL); \
    trunctfsf2_libfunc = gen_rtx_SYMBOL_REF (Pmode, TRUNCTFSF2_LIBCALL); \
    trunctfdf2_libfunc = gen_rtx_SYMBOL_REF (Pmode, TRUNCTFDF2_LIBCALL); \
    floatsitf_libfunc = gen_rtx_SYMBOL_REF (Pmode, FLOATSITF2_LIBCALL);  \
    floatditf_libfunc = gen_rtx_SYMBOL_REF (Pmode, FLOATDITF2_LIBCALL);  \
    fixtfsi_libfunc = gen_rtx_SYMBOL_REF (Pmode, FIX_TRUNCTFSI2_LIBCALL);\
    fixtfdi_libfunc = gen_rtx_SYMBOL_REF (Pmode, FIX_TRUNCTFDI2_LIBCALL);\
    fixunstfsi_libfunc = gen_rtx_SYMBOL_REF (Pmode, FIXUNS_TRUNCTFSI2_LIBCALL);\
    fixunstfdi_libfunc = gen_rtx_SYMBOL_REF (Pmode, FIX_TRUNCTFDI2_LIBCALL);  \
    eqtf2_libfunc = gen_rtx_SYMBOL_REF (Pmode, EQTF2_LIBCALL);		\
    netf2_libfunc = gen_rtx_SYMBOL_REF (Pmode, NETF2_LIBCALL);		\
    gttf2_libfunc = gen_rtx_SYMBOL_REF (Pmode, GTTF2_LIBCALL);		\
    getf2_libfunc = gen_rtx_SYMBOL_REF (Pmode, GETF2_LIBCALL);		\
    lttf2_libfunc = gen_rtx_SYMBOL_REF (Pmode, LTTF2_LIBCALL);		\
    letf2_libfunc = gen_rtx_SYMBOL_REF (Pmode, LETF2_LIBCALL);		\
    INIT_SUBTARGET_OPTABS;						\
  } while (0)

/* This is meant to be redefined in the host dependent files */
#define INIT_SUBTARGET_OPTABS

/* Nonzero if a floating point comparison library call for
   mode MODE that will return a boolean value.  Zero if one
   of the libgcc2 functions is used.  */
#define FLOAT_LIB_COMPARE_RETURNS_BOOL(MODE, COMPARISON) ((MODE) == TFmode)
