/*
 *	Copyright 1988, University Corporation for Atmospheric Research
 *
 *  Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose without fee is hereby granted, provided
 * that the above copyright notice appear in all copies, that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of UCAR/Unidata not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.  UCAR makes no
 * representations about the suitability of this software for any purpose.
 * It is provided "as is" without express or implied warranty.  It is
 * provided with no support and without obligation on the part of UCAR
 * Unidata, to assist in its use, correction, modification, or enhancement.
 *
 */
/* "$Id: netcdf.h,v 1.58 1991/09/20 19:56:13 russ Exp $" */

#ifndef _NETCDF_
#define _NETCDF_

/*
 *  The definitions ncvoid, USE_ENUM, MAX_NC_OPEN, XDR_D_INFINITY
 * and XDR_F_INFINITY may need to be set properly for your installation.
 * See ../PORTING
 */

/*
 * Argument type in user functions
 */
#if defined(__STDC__) || defined(sun) || defined(NeXT) /* has (void *) */
#define ncvoid    void
#else /* ultrix cc, for example, has void but no void star */
#define ncvoid    char
#endif


/*
 *   If xdr_enum works properly on your system, you can define 
 * USE_ENUM so that nc_type is an enum. Otherwise, nc_type is
 * an int and the valid values are #defined.
 */
#if defined(sun) || defined(vax) || defined(NeXT)
#define USE_ENUM
#endif
   
/*
 * End of Installation Defined Section.
 * You probably shouldn't modify anything beyond this point
 */


/*
 * 	Fill values
 * These values are stuffed into newly allocated space as appropriate.
 * The hope is that one might use these to notice that a particular dataum
 * has not been set.
 */
/* see ../extrema/check_infinity.c */
#ifdef vax
#define	XDR_D_INFINITY	1.7014118346046923e+38
#define	XDR_F_INFINITY	1.70141173e+38
#else
#ifdef cray
#define	XDR_D_INFINITY	1.797693134862313000e+308
#define	XDR_F_INFINITY	XDR_D_INFINITY
#else /* default */
#    define USE_F_LONG_PUN
#ifndef sun		      /* fails in SunOS 4.0.3, OK in later releases */
#    define USE_D_LONG_PUN
#endif
#ifdef  USE_F_LONG_PUN
extern long xdr_f_infinity ;  /* instantiated in array.c */
#define XDR_F_INFINITY *((float *)&xdr_f_infinity)
#else
#define XDR_F_INFINITY	3.40282357e+38
#endif /* USE_F_LONG_PUN */
#ifdef USE_D_LONG_PUN
extern long xdr_d_infinity[] ;  /* instantiated in array.c */
#define XDR_D_INFINITY *(double *)xdr_d_infinity
#else
#define XDR_D_INFINITY	1.797693134862315900e+308
#endif /* USE_D_LONG_PUN */
#endif
#endif


#define FILL_BYTE	((char)-127)		/* Largest Negative value */
#define FILL_CHAR	((char)0)
#define FILL_SHORT	((short)-32767)
#define FILL_LONG	((long)-2147483647)
#define FILL_FLOAT	XDR_F_INFINITY	/* IEEE Infinity */
#define FILL_DOUBLE	XDR_D_INFINITY

/*
 * Added feature. The above values are defaults.
 * If you wish a variable to use a different value than the above
 * defaults, create an attribute with the same type as the variable
 * and the following reserved name. The value you give the attribute
 * will be used as the fill value for that variable.
 */
#define _FillValue	"_FillValue"


/*
 *  masks for the struct NC flags field; passed in as 'mode' arg to
 * nccreate and ncopen.
 *
 */
#define NC_RDWR  1		/* read/write, 0 => readonly */
#define NC_CREAT 2		/* in create phase, cleared by ncendef */
#define NC_EXCL  4		/* on create, don't destroy existing file */
#define NC_INDEF 8		/* in define mode, cleared by ncendef */
#define NC_NSYNC 0x10	/* synchronise numrecs on change */
#define NC_HSYNC 0x20	/* synchronise whole header on change */
#define NC_NDIRTY 0x40	/* numrecs has changed */
#define NC_HDIRTY 0x80  /* header info has changed */
#define NC_NOFILL 0x100	/* Don't fill vars on endef and increase of record */
#define NC_LINK 0x8000	/* isa link */

#define NC_FILL 0	/* argument to ncsetfill to clear NC_NOFILL */

/*
 * 'mode' arguments for nccreate and ncopen
 */
#define NC_NOWRITE   0
#define NC_WRITE     NC_RDWR
#define NC_CLOBBER   (NC_INDEF | NC_CREAT | NC_RDWR)
#define NC_NOCLOBBER (NC_INDEF | NC_EXCL | NC_CREAT | NC_RDWR)

/*
 * 'size' argument to ncdimdef for an unlimited dimension
 */
#define NC_UNLIMITED 0L

/*
 * attribute id to put/get a global attribute
 */
#define NC_GLOBAL -1


/*
 * This can be as large as the maximum number of stdio streams
 * you can have open on your system.
 */
#define MAX_NC_OPEN 32

/*
 * These maximums are enforced by the interface.
 * However, nothing is statically allocated to these sizes internally.
 */
#define MAX_NC_DIMS 32
#define MAX_NC_ATTRS 512
#define MAX_NC_VARS 512
#define MAX_NC_NAME 128
#define MAX_VAR_DIMS MAX_NC_DIMS

#ifdef USE_ENUM
/*
 *  The netcdf data types
 */
typedef enum {
	NC_UNSPECIFIED, /* private */
	NC_BYTE,
	NC_CHAR,
	NC_SHORT,
	NC_LONG,
	NC_FLOAT,
	NC_DOUBLE,
	/* private */
	NC_BITFIELD,
	NC_STRING,
	NC_IARRAY,
	NC_DIMENSION,
	NC_VARIABLE,
	NC_ATTRIBUTE
} nc_type ;
#else
typedef int nc_type ;
#define	NC_UNSPECIFIED 0 /* private */
#define	NC_BYTE 1
#define	NC_CHAR 2
#define	NC_SHORT 3
#define	NC_LONG 4
#define	NC_FLOAT 5
#define	NC_DOUBLE 6
	/* private */
#define	NC_BITFIELD 7
#define	NC_STRING 8
#define	NC_IARRAY 9
#define	NC_DIMENSION 10
#define	NC_VARIABLE 11
#define	NC_ATTRIBUTE 12
#endif


/*
 * Global netcdf error status variable
 *  Initialized in error.c
 */
#define	NC_NOERR	0	/* No Error */
#define	NC_EBADID	1	/* Not a netcdf id */
#define	NC_ENFILE	2	/* Too many netcdfs open */
#define	NC_EEXIST	3	/* netcdf file exists && NC_NOCLOBBER */
#define	NC_EINVAL	4	/* Invalid Argument */
#define	NC_EPERM	5	/* Write to read only */
#define	NC_ENOTINDEFINE	6	/* Operation not allowed in data mode */
#define	NC_EINDEFINE	7	/* Operation not allowed in define mode */
#define	NC_EINVALCOORDS	8	/* Coordinates out of Domain */
#define	NC_EMAXDIMS	9	/* MAX_NC_DIMS exceeded */
#define	NC_ENAMEINUSE	10	/* String match to name in use */
#define NC_ENOTATT	11	/* Attribute not found */
#define	NC_EMAXATTS	12	/* MAX_NC_ATTRS exceeded */
#define NC_EBADTYPE	13	/* Not a netcdf data type */
#define NC_EBADDIM	14	/* Invalid dimension id */
#define NC_EUNLIMPOS	15	/* NC_UNLIMITED in the wrong index */
#define	NC_EMAXVARS	16	/* MAX_NC_VARS exceeded */
#define NC_ENOTVAR	17	/* Variable not found */
#define NC_EGLOBAL	18	/* Action prohibited on NC_GLOBAL varid */
#define NC_ENOTNC	19	/* Not a netcdf file */
#define NC_ESTS         20      /* In Fortran, string too short */
#define NC_EMAXNAME     21      /* MAX_NC_NAME exceeded */
#define NC_ENTOOL       NC_EMAXNAME   /* Backward compatibility */
#define NC_EUNLIMIT     22      /* NC_UNLIMITED size already in use */

#define	NC_EXDR		32	/* */
#define	NC_SYSERR	-1

extern int ncerr ;

/*
 * Global options variable. Used to determine behavior of error handler.
 *  Initialized in lerror.c
 */
#define	NC_FATAL	1
#define	NC_VERBOSE	2

extern int ncopts ;	/* default is (NC_FATAL | NC_VERBOSE) */

/*
 * NB: The following feature-test line is too long in order to accomodate a 
 * bug in the VMS 5.3 C compiler.
 */
#ifndef HAVE_PROTOTYPES
#   if defined(__STDC__) || defined(__GNUC__) || defined(__cplusplus) || defined(c_plusplus)
#       define	HAVE_PROTOTYPES
#   endif
#endif

#undef PROTO
#ifdef HAVE_PROTOTYPES 
#   define	PROTO(x)	x
#else
#   define	PROTO(x)	()
#endif

#ifdef __cplusplus
extern "C" {
#endif

extern int nccreate	PROTO((
    const char*	path,
    int		cmode
));
extern int ncopen	PROTO((
    const char*	path,
    int		mode
));
extern int ncredef	PROTO((
    int		cdfid
));
extern int ncendef	PROTO((
    int		cdfid
));
extern int ncclose	PROTO((
    int		cdfid
));
extern int ncinquire	PROTO((
    int		cdfid,
    int*	ndims,
    int*	nvars,
    int*	natts, 
    int*	recdim
));
extern int ncsync	PROTO((
    int		cdfid
));
extern int ncabort	PROTO((
    int		cdfid
));
extern int ncdimdef	PROTO((
    int		cdfid,
    const char*	name,
    long	length
));
extern int ncdimid	PROTO((
    int		cdfid,
    const char*	name
));
extern int ncdiminq	PROTO((
    int		cdfid,
    int		dimid,
    char*	name,
    long*	length
));
extern int ncdimrename	PROTO((
    int		cdfid,
    int		dimid,
    const char*	name
));
extern int ncvardef	PROTO((
    int		cdfid,
    const char*	name,
    nc_type	datatype, 
    int		ndims,
    int		*dim
));
extern int ncvarid	PROTO((
    int		cdfid,
    const char*	name
));
extern int ncvarinq	PROTO((
    int		cdfid,
    int		varid,
    char*	name,
    nc_type*	datatype,
    int*	ndims,
    int		*dim,
    int*	natts
));
extern int ncvarput1	PROTO((
    int		cdfid,
    int		varid,
    long	*coords,
    void*	value
));
extern int ncvarget1	PROTO((
    int		cdfid,
    int		varid,
    long*	coords,
    void*	value
));
extern int ncvarput	PROTO((
    int		cdfid,
    int		varid,
    long*	start,
    long*	count, 
    void*	value
));
extern int ncvarget	PROTO((
    int		cdfid,
    int		varid,
    long*	start,
    long*	count, 
    void*	value
));
extern int ncvarrename	PROTO((
    int		cdfid,
    int		varid,
    const char*	name
));
extern int ncattput	PROTO((
    int		cdfid,
    int		varid,
    const char*	name, 
    nc_type	datatype,
    int		len,
    void*	value
));
extern int ncattinq	PROTO((
    int		cdfid,
    int		varid,
    const char*	name, 
    nc_type*	datatype,
    int*	len
));
extern int ncattget	PROTO((
    int		cdfid,
    int		varid,
    const char*	name, 
    void*	value
));
extern int ncattcopy	PROTO((
    int		incdf,
    int		invar,
    const char*	name, 
    int		outcdf,
    int		outvar
));
extern int ncattname	PROTO((
    int		cdfid,
    int		varid,
    int		attnum,
    char*	name
));
extern int ncattrename	PROTO((
    int		cdfid,
    int		varid,
    const char*	name, 
    const char*	newname
));
extern int ncattdel	PROTO((
    int		cdfid,
    int		varid,
    const char*	name
));
extern int nctypelen	PROTO((
    nc_type	datatype
));
extern int ncsetfill	PROTO((
    int		cdfid,
    int		fillmode
));

#ifdef __cplusplus
}
#endif

#endif /* _NETCDF_ */
