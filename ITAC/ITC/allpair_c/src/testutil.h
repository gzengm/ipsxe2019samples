/*******************************************************************************
 *
 * 				  COPYRIGHT
 * 
 * The following is a notice of limited availability of the code, and disclaimer
 * which must be included in the prologue of the code and in all source listings
 * of the code.
 * 
 * Copyright Notice
 *  + 1993 University of Chicago
 *  + 1993 Mississippi State University
 * 
 * Permission is hereby granted to use, reproduce, prepare derivative works, and
 * to redistribute to others.  This software was authored by:
 * 
 * Argonne National Laboratory Group
 * W. Gropp: (630) 252-4318; FAX: (630) 252-5986; e-mail: gropp@mcs.anl.gov
 * E. Lusk:  (630) 252-7852; FAX: (630) 252-5986; e-mail: lusk@mcs.anl.gov
 * Mathematics and Computer Science Division
 * Argonne National Laboratory, Argonne IL 60439
 * 
 * Mississippi State Group
 * N. Doss:  (601) 325-2565; FAX: (601) 325-7692; e-mail: doss@erc.msstate.edu
 * A. Skjellum:(601) 325-8435; FAX: (601) 325-8997; e-mail: tony@erc.msstate.edu
 * Mississippi State University, Computer Science Department &
 *  NSF Engineering Research Center for Computational Field Simulation
 * P.O. Box 6176, Mississippi State MS 39762
 * 
 * 			      GOVERNMENT LICENSE
 * 
 * Portions of this material resulted from work developed under a U.S.
 * Government Contract and are subject to the following license: the Government
 * is granted for itself and others acting on its behalf a paid-up, nonexclusive,
 * irrevocable worldwide license in this computer software to reproduce, prepare
 * derivative works, and perform publicly and display publicly.
 * 
 * 				  DISCLAIMER
 * 
 * This computer code material was prepared, in part, as an account of work
 * sponsored by an agency of the United States Government.  Neither the United
 * States, nor the University of Chicago, nor Mississippi State University, nor
 * any of their employees, makes any warranty express or implied, or assumes any
 * legal liability or responsibility for the accuracy, completeness, or
 * usefulness of any information, apparatus, product, or process disclosed, or
 * represents that its use would not infringe privately owned rights.
 *******************************************************************************/

#ifndef _ITC_UTIL_H_
#define _ITC_UTIL_H_

#ifdef V_T

#include <VT.h>
#include <stdlib.h>

#define CHECK_RES( function, name, res ) { int _res = function; if( (res) != _res ) { fprintf( stderr, "[line %d] " #function " returned %d, not %s\n", __LINE__, _res, #res ); exit(10); } }
#define CHECK_FAIL( function, name ) { int e = function; if(e != VT_ERR_NOTINITIALIZED) { fprintf( stderr, "[line %d] " name " returned %d and not VT_ERR_NOTINITIALIZED\n", __LINE__, e); exit(10); } }
#define LOCDEF()  { int loc; VT_scldef( __FILE__, __LINE__, &loc ); VT_thisloc( loc ); }

#define VT_Clock( ) VT_timestamp()

#else

#define LOCDEF()
#define VT_symdef( a, b, c )
#define VT_begin( a )
#define VT_end( a )
#define VT_log_comment( a )
#define VT_flush( )
#define VT_Clock( ) VT_Wtime()
#define VT_traceoff()
#define VT_traceon()
#define VT_registerthread(x) 0
#define VT_getthrank( x )
#define VT_funcdef( a, b, c )
#define VT_initialize( a, b )
#define VT_finalize()
#define VT_enter( a, b )
#define VT_leave( a )

#define CHECK_RES( function, name, res ) function
#define CHECK_FAIL( function, name )     function

#endif

#define CHECK( function, name ) CHECK_RES( function, name, VT_OK )

/* timing functions from VT_usleep.c */
void VT_usleep( int usecs );
#define VT_sleep( seconds ) VT_usleep( 1000000 * (seconds) )

/* from timerperformance.c */
void timerperformance( const char *prefix, double duration );
const char *printbar( int width );
const char *prettyprintseconds( double seconds, int width, int precision, char *buffer );
void sortlongdouble( long double *array, int count );

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/* 
 *  Adjustments for Windows: 
 */
#ifdef _WIN32

/* some system functions: */
# include <io.h>     /* open  */
# include <direct.h> /* mkdir */
# define mkdir( path, mode ) mkdir( path )

/* emulation of setenv: */
#include <stdlib.h>
#include <stdio.h>
static int setenv( const char *name, const char *value, int overwrite ) {
    char buffer[1024];
    if ( ! overwrite && getenv( name ) ) { return 0; }
    _snprintf( buffer, sizeof( buffer ), "%s=%s", name, value );
    return _putenv( buffer );
}

#endif

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#endif /* _ITC_UTIL_H_ */
