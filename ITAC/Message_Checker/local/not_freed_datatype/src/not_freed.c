/***************************************************************************************
 *
 * SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
 * http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
 *
 * Copyright 2018 Intel Corporation
 *
 * THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
 *
 ***************************************************************************************/

#include <mpi.h>
#include <assert.h>

/**
 * This example covers the datatype leak reporting. It is expected to be run with
 * one process and CHECK-MAX-DATATYPES 4.
 *
 * MPI_Type_lb() is used to check whether the invalid and valid datatypes are properly
 * distinguished.
 */

int main (int argc, char **argv)
{
    int i, e;
    MPI_Aint lb;
    MPI_Datatype type, types[4];
    MPI_Aint indices[1];
    int blocklens[1];
    MPI_Datatype oldtypes[1];

    MPI_Init( &argc, &argv );

    indices[0] = 0;
    blocklens[0] = 2;
    oldtypes[0] = MPI_CHAR;

    /* generate four distinct datatypes - should trigger a leak report */
    for( i = 0; i < 4; i++ ) {
        MPI_Type_vector( 1, i+2, i+2, MPI_CHAR, types + i );
        MPI_Type_commit( types + i );
        MPI_Type_lb( types[i], &lb );
    }
    /* now free one and create it again - should not trigger a report */
    type = types[3];
    MPI_Type_free( types + 3 );
    MPI_Type_lb( type, &lb );
    MPI_Type_vector( 1, 3+2, 3+2, MPI_CHAR, types + 3 );
    MPI_Type_commit( types + 3 );

    /* creating the same type over and over again is expected to create new handles */
    for( i = 0; i < 2; i++ ) {
        MPI_Type_vector( 1, 10, 10, MPI_CHAR, types + i );
        MPI_Type_commit( types + i );
        assert( i == 0 || types[i] != types[0] );
    }
    /* each type has to be freed once */
    type = types[1];
    MPI_Type_free( types + 1 );
    MPI_Type_lb( type, &lb );
    MPI_Type_lb( types[0], &lb );
    /* now trigger leak report */
    for( i = 1; i < 4; i++ ) {
        MPI_Type_vector( 1, 10, 10, MPI_CHAR, types + i );
        MPI_Type_commit( types + i );
        assert( i == 0 || types[i] != types[0] );
    }

    /* create the same type (two chars) in four other ways, thus trigger a leak report */
    MPI_Type_hvector( 1, 2, 2, MPI_CHAR, &type );
    MPI_Type_commit( &type );
    MPI_Type_commit( &type ); /* should have no effect */
    MPI_Type_contiguous( 2, MPI_CHAR, &type );
    /* MPI_Type_commit( &type ); */ /* intentionally missing - should be included in leak report anyway */
    MPI_Type_hindexed( 1, blocklens, indices, MPI_CHAR, &type );
    MPI_Type_commit( &type );
    MPI_Type_struct( 1, blocklens, indices, oldtypes, &type );
    MPI_Type_commit( &type );

    /*
     * Create a custom type, then "duplicate" it by creating
     * a type derived from it which is identical to it
     * (in MPI-2 one could use MPI_Type_dup()). First create
     * and free without commit, then with.
     *
     * None of this should trigger an error. 
     */
    MPI_Type_contiguous( 2, MPI_CHAR, types + 0 );
    MPI_Type_contiguous( 1, types[0], types + 1 );
    MPI_Type_free( types + 0 );
    MPI_Type_free( types + 1 );

    MPI_Type_contiguous( 2, MPI_CHAR, types + 0 );
    MPI_Type_commit( types + 0);
    MPI_Type_contiguous( 1, types[0], types + 1 );
    MPI_Type_commit( types + 1 );
    MPI_Type_free( types + 0 );
    MPI_Type_free( types + 1 );
    
    MPI_Finalize( );

    return 0;
}
