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

/*
 * The root uses a different datatype than the receiving processes in
 * MPI_Bcast(). The number of transmitted bytes is the same, so
 * normally this error is not detected by MPI.
 */

#include <mpi.h>
#include <string.h>
#include <stdlib.h>

int main (int argc, char **argv)
{
    int rank, size;

    MPI_Init( &argc, &argv );
    MPI_Comm_size( MPI_COMM_WORLD, &size );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );

    /* error: types do not match */
    if( !rank ) {
        int send = 0;
        MPI_Bcast( &send, 1, MPI_INT, 0, MPI_COMM_WORLD );
    } else {
        char recv[4];
        MPI_Bcast( &recv, 4, MPI_CHAR, 0, MPI_COMM_WORLD );
    }

    MPI_Finalize( );

    return 0;
}
