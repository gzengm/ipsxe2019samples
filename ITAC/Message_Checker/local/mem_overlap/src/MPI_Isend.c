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
 * This example uses the same buffer in several different
 * non-blocking sends at the same time. The MPI standard does not
 * allow that because some MPI implementations might have to
 * manipulate the memory.
 * 
 * Because a violation of this rule has no negative consequences with
 * most MPI implementations and is convenient, many applications
 * contain the same error. They should be fixed to increase
 * portability.
 */

#include <mpi.h>
#include <string.h>
#include <stdlib.h>

int main (int argc, char **argv)
{
    int rank, size, peer;

    MPI_Init( &argc, &argv );
    MPI_Comm_size( MPI_COMM_WORLD, &size );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );

    if( !rank ) {
        char send = 0;
        MPI_Request *reqs;
        MPI_Status *statuses;

        reqs = malloc( (size - 1) * sizeof(reqs[0]) );
        statuses = malloc( (size - 1) * sizeof(statuses[0]) );
        for( peer = 1; peer < size; peer++ ) {
            /* warning: reuses the same buffer as previous iterations */
            MPI_Isend( &send, 1, MPI_CHAR, peer, 100, MPI_COMM_WORLD, reqs + peer - 1 );
        }
        MPI_Waitall( size - 1, reqs, statuses );
        free( reqs );
        free( statuses );
    } else {
        char recv;
        MPI_Status status;

        MPI_Recv( &recv, 1, MPI_CHAR, 0, 100, MPI_COMM_WORLD, &status );
    }

    MPI_Finalize( );

    return 0;
}
