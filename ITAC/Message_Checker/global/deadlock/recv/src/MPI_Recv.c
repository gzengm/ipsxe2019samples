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
 * Every process enters MPI_Recv() from its right neighbor, but
 * none of them sends any message, which leads to a deadlock.
 */

#include <mpi.h>
#include <string.h>
#include <stdlib.h>

int main (int argc, char **argv)
{
    int rank, size, peer;
    char send, recv;
    MPI_Status status;

    MPI_Init( &argc, &argv );
    MPI_Comm_size( MPI_COMM_WORLD, &size );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    peer = rank + 1;
    if( peer >= size ) {
        peer = 0;
    }

    /* waiting for a message that has not been sent */
    MPI_Recv( &recv, 1, MPI_CHAR, peer, 100, MPI_COMM_WORLD, &status );

    /*
     * Too late, this code is not going to be reached.
     * Beware, simply moving this code up would rely on
     * buffering in the MPI. The correct solution is to
     * use MPI_Isend() before the receive and MPI_Wait()
     * afterwards.
     */
    send = 0;
    MPI_Send( &send, 1, MPI_CHAR, peer, 100, MPI_COMM_WORLD );

    MPI_Finalize( );

    return 0;
}
