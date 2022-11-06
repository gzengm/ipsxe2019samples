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
 * An MPI_Send(MPI_CHAR) is matched by an MPI_Recv(MPI_INT),
 * which leads to a type mismatch.
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
    peer = rank ^ 1;

    if( peer < size ) {
        char send = 123;
        int recv;
        MPI_Status status;

        /* error: types do not match */
        if( rank % 2 ) {
            MPI_Send( &send, 1, MPI_CHAR, peer, 100, MPI_COMM_WORLD );
        } else {
            MPI_Recv( &recv, 1, MPI_INT, peer, 100, MPI_COMM_WORLD, &status );
        }
    }

    MPI_Finalize( );

    return 0;
}
