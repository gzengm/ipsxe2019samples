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
 * A failed MPI call can have various reasons, the most common one
 * being invalid parameters. Depending on whether the problem is
 * expected to change the application logic, such a failure is handled
 * as a warning or error.
 */

#include <mpi.h>

int main (int argc, char **argv)
{
    int rank, size, peer;
    MPI_Request req = MPI_REQUEST_NULL;
    char send = 123;

    MPI_Init( &argc, &argv );
    MPI_Comm_size( MPI_COMM_WORLD, &size );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );

    /* warning: freeing MPI_COMM_NULL */
    MPI_Request_free( &req );

    /* error: invalid recipient */
    peer = size;
    MPI_Send( &send, 1, MPI_CHAR, peer, 100, MPI_COMM_WORLD );

    MPI_Finalize( );

    return 0;
}
