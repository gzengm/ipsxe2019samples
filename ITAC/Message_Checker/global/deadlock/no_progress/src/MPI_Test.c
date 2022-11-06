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
 * One process calls MPI_Isend() and then polls for completion, while
 * the others call MPI_Send() directly. This happens to work for small
 * messages, if the MPI buffers the data, but there is no guarantee
 * that and at which message sizes this is done.
 *
 * The polling means that there is no real deadlock, because that process
 * could still resolve the situation by posting a receive.
 */

#include <mpi.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>

#ifndef _WIN32
# include <unistd.h>
#else
# include <windows.h>
# define sleep( n ) Sleep( 1000 * (n) )
#endif

int main (int argc, char **argv)
{
    int rank, size, to, from;
    char send, recv;
    MPI_Status status;

    MPI_Init( &argc, &argv );
    MPI_Comm_size( MPI_COMM_WORLD, &size );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    to = rank + 1;
    if( to >= size ) {
        to = 0;
    }
    from = rank - 1;
    if( from < 0 ) {
        from = size - 1;
    }

    /* sending will block unless MPI buffers the message */
    send = 0;
    if( !rank ) {
        MPI_Request req;
        int flag;

        MPI_Isend( &send, 1, MPI_CHAR, to, 100, MPI_COMM_WORLD, &req );
        do {
            sleep(1);
            MPI_Test( &req, &flag, &status );
        } while( req != MPI_REQUEST_NULL );
    } else {
        MPI_Send( &send, 1, MPI_CHAR, to, 100, MPI_COMM_WORLD );
    }

    MPI_Recv( &recv, 1, MPI_CHAR, from, 100, MPI_COMM_WORLD, &status );

    MPI_Finalize( );

    return 0;
}
