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
 * Does a large MPI_Isend() with a dynamically allocated buffer, frees
 * it as soon as the peer has received the chunk and only then calls
 * MPI_Wait().
 *
 * This makes the memory inaccessible in MPI_Wait() but happens to
 * work with most MPI implementations, although it is incorrect. Because it is not
 * guaranteed that libc actually unmaps the memory chunk, some of the
 * bytes are modified before free() to trigger an illegal modification
 * error if the memory is still available.
 */

#include <mpi.h>
#include <stdlib.h>
#include <string.h>

int main (int argc, char **argv)
{
    int rank, size, peer;
    MPI_Request req;
    MPI_Status status;

    /* make this very large so that libc uses mmap() in malloc() and unmap() in free() */
    int buffersize = 1024 * 1024 * 100;
    char *send, *recv;

    MPI_Init( &argc, &argv );
    MPI_Comm_size( MPI_COMM_WORLD, &size );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    peer = rank ^ 1;

    send = malloc( buffersize );
    memset( send, 2, buffersize );
    recv = malloc( buffersize );

    if( peer < size ) {
        MPI_Isend( send, buffersize, MPI_CHAR, peer, 100, MPI_COMM_WORLD, &req );
        MPI_Recv( recv, buffersize, MPI_CHAR, peer, 100, MPI_COMM_WORLD, &status );
    }
    MPI_Barrier( MPI_COMM_WORLD );

    /*
     * modifying the send buffer will either trigger LOCAL:MEMORY:ILLEGAL_MODIFICATION
     * (if the memory is still accessible after the free()) or LOCAL:MEMORY:INACCESSIBLE
     * (if free() unmaps it)
     */
    send[0] = 255;
    free( send );
    free( recv );

    if( peer < size ) {
        MPI_Wait( &req, &status );
    }

    MPI_Finalize( );

    return 0;
}
