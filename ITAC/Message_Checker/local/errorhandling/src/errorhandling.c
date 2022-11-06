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

/**
 * This program triggers various warnings and errors. It can be used
 * with different configurations to test the error handling and
 * reporting. Interesting configurations, specified as environment
 * variables:
 * - one instance of each error at most
 *   VT_CHECK_SUPPRESSION_LIMIT=1
 * - unlimited number of instances
 *   VT_CHECK_SUPPRESSION_LIMIT=0
 * - abort at first report, regardless of what it is
 *   VT_CHECK_MAX_REPORTS=1
 * - keep running even when MPI calls fail
 *   VT_CHECK_MAX_ERRORS=0
 * - disable message type mismatch
 *   VT_CHECK="GLOBAL:MSG:DATATYPE:MISMATCH off"
 * - disable anything that requires extra messages
 *   VT_CHECK="GLOBAL:MSG:** off"
 */

#include <mpi.h>

int main (int argc, char **argv)
{
    int rank, size, peer;
    int i;
    char send = 0, recv;
    MPI_Status status, statuses[2];
    MPI_Request reqs[2], req;

    MPI_Init( &argc, &argv );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    MPI_Comm_size( MPI_COMM_WORLD, &size );
    peer = rank ^ 1;

    /* 20 overlap warnings */
    if( peer < size ) {
        for( i = 0; i < 20; i++ ) {
            if( rank & 1 ) {
                MPI_Recv( &recv, 1, MPI_CHAR, peer, 100, MPI_COMM_WORLD, &status );
                MPI_Recv( &recv, 1, MPI_CHAR, peer, 100, MPI_COMM_WORLD, &status );
            } else {
                MPI_Isend( &send, 1, MPI_CHAR, peer, 100, MPI_COMM_WORLD, reqs + 0 );
                MPI_Isend( &send, 1, MPI_CHAR, peer, 100, MPI_COMM_WORLD, reqs + 1 );
                MPI_Waitall( 2, reqs, statuses );
            }
        }
    }

    MPI_Barrier( MPI_COMM_WORLD );

    /* warning: free an invalid request */
    req = MPI_REQUEST_NULL;
    MPI_Request_free( &req );

    MPI_Barrier( MPI_COMM_WORLD );

    /* 15 errors: send to invalid recipient */
    for( i = 0; i < 15; i++ ) {
        MPI_Send( &send, 1, MPI_CHAR, -1, 200, MPI_COMM_WORLD );
    }

    MPI_Barrier( MPI_COMM_WORLD );

    /* datatype mismatch */
    if( peer < size ) {
        if( rank & 1 ) {
            int recvint;

            MPI_Recv( &recvint, 1, MPI_INT, peer, 300, MPI_COMM_WORLD, &status );
        } else {
            MPI_Send( &send, 1, MPI_CHAR, peer, 300, MPI_COMM_WORLD );
        }
    }

    MPI_Barrier( MPI_COMM_WORLD );

    /* collop mismatch */
    if( rank & 1 ) {
        MPI_Barrier( MPI_COMM_WORLD );
    } else {
        MPI_Reduce( &send, &recv, 1, MPI_BYTE, MPI_SUM, 0, MPI_COMM_WORLD );
    }

    MPI_Barrier( MPI_COMM_WORLD );

    /* root mismatch */
    MPI_Reduce( &send, &recv, 1, MPI_BYTE, MPI_SUM, rank ? 0 : 1, MPI_COMM_WORLD );

    MPI_Barrier( MPI_COMM_WORLD );

    /* create unfreed requests */
    if( peer < size ) {
        for( i = 0; i < 2; i++ ) {
            MPI_Send_init( &send, 1, MPI_CHAR, peer, 300, MPI_COMM_WORLD, &req );
        }
    }

    MPI_Finalize( );

    return 0;
}
