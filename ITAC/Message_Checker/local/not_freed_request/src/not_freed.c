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

/**
 * This example covers the request leak reporting. It is expected to be run with
 * one process and CHECK-MAX-REQUESTS 4.
 */

int main (int argc, char **argv)
{
    int i, e;
    char buffer[1024];
    int offset = 0;
    MPI_Request req;
    MPI_Request reqs[4];

    MPI_Init( &argc, &argv );
    for( i = 0; i < sizeof(buffer)/sizeof(buffer[0]); i++ ) {
        buffer[i] = (char)i;
    }

    /* generate four requests - should trigger a leak report */
    for( i = 0; i < 4; i++ ) {
        MPI_Send_init( buffer + (offset++), 1, MPI_CHAR, 0, 0, MPI_COMM_SELF, reqs + i );
    }
    /* now free one and create it again - should not trigger a report */
    MPI_Request_free( reqs + 3 );
    MPI_Send_init( buffer + (offset++), 1, MPI_CHAR, 0, 1, MPI_COMM_SELF, reqs + 3 );

    /* leak one request in various functions, all with different source/line */
    MPI_Send_init( buffer + (offset++), 1, MPI_CHAR, 0, 100, MPI_COMM_SELF, &req );
    MPI_Recv_init( buffer + (offset++), 1, MPI_CHAR, 0, 101, MPI_COMM_SELF, &req );
    MPI_Isend( buffer + (offset++), 1, MPI_CHAR, 0, 102, MPI_COMM_SELF, &req );
    MPI_Irecv( buffer + (offset++), 1, MPI_CHAR, 0, 104, MPI_COMM_SELF, &req );

    /* leak with different datatypes (should be merged despite the different numerical handles) */
    for( i = 0; i < 2; i++ ) {
        MPI_Datatype type;

        MPI_Type_vector( 1, i+2, i+2, MPI_CHAR, &type );
        MPI_Type_commit( &type );
        MPI_Isend( buffer + offset, 1, type, 0, 200, MPI_COMM_SELF, &req );
        offset += i + 2;
        /*
         * also leak the type to ensure that ids are different in each loop iteration
         * MPI_Type_free( &type );
         */
    }

    /* now do the same in a loop */
    for( i = 0; i < 5; i++ ) {
        MPI_Send_init( buffer + (offset++), 1, MPI_CHAR, 0, 300, MPI_COMM_SELF, &req );
        MPI_Recv_init( buffer + (offset++), 1, MPI_CHAR, 0, 301, MPI_COMM_SELF, &req );
        MPI_Isend( buffer + (offset++), 1, MPI_CHAR, 0, 302, MPI_COMM_SELF, &req );
        for( e = 0; e < 2; e++ ) {
            MPI_Irecv( buffer + (offset++), 1, MPI_CHAR, 0, 304, MPI_COMM_SELF, &req );
        }
    }

    /* leak an active request */
    MPI_Isend( buffer + (offset++), 1, MPI_CHAR, 0, 400, MPI_COMM_SELF, &req );
    MPI_Request_free( &req );

    MPI_Finalize( );

    return 0;
}
