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
#include <stdio.h>

/**
 * This program contains a potential deadlock. The communication
 * pattern is as follows:
 *
 * Process 0               Process 1
 * MPI_Buffer_attach()     
 * MPI_Bsend()
 * MPI_Buffer_detach()
 * MPI_Barrier()           MPI_Barrier()
 *                         MPI_Recv()
 *
 * MPI_Buffer_detach() will block until the message is
 * transferred into the network. There is no guarantee that
 * this is possible because the recipient is not entering
 * its MPI_Recv() until after the barrier.
 */

int main( int argc, char **argv )
{
    /**
     * The buffer size is considerably larger than the message which is to be sent,
     * but allocating it dynamically based on MPI_Pack_size() results would be
     * cleaner.
     */
    int buffer[1024], exch[100];
    int size, rank;

    MPI_Init( &argc, &argv );

    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    
    if (size < 2) {
        printf("Size should be at least 2\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
        return 1;
    }
    
    if(rank == 0) {
        int retsize;

        /* Buffered send and detach buffer */
        MPI_Buffer_attach(buffer, sizeof(buffer));
        MPI_Bsend(exch, 10, MPI_INT, 1, 20, MPI_COMM_WORLD);
        MPI_Buffer_detach(&buffer, &retsize);
        
        MPI_Barrier(MPI_COMM_WORLD);
    } else if (rank == 1) {
        /* Call MPI_Recv after barrier */
        MPI_Status status;
        MPI_Barrier(MPI_COMM_WORLD);
        MPI_Recv(exch, 10, MPI_INT, 0, 20, MPI_COMM_WORLD, &status);
    } else {
        /* All others wait in barrier */
        MPI_Barrier(MPI_COMM_WORLD);
    }

    MPI_Finalize();

    return 0;
}

