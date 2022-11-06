
//==============================================================
//
// SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
// http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
//
// Copyright 2017 Intel Corporation
//
// THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
// NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
// PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
//
// =============================================================
using System;
using System.Text;
using System.Diagnostics;
using System.Threading;
using System.Management;

namespace Intel.VTuneAmplifierXE.Samples
{
    class ThreadParams
    {
        private int startPosition;
        private int amountOfWork;

        public ThreadParams(int startPosition, int amountOfWork)
        {
            this.startPosition = startPosition;
            this.amountOfWork = amountOfWork;
        }

        public int StartPosition
        {
            get
            {
                return startPosition;
            }
            set
            {
                startPosition = value;
            }
        }

        public int AmountOfWork
        {
            get
            {
                return amountOfWork;
            }
            set
            {
                amountOfWork = value;
            }
        }
    }

    class ParallelNQueens
    {
        private int nrOfSolutions = 0;  //keeps track of the number of solutions
        private int size = 0;  // the board-size read from command-line                 
        private int[] correctSolution; //array of the number of correct solutions for each board size

        // Create a new Mutex. The creating thread does not own the Mutex.
        private static Mutex nrOfSolutionsMutex = new Mutex();

        public int NrOfSolutions
        {
            get
            {
                return nrOfSolutions;
            }
            set
            {
                nrOfSolutions = value;
            }
        }

        public int Size
        {
            get
            {
                return size;
            }
            set
            {
                size = value;
            }
        }

        public int CorrectSolutionSize
        {
            get
            {
                return correctSolution[size];
            }
        }

        public ParallelNQueens()
        {
            correctSolution = new int[16];

            /* Set the expected number of solutions for each board-size for later checking. */
            correctSolution[0] = 0;
            correctSolution[1] = 0;
            correctSolution[2] = 0;
            correctSolution[3] = 0;
            correctSolution[4] = 2;
            correctSolution[5] = 10;
            correctSolution[6] = 4;
            correctSolution[7] = 40;
            correctSolution[8] = 92;
            correctSolution[9] = 352;
            correctSolution[10] = 724;
            correctSolution[11] = 2680;
            correctSolution[12] = 14200;
            correctSolution[13] = 73712;
            correctSolution[14] = 365596;
            correctSolution[15] = 2279184;
        }

        /*
            Recursive function to find all solutions on a board 
            represented by the argument "queens", placing the next queen
            at location (row, col)

            On Return: nrOfSolutions has been increased to add solutions for this board
        */
        public void SetQueen(ref int[] queens, int row, int col)
        {
            //check all previously placed rows for attacks
            for (int i = 0; i < row; i++)
            {
                // vertical attacks
                if (queens[i] == col)
                {
                    return;
                }
                // diagonal attacks
                if (Math.Abs(queens[i] - col) == (row - i))
                {
                    return;
                }
            }
            // column is ok, set the queen
            queens[row] = col;

            if (row == size - 1)
            {
                // increment is not atomic, so setting a lock is required here                  
                nrOfSolutionsMutex.WaitOne();
                nrOfSolutions++;  //Placed final queen, found a solution
                nrOfSolutionsMutex.ReleaseMutex();
            }
            else
            {
                // try to fill next row
                for (int i = 0; i < size; i++)
                {
                    SetQueen(ref queens, row + 1, i);
                }
            }
        }

        /*
            Function to find all solutions for nQueens problem on size x size chessboard.

            On Return: nrOfSoultions = number of solutions for size x size chessboard.
        */
        public void Solve()
        {
            /* Determine cores */
            int coreCount = 0;
            coreCount = Environment.ProcessorCount;
            Console.WriteLine("Number Of Cores: {0}", coreCount);

            coreCount = (coreCount > size) ? size : coreCount;

            Thread[] threads = new Thread[coreCount];
            Console.WriteLine("Number of threads created: {0}", coreCount);

            /* Test whether board size is a multiple of the available number of cores */
            int residue = 0;
            int chunkSize = 0;
            if ((this.Size % coreCount) != 0)
            {
                residue = this.Size % coreCount;
                chunkSize = (this.Size - residue) / coreCount;
            }
            else
            {
                residue = 0;
                chunkSize = this.Size / coreCount;
            }

            int previousEndPos = 0;
            for (int i = 0; i < coreCount; i++)
            {
                threads[i] = new Thread(new ParameterizedThreadStart(ThreadFunc));

                ThreadParams parameters;
                if (residue > 0)
                {
                    parameters = new ThreadParams(previousEndPos, chunkSize + 1);
                    previousEndPos = previousEndPos + chunkSize + 1;
                    residue--;
                }
                else
                {
                    parameters = new ThreadParams(previousEndPos, chunkSize);
                    previousEndPos = previousEndPos + chunkSize;
                }
                threads[i].Start(parameters);
            }

            for (int i = 0; i < coreCount; i++)
            {
                threads[i].Join();
            }

            /*              
                Parallel.For(0, size, i =>
                {                
                    int[] queens = new int[size]; 
                    setQueen(ref queens, 0, i);
                }); // Parallel.For which is part of TPL if you are using .NET framework 4
             */
        }

        public void ThreadFunc(Object parameters)
        {
            ThreadParams threadParameters = (ThreadParams)parameters;
            int startPosition = threadParameters.StartPosition;
            int amountOfWork = threadParameters.AmountOfWork;

            int[] queens = new int[size];
            for (int i = startPosition; i < (startPosition + amountOfWork); i++)
            {
                SetQueen(ref queens, 0, i);
            }
        }

        public static void Main(string[] args)
        {
            Stopwatch stopWatch = new Stopwatch();
            stopWatch.Start();

            ParallelNQueens parallelNqueensSolver = new ParallelNQueens();
            if (args.Length < 1)
            {
                Console.WriteLine("Usage: parallel_nqueens <boardSize> [default is 13].\n");
                parallelNqueensSolver.Size = 13;
            }
            else
            {
                int size = 13;
                try
                {
                    size = Convert.ToInt32(args[0]);
                }
                catch (FormatException e)
                {
                    Console.WriteLine("Input string is not a sequence of digits. Setting it to 13. \n");
                    size = 13;
                }
                catch (OverflowException e)
                {
                    Console.WriteLine("The number cannot fit in an Int32. Setting it to 13. \n");
                    size = 13;
                }
                finally
                {
                    if (size < 4 || size > 15)
                    {
                        Console.WriteLine("Boardsize should be between 4 and 15; Setting it to 13. \n");
                        size = 13;
                    }
                }
                parallelNqueensSolver.Size = size;
            }

            Console.WriteLine("Starting nqueens (1_nqueens_parallel) solver for size {0} ...\n", parallelNqueensSolver.Size);

            parallelNqueensSolver.Solve();

            Console.WriteLine("Number of solutions: {0}", parallelNqueensSolver.NrOfSolutions);

            if (parallelNqueensSolver.NrOfSolutions != parallelNqueensSolver.CorrectSolutionSize)
            {
                Console.WriteLine("!!Incorrect result!! Number of solutions should be {0}\n", parallelNqueensSolver.CorrectSolutionSize);
            }
            else
            {
                Console.WriteLine("Correct result!\n");
            }

            stopWatch.Stop();
            Console.WriteLine("Calculations took {0} minutes, {1} seconds, {2} milliseconds\n", stopWatch.Elapsed.Minutes, stopWatch.Elapsed.Seconds, stopWatch.Elapsed.Milliseconds);

            return;
        }
    }
}