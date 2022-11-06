
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

namespace Intel.VTuneAmplifierXE.Samples
{
    class SerialNQueens
    {
        private int nrOfSolutions = 0;  //keeps track of the number of solutions
        private int size = 0;  // the board-size read from command-line                 
        private int[] correctSolution; //array of the number of correct solutions for each board size

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

        public SerialNQueens()
        {
            correctSolution = new int[16];

            // Set the expected number of solutions for each board-size for later checking.
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
                nrOfSolutions++;  //Placed final queen, found a solution
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
            int[] queens = new int[size]; //array representing queens placed on a chess board.  Index is row position, value is column.

            for (int i = 0; i < size; i++)
            {
                // try all positions in first row	
                SetQueen(ref queens, 0, i);
            }
        }

        public static void Main(string[] args)
        {
            Stopwatch stopWatch = new Stopwatch();
            stopWatch.Start();

            SerialNQueens serialNQueensSolver = new SerialNQueens();
            if (args.Length < 1)
            {
                Console.WriteLine("Usage: serial_nqueens <boardSize> [default is 13].\n");
                serialNQueensSolver.Size = 13;
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
                serialNQueensSolver.Size = size;
            }

            Console.WriteLine("Starting nqueens (1_nqueens_serial) solver for size {0} ...\n", serialNQueensSolver.Size);

            serialNQueensSolver.Solve();

            Console.WriteLine("Number of solutions: {0}", serialNQueensSolver.NrOfSolutions);

            if (serialNQueensSolver.NrOfSolutions != serialNQueensSolver.CorrectSolutionSize)
            {
                Console.WriteLine("!!Incorrect result!! Number of solutions should be {0}\n", serialNQueensSolver.CorrectSolutionSize);
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