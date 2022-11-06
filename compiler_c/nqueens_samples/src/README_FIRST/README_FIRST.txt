Important information for the Intel(R) C++ Compiler nqueens sample

The NQueens-comp solution contains six MFC applications that can run independently. 
All six applications use the same common graphical user interface (GUI) created with the MFC 
application wizard and internally each application calls a different nqueens solver to 
solve the same problem. 

nq-serial: in this project the nqueens solver is serial, so it tends to be the slowest
  
  This project can be built with both Intel C++ or Microsoft Visual C++*. 
  
nq-win32api: in this project the nqueens solver uses Win32 threads and performance
  is better. The number of threads to be used is hard-coded, so it is not as easy to scale
  
  This project can be built with both Intel C++ or Visual C++. 

nq-tbb-intel: in this project the nqueens solver uses Intel(R) Threading Building Blocks (Intel(R) TBB)
  and both performance and scalability are much better.  

  This project shows you how to use the Intel TBB "parallel_for". 
  
  This project can be built with both Intel C++ or Visual C++. 

nq-tbb-lambda: in this project the nqueens solver uses Intel TBB with lambda functions and 
  the performance and scalability are similar to nq-tbb-intel.  

  This project shows you how to use the Intel TBB "parallel_for" with lambda code. 
  
  This project can be built with both Intel C++ or Visual C++ 2010* or 2012*.
  lambda is not supported by Visual C++ 2008*.

Intel is trademark of Intel Corporation in the U.S. and other countries.
* Other names and brands may be claimed as the property of others.