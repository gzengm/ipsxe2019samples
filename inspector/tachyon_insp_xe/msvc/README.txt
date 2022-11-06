------------------------------------------
Intel(R) Inspector 
tachyon_insp_xe Application
README
------------------------------------------

Introduction
------------------------------------------

This is the README file for the tachyon_insp_xe sample application written using the C++ programming language. It explains how to open, build, and run this sample application on either the Microsoft Windows* OS or the Linux* OS using the Intel(R) Inspector XE and the Microsoft Visual Studio* integrated development environment (IDE) or
the standalone Inspector GUI.


Acquiring the Sample Application
------------------------------------------

Copy the tachyon_insp_xe.zip or tachyon_insp_xe.tar from the <INSTALL_DIR>\samples\en\C++ directory (where <INSTALL_DIR> is the location where you installed the Intel(R) Inspector XE) to a writable directory or share on your system. Extract the sample.


Building the samples
------------------------------------------

Use the available .sln file or Makefile to build.



Running a Threading Error Analysis
------------------------------------------

Build the debug version of the threading sample, find_and_fix_threading_errors.

Click the blue arrow on the Intel Inspector toolbar to configure a new analysis. Choose Detect Deadlocks and Data Races 

and press the Start button to begin analysis.

After the run you will see two errors, one data race in find_and_fix_threading_errors.cpp and one in winvideo.h. Double click the  

data race in find_and_fix_memory_errors.cpp to examine the source code.

Looking at the source code, you will see comments explaining the race. There is no lock protecting the global variable col. Make the 

suggested changes to make col a local variable. Rebuild and rerun the analysis. The issue will no longer be found.




Running a Memory Error Analysis
------------------------------------------

Build the debug version of the memory sample, find_and_fix_memory_errors.

Click the blue arrow on the Intel Inspector toolbar to configure a new analysis. Choose Detect Memory Problems and press 

the Start button to begin analysis.

After the run you will see five errors. Double click the Mismatched allocation/deallocation problem to examine the source code.

Looking at the source code, you will see that drawing object was allocated using new and deallocated useing free. Follow the comment

in the source code to change to the correct deallocator. Rebuild and rerun the analysis. The issue will no longer be found.




Conclusion
------------------------------------------

You have successfully performed threading and memory analysis using the Intel Inspector. 

Feel free to experiment with the tool using your own programs.

Please contact us if you have any feedback or questions at http://software.intel.com/en-us/articles/intel-software-developer-support/


Legal Information
------------------------------------------

INFORMATION IN THIS DOCUMENT IS PROVIDED IN CONNECTION WITH INTEL PRODUCTS. NO LICENSE, EXPRESS OR IMPLIED, BY ESTOPPEL OR OTHERWISE, TO ANY INTELLECTUAL PROPERTY RIGHTS IS GRANTED BY THIS DOCUMENT. EXCEPT AS PROVIDED IN INTEL'S TERMS AND CONDITIONS OF SALE FOR SUCH PRODUCTS, INTEL ASSUMES NO LIABILITY WHATSOEVER, AND INTEL DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY, RELATING TO SALE AND/OR USE OF INTEL PRODUCTS INCLUDING LIABILITY OR WARRANTIES RELATING TO FITNESS FOR A PARTICULAR PURPOSE, MERCHANTABILITY, OR INFRINGEMENT OF ANY PATENT, COPYRIGHT OR OTHER INTELLECTUAL PROPERTY RIGHT. 

UNLESS OTHERWISE AGREED IN WRITING BY INTEL, THE INTEL PRODUCTS ARE NOT DESIGNED NOR INTENDED FOR ANY APPLICATION IN WHICH THE FAILURE OF THE INTEL PRODUCT COULD CREATE A SITUATION WHERE PERSONAL INJURY OR DEATH MAY OCCUR. 

Intel may make changes to specifications and product descriptions at any time, without notice. Designers must not rely on the absence or characteristics of any features or instructions marked "reserved" or "undefined." Intel reserves these for future definition and shall have no responsibility whatsoever for conflicts or incompatibilities arising from future changes to them. The information here is subject to change without notice. Do not finalize a design with this information. 

The products described in this document may contain design defects or errors known as errata which may cause the product to deviate from published specifications. Current characterized errata are available on request. Contact your local Intel sales office or your distributor to obtain the latest specifications and before placing your product order. Copies of documents which have an order number and are referenced in this document, or other Intel literature, may be obtained by calling 1-800-548-4725, or go to: http://www.intel.com/design/literature.htm 

Intel processor numbers are not a measure of performance. Processor numbers differentiate features within each processor family, not across different processor families. Go to: http://www.intel.com/products/processor_number/

BlueMoon, BunnyPeople, Celeron, Celeron Inside, Centrino, Centrino Inside, Cilk, Core Inside, E-GOLD, i960, Intel, the Intel logo, Intel AppUp, Intel Atom, Intel Atom Inside, Intel Core, Intel Inside, Intel Insider, the Intel Inside logo, Intel NetBurst, Intel NetMerge, Intel NetStructure, Intel SingleDriver, Intel SpeedStep, Intel Sponsors of Tomorrow., the Intel Sponsors of Tomorrow. logo, Intel StrataFlash, Intel vPro, Intel XScale, InTru, the InTru logo, the InTru Inside logo, InTru soundmark, Itanium, Itanium Inside, MCS, MMX, Moblin, Pentium, Pentium Inside, Puma, skoool, the skoool logo, SMARTi, Sound Mark, The Creators Project, The Journey Inside, Thunderbolt, Ultrabook, vPro Inside, VTune, Xeon, Xeon Inside, X-GOLD, XMM, X-PMU and XPOSYS are trademarks of Intel Corporation in the U.S. and/or other countries.

*Other names and brands may be claimed as the property of others.

Microsoft, Windows, Visual Studio, Visual C++, and the Windows logo are trademarks, or registered trademarks of Microsoft Corporation in the United States and/or other countries. 

Copyright (C) 2013, Intel Corporation. All rights reserved. 
