This file (example.cpp) is meant to be used with the "Introduction to Advisor XE 201x" and 
"Intel Advisor XE 201x on the Command Line + Automation" tutorials for Intel® Advisor XE 201x.
It is intended to be compiled with the Intel® C++ Compiler in Microsoft* Visual Studio 2010. 
The process may be slightly different using other compilers or in other environments.

To compile as intended, right click the project name and under "Intel Compiler", select "Use 
Intel C++", and perform the following steps.

1. On the tool bar, select "Build" and then select "Configuration Manager". Change the 
   project's configuration to "Release". If your system is a 64-bit System, change the 
	 project's Platform to "x64". If no such platform exists, select "<New...>" in the dropdown
	 that appeared when you clicked to change the platform, and select "x64" in the "New 
	 platform:" box. Select "Win32" for the "Copy settings from:" box. Check "Create new 
	 solution platforms" and hit "OK". Hit "OK" again to finish setting up the configuration.

2. Right click the project name in the solution explorer and select "Properties". Under 
   Configuration Properties>C/C++, ensure that you use and exclude the following flags as 
	 indicated.

		Compile WITH the following Flags:
	/Zi 					(Under >General>Debug Information Format, set to "Program Database")
	/O2						(Under >Optimization>Optimization, set to "Maximize Speed")
			(Type/copy all of the following into the "Additional Options" box under >Command Line,
			 separated by spaces.):
	/fp:fast 
	/Qopt-report5 
	/QxHOST 
	/Qopenmp 
	/debug:inline-debug-info 

		Compile WITHOUT the following flags:
	/Qi						(Under Optimization>Enable Intrinsic Functions, set to "No")
	/Qip or /Qipo	(Under Optimization [Intel C++]>Interprocedural Optimization, set to "No")
	/Qparallel		(Under Optimization [Intel C++]>Parallelization, set to "No")

3. When all flags are set, select "Build" on the tool bar, and select "Build Solution" or 
	 "Build Vectorization_Advisor".

See the online video: https://software.intel.com/en-us/videos/introduction-to-intel-advisor-xe-2016
