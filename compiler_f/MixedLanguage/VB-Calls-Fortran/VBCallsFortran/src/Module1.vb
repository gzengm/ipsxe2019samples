'==============================================================
'
' SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE 
' LICENSE AGREEMENT,
' http://software.intel.com/en-us/articles/intel-sample-source-
' code-license-agreement/
'
' Copyright 2016 Intel Corporation
'
' THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR 
' IMPLIED, INCLUDING BUT NOT LIMITED TO ANY IMPLIED WARRANTY OF 
' MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, NON-
' INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
'
' =============================================================

Module Module1
    REM Use ByVal to pass strings unless the called routine expects BSTR structures
    Public Declare Auto Sub DLL_ROUT Lib "FCALL.DLL" _
   (ByVal DBL_IN() As Double, ByVal STR_IN As String, ByVal DBL_OUT() As Double)
End Module
