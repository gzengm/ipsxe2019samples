; ==============================================================
; 
; SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
; http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
; 
; Copyright 2017 Intel Corporation
; 
; THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
; NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
; PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
; 
; =============================================================

    .686P
    .XMM
    .model  flat

    PUBLIC  _split_proc
    PUBLIC  _dyn_hotspot1
    PUBLIC  _dyn_hotspot2
    PUBLIC  _g_split_proc_size
    PUBLIC  _g_dyn_hotspot1_size
    PUBLIC  _g_dyn_hotspot2_size

    ;The function that demonstrates split function with several code ranges
    ;void split_proc(void** parray, unsigned size)
    ;{
    ;   unsigned i = 0;
    ;   for (i = 0; i < size; i++)
    ;   {
    ;       ((FNHOTSPOT)parray[i])();
    ;   }
    ;}

    _TEXT   SEGMENT
        _i$ = -4            ; size = 4
        _parray$ = 8        ; size = 4
        _size$ = 12         ; size = 4

    _split_proc PROC
    split_proc_begin LABEL DWORD
        push    ebp
        mov     ebp, esp
        push    ecx
        mov     DWORD PTR _i$[ebp], 0
        mov     DWORD PTR _i$[ebp], 0
        jmp     SHORT $LN3@split_hots
        $LN2@split_hots:
        mov     eax, DWORD PTR _i$[ebp]
        add     eax, 1
        mov     DWORD PTR _i$[ebp], eax
        $LN3@split_hots:
        mov     ecx, DWORD PTR _i$[ebp]
        cmp     ecx, DWORD PTR _size$[ebp]
        jae     SHORT $LN4@split_hots
        mov     edx, DWORD PTR _i$[ebp]
        mov     eax, DWORD PTR _parray$[ebp]
        mov     ecx, DWORD PTR [eax+edx*4]
        call    ecx
        jmp     SHORT $LN2@split_hots
        $LN4@split_hots:
        mov     esp, ebp
        pop     ebp
        ret     0
    split_proc_end LABEL DWORD
    _split_proc ENDP

    ;
    ;void dyn_hotspot1()
    ;{
    ;    double x = 0;
    ;    int i = 0;
    ;    int j = 0;
    ;    for (i = 0; i < 100000; i++)
    ;    {
    ;        for (j = 0; j < 10000; j++)
    ;        {
    ;            x+= i*j;
    ;        }
    ;    }
    ;}

    _TEXT   SEGMENT

    tv75 = -84          ; size = 4
    _j$ = -16           ; size = 4
    _i$ = -12           ; size = 4
    _x$ = -8            ; size = 8

    _dyn_hotspot1 PROC
    dyn_hotspot1_begin LABEL DWORD
        push    ebp
        mov     ebp, esp
        sub     esp, 84
        push    ebx
        push    esi
        push    edi
        fldz
        fstp    QWORD PTR _x$[ebp]
        mov     DWORD PTR _i$[ebp], 0
        mov     DWORD PTR _j$[ebp], 0
        mov     DWORD PTR _i$[ebp], 0
        jmp     SHORT $LN6@dyn_hotspot1
        $LN5@dyn_hotspot1:
        mov     eax, DWORD PTR _i$[ebp]
        add     eax, 1
        mov     DWORD PTR _i$[ebp], eax
        $LN6@dyn_hotspot1:
        cmp     DWORD PTR _i$[ebp], 100000
        jge     SHORT $LN4@dyn_hotspot1
        mov     DWORD PTR _j$[ebp], 0
        jmp     SHORT $LN3@dyn_hotspot1
        $LN2@dyn_hotspot1:
        mov     eax, DWORD PTR _j$[ebp]
        add     eax, 1
        mov     DWORD PTR _j$[ebp], eax
        $LN3@dyn_hotspot1:
        cmp     DWORD PTR _j$[ebp], 10000
        jge     SHORT $LN1@dyn_hotspot1
        mov     eax, DWORD PTR _i$[ebp]
        imul    eax, DWORD PTR _j$[ebp]
        mov     DWORD PTR tv75[ebp], eax
        fild    DWORD PTR tv75[ebp]
        fadd    QWORD PTR _x$[ebp]
        fstp    QWORD PTR _x$[ebp]
        jmp     SHORT $LN2@dyn_hotspot1
        $LN1@dyn_hotspot1:
        jmp     SHORT $LN5@dyn_hotspot1
        $LN4@dyn_hotspot1:
        pop     edi
        pop     esi
        pop     ebx
        mov     esp, ebp
        pop     ebp
        ret     0
    dyn_hotspot1_end LABEL DWORD
    _dyn_hotspot1 ENDP

    _TEXT   ENDS

    ;
    ;The function that does time consuming operation to get samples
    ;Note that size of the dyn_hotspot2 function must be less then
    ;the dyn_hotspot1 function*/
    ;void dyn_hotspot2()
    ;{
    ;    double x = 0;
    ;    int i = 0;
    ;    for (i = 0; i < 100000000; i++)
    ;    {
    ;        x += i + i;
    ;    }
    ;}

    _TEXT   SEGMENT

    tv72 = -16          ; size = 4
    _i$ = -12           ; size = 4
    _x$ = -8            ; size = 8

    _dyn_hotspot2 PROC
    dyn_hotspot2_begin LABEL DWORD
        push    ebp
        mov     ebp, esp
        sub     esp, 16
        fldz
        fstp    QWORD PTR _x$[ebp]
        mov     DWORD PTR _i$[ebp], 0
        mov     DWORD PTR _i$[ebp], 0
        jmp     SHORT $LN3@dyn_hotspot2
        $LN2@dyn_hotspot2:
        mov     eax, DWORD PTR _i$[ebp]
        add     eax, 1
        mov     DWORD PTR _i$[ebp], eax
        $LN3@dyn_hotspot2:
        cmp     DWORD PTR _i$[ebp], 100000000
        jge     SHORT $LN1@dyn_hotspot2
        mov     ecx, DWORD PTR _i$[ebp]
        add     ecx, DWORD PTR _i$[ebp]
        mov     DWORD PTR tv72[ebp], ecx
        fild    DWORD PTR tv72[ebp]
        fadd    QWORD PTR _x$[ebp]
        fstp    QWORD PTR _x$[ebp]
        jmp     SHORT $LN2@dyn_hotspot2
        $LN1@dyn_hotspot2:
        mov     esp, ebp
        pop     ebp
        ret     0
    dyn_hotspot2_end LABEL DWORD
    _dyn_hotspot2 ENDP

    _TEXT   ENDS

    _DATA SEGMENT

    _g_split_proc_size DWORD (split_proc_end - split_proc_begin)
    _g_dyn_hotspot1_size DWORD (dyn_hotspot1_end - dyn_hotspot1_begin)
    _g_dyn_hotspot2_size DWORD (dyn_hotspot2_end - dyn_hotspot2_begin)

    _DATA ENDS

END
