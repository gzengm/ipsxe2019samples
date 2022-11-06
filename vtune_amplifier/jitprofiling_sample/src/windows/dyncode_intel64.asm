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

    .listall

    PUBLIC split_proc
    PUBLIC g_split_proc_size
    PUBLIC dyn_hotspot1
    PUBLIC g_dyn_hotspot1_size
    PUBLIC dyn_hotspot2
    PUBLIC g_dyn_hotspot2_size

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
        i$ = 32
        parray$ = 64
        size$ = 72

    split_proc PROC
        split_proc_begin LABEL DWORD
        $LN6:
        mov     DWORD PTR [rsp+16], edx
        mov     QWORD PTR [rsp+8], rcx
        sub     rsp, 56
        mov     DWORD PTR i$[rsp], 0
        mov     DWORD PTR i$[rsp], 0
        jmp     SHORT $LN3@split_proc
        $LN2@split_proc:
        mov     eax, DWORD PTR i$[rsp]
        add     eax, 1
        mov     DWORD PTR i$[rsp], eax
        $LN3@split_proc:
        mov     eax, DWORD PTR size$[rsp]
        cmp     DWORD PTR i$[rsp], eax
        jae     SHORT $LN1@split_proc
        mov     ecx, DWORD PTR i$[rsp]
        mov     rax, QWORD PTR parray$[rsp]
        call    QWORD PTR [rax+rcx*8]
        jmp     SHORT $LN2@split_proc
        $LN1@split_proc:
        add     rsp, 56
        ret     0
        split_proc_end LABEL DWORD
    split_proc ENDP
    _TEXT   ENDS

    ;void dyn_hotspot1()
    ;{
    ;    int i = 0;
    ;    int j = 0;
    ;    int result = 0;
    ;
    ;    for (i = 0; i < 10000; i++)
    ;    {
    ;        for (j = 0; j < 100000; j++)
    ;        {
    ;            result = i + j;
    ;        }
    ;    }
    ;}

    _TEXT	SEGMENT
        i$ = 0
        j$ = 4
        result$ = 8

    dyn_hotspot1 PROC
        dyn_hotspot1_begin LABEL DWORD
        $LN9:
        push    rdi
        sub     rsp, 16
        mov     rdi, rsp
        mov     rcx, 4
        mov     eax, -858993460
        rep     stosd
        mov     DWORD PTR i$[rsp], 0
        mov     DWORD PTR j$[rsp], 0
        mov     DWORD PTR result$[rsp], 0
        mov     DWORD PTR i$[rsp], 0
        jmp     SHORT $LN6@dyn_hotspo@2
        $LN5@dyn_hotspo@2:
        mov     eax, DWORD PTR i$[rsp]
        add     eax, 1
        mov     DWORD PTR i$[rsp], eax
        $LN6@dyn_hotspo@2:
        cmp     DWORD PTR i$[rsp], 10000
        jge     SHORT $LN4@dyn_hotspo@2
        mov     DWORD PTR j$[rsp], 0
        jmp     SHORT $LN3@dyn_hotspo@2
        $LN2@dyn_hotspo@2:
        mov     eax, DWORD PTR j$[rsp]
        add     eax, 1
        mov     DWORD PTR j$[rsp], eax
        $LN3@dyn_hotspo@2:
        cmp     DWORD PTR j$[rsp], 100000
        jge     SHORT $LN1@dyn_hotspo@2
        mov     ecx, DWORD PTR j$[rsp]
        mov     eax, DWORD PTR i$[rsp]
        add     eax, ecx
        mov     DWORD PTR result$[rsp], eax
        jmp     SHORT $LN2@dyn_hotspo@2
        $LN1@dyn_hotspo@2:
        jmp     SHORT $LN5@dyn_hotspo@2
        $LN4@dyn_hotspo@2:
        add     rsp, 16
        pop     rdi
        ret     0
        dyn_hotspot1_end LABEL DWORD
    dyn_hotspot1 ENDP
    _TEXT   ENDS

    ;void dyn_hotspot2()
    ;{
    ;    int i = 0;
    ;    int result = 0;
    ;
    ;    for (i = 0; i < 100000000; i++)
    ;    {
    ;           result = i + i;
    ;    }
    ;}
    _TEXT   SEGMENT
        i$ = 0
        result$ = 4

    dyn_hotspot2 PROC
        dyn_hotspot2_begin LABEL DWORD
        $LN6:
        push    rdi
        sub     rsp, 16
        mov     rdi, rsp
        mov     rcx, 4
        mov     eax, -858993460
        rep     stosd
        mov     DWORD PTR i$[rsp], 0
        mov     DWORD PTR result$[rsp], 0
        mov     DWORD PTR i$[rsp], 0
        jmp SHORT $LN3@dyn_hotspo
        $LN2@dyn_hotspo:
        mov     eax, DWORD PTR i$[rsp]
        add     eax, 1
        mov     DWORD PTR i$[rsp], eax
        $LN3@dyn_hotspo:
        cmp     DWORD PTR i$[rsp], 100000000
        jge     SHORT $LN1@dyn_hotspo
        mov     ecx, DWORD PTR i$[rsp]
        mov     eax, DWORD PTR i$[rsp]
        add     eax, ecx
        mov     DWORD PTR result$[rsp], eax
        jmp     SHORT $LN2@dyn_hotspo
        $LN1@dyn_hotspo:
        add     rsp, 16
        pop     rdi
        ret     0
        dyn_hotspot2_end LABEL DWORD
    dyn_hotspot2 ENDP
    _TEXT   ENDS

    _DATA SEGMENT

    g_split_proc_size DWORD (split_proc_end - split_proc_begin)
    g_dyn_hotspot1_size DWORD (dyn_hotspot1_end - dyn_hotspot1_begin)
    g_dyn_hotspot2_size DWORD (dyn_hotspot2_end - dyn_hotspot2_begin)

    _DATA ENDS

    END
