; Dave's NASM port of JONESFORTH
; 
; register use:
;       esi - next forth word address to execute
;       ebp - return stack for forth word addresses
;       esp - "normal" stack for params

; +----------------------------------------------------------------------------+
; | The NEXT Macro                                                             |
; +----------------------------------------------------------------------------+
; Jones calls this "indirect threaded code", which has a list of addresses
; to be loaded and executed. the NEXT macro does the loading and executing.

; This macro uses lodsd (load string, double) (GAS calls it `lodsl` for `long`)
; Which is basically equivalent to:
;     mov eax, [esi]
;     add esi, 4
; (Loads 4 byte value at address in esi into eax, increments esi)
; Then we jump to the address now stored in eax. So, clearly a valid
; machine code address needs to have been stored in the memory pointed to by
; esi!
; NOTE: still need to verify that JF wants to execute the code at that address
; and not yet ANOTHER indirection. If we need another indirection, we'll need
; jmp [eax] instead
;
%macro NEXT 0
    lodsd
    jmp eax
%endmacro

; +----------------------------------------------------------------------------+
; | Return stack PUSH/POP macros                                               |
; +----------------------------------------------------------------------------+
; - We will use the frame pointer (ebp) register for a FORTH word return stack.
; - The PUSHRSP and POPRSP macros handle pushing registers onto stack memory
;   managed by ebp - the Return Stack Pointer (RSP)
; - NASM macros specify a number of params and substitute 'em with %1, %2, etc.
;
%macro PUSHRSP 1
    lea ebp, [ebp-4]   ; "load effective address" of next stack position
    mov [ebp], %1      ; "push" the register value to the address at ebp
%endmacro
%macro POPRSP 1
    mov %1, [ebp]
    lea ebp, [ebp+4]
%endmacro

SECTION .bss
return_stack: resb 8192
var_S0:       resb 4
data_segment: resb 65535

SECTION .data

cold_start: db quit  ; store address of quit word in cold_start ...hopefully
quit:       db "foo" ; LOL, just seeing if any of this works

SECTION .text

; +----------------------------------------------------------------------------+
; | Forth DOCOL implementation                                                 |
; +----------------------------------------------------------------------------+
; It's easy to see what it does, but a better comment will go here when I figure
; out WHY.
docol:
    PUSHRSP esi     ; push current esi on to the return stack
    add eax, 4      ; eax points to code word after NEXT, so...
    mov esi, eax    ; ...adding 4 lets esi point to first data word
    NEXT            ; then I guess we call NEXT again.

; +----------------------------------------------------------------------------+
; +----------------------------------------------------------------------------+
global _start

_start:

    cld    ; Clear the "direction flag" which means the string instructions (such
           ; as LODSD) work in increment order instead of decrement...
    mov [var_S0], esp ; save the regular stack pointer (used for data) in FORTH var S0!

    mov ebp, return_stack ; Initialise the return stack pointer

    mov esi, cold_start ; give next forth word to execute

    NEXT ; Run!

