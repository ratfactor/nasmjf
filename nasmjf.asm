; Dave's NASM port of JONESFORTH
; 
; register use:
;       esi - next forth word address to execute
;       ebp - return stack for forth word addresses
;       esp - "normal" stack for params


; +----------------------------------------------------------------------------+
; | The NEXT Macro                                                             |
; +----------------------------------------------------------------------------+
; Jones calls this "indirect threaded code", in which words definitions are just
; lists of pointers to other words. NEXT loads and executes the next pointer.
%macro NEXT 0
    lodsd     ; NEXT: Load from memory into eax, inc esi to point to next word.
    jmp [eax] ; Jump to whatever code we're now pointing at.
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
return_stack_top: resb 4
var_S0:       resb 4
data_segment: resb 1024 

SECTION .data

cold_start: dd quit  ; we need a way to indirectly address the first word

SECTION .text

; +----------------------------------------------------------------------------+
; | Forth DOCOL implementation                                                 |
; +----------------------------------------------------------------------------+
; This is the "interpreter" word - it is used at the beginning of proper Forth
; words that are composed of other words (not machine code). It gets the esi
; register pointed at the first word address and starts the NEXT macro.
docol:
    PUSHRSP esi     ; docol: push esi on to the RSP return stack
    add eax, 4      ; eax points to docol (me!) in word definition. Go to next.
    mov esi, eax    ; Put the next word pointer into esi
    NEXT

; +----------------------------------------------------------------------------+
; +----------------------------------------------------------------------------+
global _start

_start:

    cld    ; Clear the "direction flag" which means the string instructions (such
           ; as LODSD) work in increment order instead of decrement...
    mov [var_S0], esp ; save the regular stack pointer (used for data) in FORTH var S0!

    mov ebp, return_stack_top ; Initialise the return stack pointer
                              ; LOL, I accidentally had this set to the "bottom" of
                              ; the stack instead and it grew up into my last word
                              ; definition - which took FOREVER to figure out!
    mov esi, cold_start       ; give next forth word to execute

    NEXT ; Run!

; "flags" for Forth word definitions
%assign F_IMMED 0x80
%assign F_HIDDEN 0x20
%assign F_LENMASK 0x1f

; link holds address of last word defined (to make linked list)
; must be %define rather than %assign or we'll run afoul assigning
; the name_label address below (
%define link 0           ; null link - beginning of the linked list

; Forth commands or instructions are called "words".
; There are two kinds of words defined in JonesForth: 
;   1. "code words" are pure machine language implementations
;   2. "data words" are defined as a series of pointers to words
; Both start with a pointer to a code word's instructions which can
; be executed. The differene is that a code word points to its own
; instructions and a data word points to a word called "docol", ("do
; the colon word") which loads and runs the individual words.

; in memory, the words look like this for examples "foo" and "bar":
;
;      Code Word                 Data Word
;   name_foo:                 name_bar:
;       (word link)               (word link)
;       (namelen + flags)         (namelen + flags)
;       "foo"                     "bar"
;   foo:                      bar:
;       (ptr to docol)            (ptr to code_bar)
;       (ptr to a word)       code_bar:
;       (ptr to a word)           (machine code)
;       (ptr to a word)           (machine code)
;       ...                       ...
;
; Data words are defined with the help of the DEFWORD macro.
; Code words are defined with the help of the DEFCODE macro.
; 

; The difference is that DEFWORD will start with the addr of the DOCOL
; interpreter and will be followed by pointers to other words.
; DEFCODE will be followed by pure ASM machine code.

%macro DEFWORD 4 ; 1=name 2=namelen 3=flags 4=label
        SECTION .data
        align 4

        global name_%4 ; name_<label>
        name_%4:
            dd link                ; the previous word's addr
            %define link name_%4   ; store this link addr for next time
            db %3 + %2   ; flags + namelen 
            db %1        ;name string
            align 4

         global %4 ; <label>
         %4:
             dd docol ; addr of the "interpreter" that will handle
                     ; the list of forth word addrs that will follow
%endmacro

%macro DEFCODE 4 ; 1=name 2=namelen 3=flags 4=label
        SECTION .data
        align 4

        ; name_<label>
        global name_%4
        name_%4:
            dd link                ; the previous word's addr
            %define link name_%4   ; store this link addr for next time
            db %3 + %2   ; flags + namelen 
            db %1        ;name string
            align 4

        ; <label>
        global %4
        %4:
            dd code_%4 ; addr of code_label, for the asm code that will follow
            align 4

        ; code_<label>
        SECTION .text
	global code_%4
        code_%4:
            ; Then whatever follows this macro output is the assembly
            ; code for the Forth word...
%endmacro

	DEFWORD "quit",4,0,quit
        dd gtfo
	;dw RZ,RSPSTORE	; R0 RSP!, clear the return stack
	;dw INTERPRET    ; interpret the next word
	;dw BRANCH,-8    ; and loop (indefinitely)

; just try to quit gracefully:
	DEFCODE "gtfo",4,0,gtfo
	mov ebx, 0    ; exit code
        mov eax, 1    ; exit syscall
	int 80h       ; call kernel
