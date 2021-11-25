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
%define buffer_size 4096
return_stack: resb 8192
return_stack_top: resb 4
var_S0:       resb 4
data_segment: resb 1024 
buffer:       resb buffer_size

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

        ; quit is a bit of a silly name - it's more of a reset
	DEFWORD "quit",4,0,quit
        dd R0           ; push R0 (addr of top of return stack)
        dd RSPSTORE     ; store R0 in return stack pointer (ebp)
	dd INTERPRET    ; interpret the next word
	;dd BRANCH,-8    ; and loop (indefinitely)

        dd gtfo

        ; just try to quit gracefully:
	DEFCODE "gtfo",4,0,gtfo
	mov ebx, 0    ; exit code
        mov eax, 1    ; exit syscall
	int 80h       ; call kernel


; +----------------------------------------------------------------------------+
; Forth constants:
;
;  VERSION		Is the current version of this FORTH.
;  R0		The address of the top of the return stack.
;  DOCOL		Pointer to DOCOL.
;  F_IMMED		The IMMEDIATE flag's actual value.
;  F_HIDDEN	The HIDDEN flag's actual value.
;  F_LENMASK	The length mask in the flags/len byte.
;  SYS_*		and the numeric codes of various Linux syscalls (from <asm/unistd.h>)

%assign NASMJF_VERSION 0x001

; JONESFORTH gets the system call numbers by including asm/unistd.h.
; But I'm just gonna hardcode them here. Found in Linux source
; in file arch/x86/include/asm/unistd_32.h
%assign __NR_exit  1
%assign __NR_open  5
%assign __NR_close 6
%assign __NR_read  3
%assign __NR_write 4
%assign __NR_creat 8
%assign __NR_brk   45

; Check it out! A const is just a word that pushes a value!
%macro DEFCONST 5 ; 1=name 2=namelen 3=flags 4=label 5=value
        DEFCODE %1,%2,%3,%4
        push %5
        NEXT
%endmacro

    DEFCONST "VERSION",7,,VERSION,NASMJF_VERSION
    DEFCONST "R0",2,,R0,return_stack_top
    DEFCONST "DOCOL",5,,__DOCOL,docol
    DEFCONST "F_IMMED",7,,__F_IMMED,F_IMMED
    DEFCONST "F_HIDDEN",8,,__F_HIDDEN,F_HIDDEN
    DEFCONST "F_LENMASK",9,,__F_LENMASK,F_LENMASK

    DEFCONST "SYS_EXIT",8,,SYS_EXIT,__NR_exit
    DEFCONST "SYS_OPEN",8,,SYS_OPEN,__NR_open
    DEFCONST "SYS_CLOSE",9,,SYS_CLOSE,__NR_close
    DEFCONST "SYS_READ",8,,SYS_READ,__NR_read
    DEFCONST "SYS_WRITE",9,,SYS_WRITE,__NR_write
    DEFCONST "SYS_CREAT",9,,SYS_CREAT,__NR_creat
    DEFCONST "SYS_BRK",7,,SYS_BRK,__NR_brk

    DEFCONST "O_RDONLY",8,,__O_RDONLY,0
    DEFCONST "O_WRONLY",8,,__O_WRONLY,1
    DEFCONST "O_RDWR",6,,__O_RDWR,2
    DEFCONST "O_CREAT",7,,__O_CREAT,0100
    DEFCONST "O_EXCL",6,,__O_EXCL,0200
    DEFCONST "O_TRUNC",7,,__O_TRUNC,01000
    DEFCONST "O_APPEND",8,,__O_APPEND,02000
    DEFCONST "O_NONBLOCK",10,,__O_NONBLOCK,04000


; ============================================================
; Words in ASM

        ; return stack pointer
	DEFCODE "RSP!",4,,RSPSTORE
	pop ebp
	NEXT


        ;
        ; * * *   The Forth interpreter!   * * *
        ;
	DEFCODE "INTERPRET",9,,INTERPRET
	call _WORD              ; Returns %ecx = length, %edi = pointer to word.

	;// Is it in the dictionary?
	;xor %eax,%eax
	;movl %eax,interpret_is_lit // Not a literal number (not yet anyway ...)
	;call _FIND		// Returns %eax = pointer to header or 0 if not found.
	;test %eax,%eax		// Found?
	;jz 1f

	;// In the dictionary.  Is it an IMMEDIATE codeword?
	;mov %eax,%edi		// %edi = dictionary entry
	;movb 4(%edi),%al	// Get name+flags.
	;push %ax		// Just save it for now.
	;call _TCFA		// Convert dictionary entry (in %edi) to codeword pointer.
	;pop %ax
	;andb $F_IMMED,%al	// Is IMMED flag set?
	;;mov %edi,%eax
	;jnz 4f			// If IMMED, jump straight to executing.

	;jmp 2f

;1:	;// Not in the dictionary (not a word) so assume it's a literal number.
	;incl interpret_is_lit
	;call _NUMBER		// Returns the parsed number in %eax, %ecx > 0 if error
	;test %ecx,%ecx
	;;jnz 6f
	;mov %eax,%ebx
	;mov $LIT,%eax		// The word is LIT

;2:	;// Are we compiling or executing?
	;movl var_STATE,%edx
	;test %edx,%edx
	;jz 4f			// Jump if executing.

	;// Compiling - just append the word to the current dictionary definition.
	;call _COMMA
	;mov interpret_is_lit,%ecx // Was it a literal?
	;test %ecx,%ecx
	;jz 3f
	;mov %ebx,%eax		// Yes, so LIT is followed by a number.
	;call _COMMA
;3:	;NEXT

;4:	;// Executing - run it!
	;mov interpret_is_lit,%ecx // Literal?
	;test %ecx,%ecx		// Literal?
	;jnz 5f

	;// Not a literal, execute it now.  This never returns, but the codeword will
	;// eventually call NEXT which will reenter the loop in QUIT.
	;jmp *(%eax)

;5:	;// Executing a literal, which means push it on the stack.
	;push %ebx
	;NEXT

;6:	;// Parse error (not a known word or a number in the current BASE).
	;// Print an error message followed by up to 40 characters of context.
	;mov $2,%ebx		// 1st param: stderr
	;mov $errmsg,%ecx	// 2nd param: error message
	;mov $errmsgend-errmsg,%edx // 3rd param: length of string
	;mov $__NR_write,%eax	// write syscall
	;int $0x80

	;mov (currkey),%ecx	// the error occurred just before currkey position
	;mov %ecx,%edx
	;sub $buffer,%edx	// %edx = currkey - buffer (length in buffer before currkey)
	;cmp $40,%edx		// if > 40, then print only 40 characters
	;jle 7f
	;mov $40,%edx
;7:	;sub %edx,%ecx		// %ecx = start of area to print, %edx = length
	;mov $__NR_write,%eax	// write syscall
	;int $0x80

	;mov $errmsgnl,%ecx	// newline
	;mov $1,%edx
	;mov $__NR_write,%eax	// write syscall
	;int $0x80

	;NEXT


        ; ***** WORD *****
        ; Reads a word of input!
        ; 1. Skips whitespace
        ; 2. Calls KEY to read characters until more whitespace
        ; 3. Calculates length of word, returns address and length on stack
        ;
        ; Forth strings are address+length, no NUL termination.

	DEFCODE "WORD",4,,FWORD ; note changed nasm reserved keyword WORD to FWORD
	call _WORD
	push edi                ; push base address
	push ecx                ; push length
	NEXT
_WORD:
	; Search for first non-blank character.  Also skip \ comments.
.skip_non_words:
	call _KEY               ; get next key, returned in %eax
;	cmpb $'\\',%al		// start of a comment?
;	je 3f			// if so, skip the comment
;	cmpb $' ',%al
;	jbe .skip_non_words			// if so, keep looking

;	/* Search for the end of the word, storing chars as we go. */
;	mov $word_buffer,%edi	// pointer to return buffer
;2:
;	stosb			// add character to return buffer
;	call _KEY		// get next key, returned in %al
;	cmpb $' ',%al		// is blank?
;	ja 2b			// if not, keep looping

;	/* Return the word (well, the static buffer) and length. */
;	sub $word_buffer,%edi
;	mov %edi,%ecx		// return length of the word
;	mov $word_buffer,%edi	// return address of the word
;	ret

;	/* Code to skip \ comments to end of the current line. */
;3:
;	call _KEY
;	cmpb $'\n',%al		// end of line yet?
;	jne 3b
;	jmp .skip_non_words

SECTION .data
word_buffer:
	db 32 ; 32 bytes of buffer for word names

SECTION .text

        ; ***** KEY *****
        ; This should really be called "char" because it gets a character of
        ; input, not a "key". It's easy to imagine the historical
        ; implementation fitting the name, though.
	DEFCODE "KEY",3,,KEY
	call _KEY
	push eax		; push return value on stack
	NEXT
_KEY:
	mov ebx, [currkey]
	cmp ebx, [bufftop]
	jge .get_more_input
	xor eax, eax
	mov al, [ebx]           ; get next key from input buffer
	inc ebx
	mov [currkey], ebx        ; increment currkey
	ret

.get_more_input:  ; Use read(2) to fetch more input from stdin.
	xor ebx,ebx             ; 1st param: stdin
	mov ecx,buffer          ; 2nd param: buffer
	mov [currkey],ecx
	mov edx,buffer_size     ; 3rd param: max length
	mov eax,__NR_read       ; syscall: read
	int 0x80                ; syscall!
	test eax,eax            ; If %eax <= 0, then exit.
	jbe .eof 
	add ecx,eax             ; buffer+%eax = bufftop
	mov [bufftop],ecx
	jmp _KEY

.eof: ; Error or end of input: exit the program.
	xor ebx,ebx
	mov eax,__NR_exit       ; syscall: exit
	int 0x80

SECTION	.data
	align 4
currkey:
	db 0,0,0,0  ; Current place in input buffer (next character to read).
bufftop:
	db 0,0,0,0  ; Last valid data in input buffer + 1.

