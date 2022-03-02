; Dave's NASM port of jonesFORTH
; 
; register use:
;       esi - next forth word address to execute
;       ebp - return stack for forth word addresses
;       esp - "normal" stack for params

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
emit_scratch: resb 4 ; note: JF had this in .data as .space 1

SECTION .data

cold_start: dd QUIT  ; we need a way to indirectly address the first word

SECTION .text

; +----------------------------------------------------------------------------+
; | Forth DOCOL implementation                                                 |
; +----------------------------------------------------------------------------+
; This is the "interpreter" word - it is used at the beginning of proper Forth
; words that are composed of other words (not machine code). It gets the esi
; register pointed at the first word address and starts the NEXT macro.
DOCOL:
    PUSHRSP esi     ; DOCOL: push esi on to the RSP return stack
    add eax, 4      ; eax points to DOCOL (me!) in word definition. Go to next.
    mov esi, eax    ; Put the next word pointer into esi
    NEXT

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

    ; Allocate memory for Forth dictionary.
    ; First, we get the start address of the "break", which is where
    ; the data segment starts. Then we request a break that is at a new
    ; address N bytes larger. The OS does it and now we've got more
    ; memory available to us!
    ;
    ; Note that brk returns the current break address on failure, so
    ; the first call we make with 0 in ebx is a way of making brk fail
    ; on purpose! Most examples on the web scrupulously avoid explaining
    ; this.
    ;
    ; My understanding is that Linux will ACTUALLY allocate the memory
    ; when we attempt to use it. And it will do so in 4Kb chunks.
    xor ebx, ebx
    mov eax, __NR_brk         ; syscall brk
    int 0x80
    mov [var_HERE], eax       ; eax has start addr of data segment
    add eax, 0x16000          ; add our desired number of bytes to break addr
    mov ebx, eax              ; reserve memory by setting this new break addr
    mov eax, __NR_brk         ; syscall brk again
    int 0x80

    ; Now "prime the pump" for the NEXT macro by sticking an indirect
    ; address in esi. NEXT will jump to whatever's stored there.
    mov esi, cold_start
    NEXT ; Start Forthing!

; +----------------------------------------------------------------------------+
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
; instructions and a data word points to a word called "DOCOL", ("do
; the colon word") which loads and runs the individual words.

; in memory, the words look like this for examples "foo" and "bar":
;
;      Code Word                 Data Word
;   name_foo:                 name_bar:
;       (word link)               (word link)
;       (namelen + flags)         (namelen + flags)
;       "foo"                     "bar"
;   foo:                      bar:
;       (ptr to DOCOL)            (ptr to code_bar)
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
             dd DOCOL ; addr of the "interpreter" that will handle
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

    ; the name "QUIT" makes sense from a certain point of view: it
    ; returns to the interpreter
    DEFWORD "QUIT",4,0,QUIT
    dd R0           ; push R0 (addr of top of return stack)
    dd RSPSTORE     ; store R0 in return stack pointer (ebp)
    dd INTERPRET    ; interpret the next word
    dd BRANCH,-8    ; and loop (indefinitely)

    dd gtfo

        ; just try to exit gracefully:
    DEFCODE "gtfo",4,0,gtfo
    mov ebx, 0    ; exit code
    mov eax, 1    ; exit syscall
    int 80h       ; call kernel




; ============================================================
; Words in ASM

        ; return stack pointer
    DEFCODE "RSP!",4,,RSPSTORE
    pop ebp
    NEXT

    ; BRANCH is the simplest possible way to loop - it always
    ; moves the word pointer by the amount in the next value
    ; pointed to by esi! It's helpful to see how LIT works because
    ; it's a similar premise - the value after BRANCH isn't a
    ; word address, it's the amount to add to esi.
    ; To branch/loop back to a previous instruction, you provide
    ; a negative offset.
    DEFCODE "BRANCH",6,,BRANCH
    add esi, [esi]          ; add the offset to the instruction pointer
    NEXT

        ;
        ; * * *   The Forth interpreter!   * * *
        ;
    DEFCODE "INTERPRET",9,,INTERPRET
    call _WORD              ; Returns %ecx = length, %edi = pointer to word.

    ; Is it in the dictionary?
    xor eax,eax             ; back from _WORD...zero eax
    mov [interpret_is_lit], eax ; 0 means not a literal number (yet)
    call _FIND              ; Returns %eax = pointer to header or 0 if not found.
    test eax,eax            ; Found?
    jz .try_literal

    ; In the dictionary.  Is it an IMMEDIATE codeword?
    mov edi,eax             ; edi = dictionary entry YES WE HAVE MATCHED A WORD!!!
    mov al,[edi+4]          ; Get name+flags.
    push ax                 ; Just save it for now.
    call _TCFA              ; Convert dictionary entry (in %edi) to codeword pointer.
    pop ax
    and al,F_IMMED          ; is IMMED flag set?
    mov eax,edi
    jnz .execute            ; If IMMED, jump straight to executing.
    jmp .check_state

.try_literal:      ; (1) Not in the dictionary (not a word) so assume it's a literal number.
    inc byte [interpret_is_lit] ; DID NOT MATCH a word, trying literal number
    call _NUMBER            ; Returns the parsed number in %eax, %ecx > 0 if error
    test ecx,ecx
    jnz .parse_error
    mov ebx,eax
    mov eax,LIT             ; The word is now LIT

.check_state:      ; (2) Are we compiling or executing?
    mov edx,[var_STATE]
    test edx,edx
    jz .execute             ; Jump if executing.

    ; Compiling - just append the word to the current dictionary definition.
    call _COMMA
    mov ecx,[interpret_is_lit] ; Was it a literal?
    test ecx,ecx
    jz .go_next             ; nope, done
    mov eax,ebx             ; Yes, so LIT is followed by a number.
    call _COMMA
.go_next: ; (3)
    NEXT

.execute:         ; (4) Executing - run it!
    mov ecx,[interpret_is_lit] ; Literal?
    test ecx,ecx               ; Literal?
    jnz .do_literal

    ; Not a literal, execute it now.  This never returns, but the codeword will
    ; eventually call NEXT which will reenter the loop in QUIT.
    jmp [eax]

.do_literal:      ; (5) Executing a literal, which means push it on the stack.
    push ebx
    NEXT

.parse_error:     ; (6) Parse error (not a known word or a number in the current BASE).
    ; Print an error message followed by up to 40 characters of context.
    mov ebx,2               ; 1st param: stderr
    mov ecx,errmsg          ; 2nd param: error message
    mov edx,(errmsgend - errmsg) ; 3rd param: length of string
    mov eax,__NR_write      ; write syscall
    int 80h

    mov ecx,[currkey]       ; the error occurred just before currkey position
    mov edx,ecx
    sub edx,buffer          ; edx = currkey - buffer (length in buffer before currkey)
    cmp edx,40              ; if > 40, then print only 40 characters
    jle .print_error
    mov edx,40
.print_error:     ; (7)
    sub ecx,edx             ; ecx = start of area to print, edx = length
    mov eax,__NR_write      ; write syscall
    int 80h

    mov ecx,errmsgnl      ; newline
    mov edx,1               ; 1 char long
    mov eax,__NR_write    ; write syscall
    int 80h

    NEXT


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
    cmp al,'\'              ; start of a comment?
    je .skip_comment        ; if so, skip the comment
    cmp al,' '              ; compare to ASCII space (0x20)
    jbe .skip_non_words     ; Is space or lower, keep scanning

        ; now we've reached a word - start storing the chars
    mov edi,word_buffer     ; put addr to word return buffer in edi (used by stosb)
.collect_word:
    stosb                   ; add character to return buffer (8 bits from al)
    call _KEY               ; get next key, returned in %al
    cmp al,' '              ; compare to ASCII space (0x20)
    ja .collect_word        ; Is higher than space, keep collecting

        ; return word buffer addr and length...
    sub edi, word_buffer    ; calculate the length of the word
    mov ecx, edi            ; return it
    mov edi, word_buffer    ; return start address of the word
    ret

.skip_comment: ; skip \ comment to end of current line
    call _KEY
    cmp al,`\n`             ; eol? (escapes okay in backtick strings in nasm)
        jne .skip_comment
    jmp .skip_non_words

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
    push eax        ; push return value on stack
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


        ; ***** TCFA *****
        ; Turn a dictionary pointer into a codeword pointer.
        ; This is where we use the stored length of the word name
        ; to skip to the beginning of the code.
    DEFCODE ">CFA",4,,TCFA
    pop edi
    call _TCFA
    push edi
    NEXT
_TCFA:
    xor eax,eax
    add edi,4               ; Skip link pointer.
    mov al,[edi]            ; Load flags+len into %al.
    inc edi                 ; Skip flags+len byte.
    and al,F_LENMASK        ; Just the length, not the flags.
    add edi,eax             ; Skip the name.
    add edi,3               ; The codeword is 4-byte aligned:
    and edi,~3              ;   Add ...00000011 and mask ...11111100.
    ret                     ;   For more, see log06.txt in this repo.

        ; ***** NUMBER *****
        ; parse numeric literal from input using BASE as radix
    DEFCODE "NUMBER",6,,NUMBER
    pop ecx                 ; length of string
    pop edi                 ; start address of string
    call _NUMBER
    push eax                ; parsed number
    push ecx                ; number of unparsed characters (0 = no error)
    NEXT
_NUMBER:
    xor eax,eax
    xor ebx,ebx

    test ecx,ecx            ; trying to parse a zero-length string is an error, but returns 0
    jz .return

    mov edx, [var_BASE]    ; get BASE (in dl)

    ; Check if first character is '-'.
    mov bl,[edi]            ; bl = first character in string
    inc edi
    push eax                ; push 0 on stack
    cmp bl,'-'              ; negative number?
    jnz .convert_char
    pop eax
    push ebx                ; push non-0 on stack, indicating negative
    dec ecx
    jnz .next_char
    pop ebx                 ; error: string is only '-'.
    mov ecx,1
    ret

.next_char:        ; (1) Loop reading digits.
    imul eax,edx           ; eax *= BASE
    mov bl,[edi]           ; bl = next character in string
    inc edi

.convert_char:   ; (2) Convert 0-9, A-Z to a number 0-35.
    sub bl,'0'              ; < '0'?
    jb .negate
    cmp bl,10        ; <= '9'?
    jb .compare_base
    sub bl,17              ; < 'A'? (17 is 'A'-'0')
    jb .negate
    add bl,10

.compare_base:   ; (3)
        cmp bl,dl               ; >= BASE?
    jge .negate

    ; add it to eax and loop.
    add eax,ebx
    dec ecx
    jnz .next_char

.negate:       ; (4) Negate the result if first character was '-' (saved on the stack).
    pop ebx
    test ebx,ebx
    jz .return
    neg eax

.return: ;(5)
    ret



        ; ***** LIT *****
        ; esi always points to the next thing. Usually this is
        ; the next word. But in this case, it's the literal value
        ; to push onto the stack.
    DEFCODE "LIT",3,,LIT
    lodsd                   ; loads the value at esi into eax, incements esi
    push eax                ; push the literal number on to stack
    NEXT

        ; ***** FIND *****
    ; Before this, we'll have called _WORD which pushed (returned):
        ;     ecx = length
        ;     edi = start of word (addr)
    DEFCODE "FIND",4,,FIND
    pop ecx                 ; length of word
    pop edi                 ; buffer with word
    call _FIND
    push eax                ; push address of dict entry (or null) as return val
    NEXT

_FIND:
    push esi                ; _FIND! Save esi, we'll use this reg for string comparison

    ; Now we start searching backwards through the dictionary for this word.
    mov edx,[var_LATEST]    ; LATEST points to name header of the latest word in the dictionary
.test_word:
        test edx,edx            ; NULL pointer?  (end of the linked list)
    je .not_found

    ; First compare the length expected and the length of the word.
    ; Note that if the F_HIDDEN flag is set on the word, then by a bit of trickery
    ; this won't pick the word (the length will appear to be wrong).
    xor eax,eax
    mov al, [edx+4]           ; al = flags+length field
    and al,(F_HIDDEN|F_LENMASK) ; al = name length
    cmp cl,al        ; Length is the same?
    jne .prev_word          ; nope, try prev

    ; Compare the strings in detail.
    push ecx                ; Save the length
    push edi                ; Save the address (repe cmpsb will move this pointer)
    lea esi,[edx+5]         ; Dictionary string we are checking against.
    repe cmpsb              ; Compare the strings.
    pop edi
    pop ecx
    jne .prev_word          ; nope, try prev

    ; The strings are the same - return the header pointer in eax
    pop esi
    mov eax,edx
    ret                     ; Found!

.prev_word:
    mov edx,[edx]           ; Move back through the link field to the previous word
    jmp .test_word          ; loop, test prev word

.not_found:
    pop esi
    xor eax,eax             ; Return zero to indicate not found (aka null ptr)
    ret








        ; CREATE makes words! Specifically, the header portion of words.

    DEFCODE "CREATE",6,,CREATE
    pop ecx                   ; length of word name
    pop ebx                   ; address of word name

    ; link pointer
    mov edi, [var_HERE]       ; the address of the header
    mov eax, [var_LATEST]     ; get link pointer
    stosd                     ; and store it in the header.

    ; Length byte and the word itself.
    mov al, cl                ; Get the length.
    stosb                     ; Store the length/flags byte.
    push esi
    mov esi, ebx              ; esi = word
    rep movsb                 ; Copy the word
    pop esi
    add edi, 3                ; Align to next 4 byte boundary. See TCFA
    and edi, ~3

    ; Update LATEST and HERE.
    mov eax, [var_HERE]
    mov [var_LATEST], eax
    mov [var_HERE], edi
    NEXT


    ; COMMA (,) 
    ; This is a super primitive word used to compile words. It puts the
    ; currently-pushed value from the stack to the position pointed to
    ; by HERE and increments HERE to the next 4 bytes.

    DEFCODE ",",1,,COMMA
    pop eax                ; Code pointer to store.
    call _COMMA
    NEXT
    _COMMA:
    mov edi, [var_HERE]
    stosd                  ; puts the value in eax at edi, increments edi
    mov [var_HERE], edi
    ret

    ; LBRAC and RBRAC ([ and ])
    ; Simply toggle the STATE variable (0=immediate, 1=compile)
    ; So:
    ;       <compile mode> [ <immediate mode> ] <compile mode>
    ;
    ; Note that LBRAC has the immediate flag set because otherwise
    ; it would get compiled rather than switch modes then and there.

    DEFCODE "[",1,F_IMMED,LBRAC
    xor eax, eax
    mov [var_STATE], eax      ; Set STATE to 0 (immediate)
    NEXT

    DEFCODE "]",1,,RBRAC
    mov [var_STATE], word 1   ; Set STATE to 1 (compile)
    NEXT


    ; Just two more "code" word definitions before we can define
    ; COLON out of pure words.
    ;
    ; FETCH (@) grabs a value from an address on the stack and
    ; puts that value on the stack.
    ;
    ; HIDDEN toggles the hidden flag for the dictionary entry
    ; at the address on the stack
    ;
    ; EXIT pops the return stack value into esi - this is the
    ; reverse of what DOCOL does.

    DEFCODE "@",1,,FETCH
    pop ebx                 ; address to fetch
    mov eax, [ebx]          ; fetch it
    push eax                ; push value onto stack
    NEXT

    DEFCODE "HIDDEN",6,,HIDDEN
    pop edi                 ; Dictionary entry, first byte is link
    add edi, 4              ; Move to name/flags byte.
    xor [edi], word F_HIDDEN  ; Toggle the HIDDEN bit in place.
    NEXT

    DEFCODE "EXIT",4,,EXIT
    POPRSP esi    	    ; pop return stack into esi
    NEXT

    ; COLON (:) creates the new word header and starts compile mode
    ; It also sets the new definition to hidden so the word isn't
    ; discovered while it is being compiled.

    DEFWORD ":",1,,COLON
    dd FWORD                 ; Get the name of the new word
    dd CREATE               ; CREATE the dictionary entry / header
    dd LIT, DOCOL, COMMA    ; Append DOCOL  (the codeword).
    ;dd LATEST, FETCH, HIDDEN ; Make the word hidden while it's being compiled.
    dd LATEST, HIDDEN ; Make the word hidden while it's being compiled.
    dd RBRAC                ; Go into compile mode.
    dd EXIT                 ; Return from the function.

    ; SEMICOLON (;) is an immediate word (F_IMMED) and it ends compile
    ; mode and unhides the word entry being compiled.

    DEFWORD ";",1,F_IMMED,SEMICOLON
    dd LIT, EXIT, COMMA     ; Append EXIT (so the word will return).
;    dd LATEST, FETCH, HIDDEN ; Unhide word now that it's been compiled.
    dd LATEST, HIDDEN ; Unhide word now that it's been compiled.
    dd LBRAC                ; Go back to IMMEDIATE mode.
    dd EXIT                 ; Return from the function.







; =============================================================================
; Final word definitions
; =============================================================================

    ; EMIT just displays a character of output from the stack.
    ; It doesnt attempt to be efficient at all (no buffering, etc.)
    DEFCODE "EMIT",4,,EMIT
    pop eax
    call _EMIT
    NEXT
_EMIT:
    mov [emit_scratch], al  ; put char to print at scratch space
    mov ebx, 1              ; syscall param 1: stdout
    mov ecx, emit_scratch   ; syscall param 2: address to print
    mov edx, 1              ; syscall param 3: length in bytes to print
    mov eax, __NR_write     ; syscall 'write'
    int 0x80                ; request syscall!
    ret


    DEFCODE "CHAR",4,,CHAR
    call _WORD              ; Returns %ecx = length, %edi = pointer to word.
    xor eax,eax
    mov al,[edi]            ; Get the first character of the word.
    push eax                ; Push it onto the stack.
    NEXT

    DEFCODE "EXECUTE",7,,EXECUTE
    pop eax                ; Get xt into %eax
    jmp [eax]              ; and jump to it.
                           ; After xt runs its NEXT will continue executing the current word.

    DEFCODE "SYSCALL3",8,,SYSCALL3
    pop eax                ; System call number (see <asm/unistd.h>)
    pop ebx                ; First parameter.
    pop ecx                ; Second parameter
    pop edx                ; Third parameter
    int 80h
    push eax               ; Result (negative for -errno)
    NEXT

    DEFCODE "SYSCALL2",8,,SYSCALL2
    pop eax                ; System call number (see <asm/unistd.h>)
    pop ebx                ; First parameter.
    pop ecx                ; Second parameter
    int 80h
    push eax               ; Result (negative for -errno)
    NEXT

    DEFCODE "SYSCALL1",8,,SYSCALL1
    pop eax                ; System call number (see <asm/unistd.h>)
    pop ebx                ; First parameter.
    int 80h
    push eax               ; Result (negative for -errno)
    NEXT

    DEFCODE "SYSCALL0",8,,SYSCALL0
    pop eax                ; System call number (see <asm/unistd.h>)
    int 80h
    push eax               ; Result (negative for -errno)
    NEXT

; +----------------------------------------------------------------------------+
; Forth constants:
;
;  VERSION        Is the current version of this FORTH.
;  R0        The address of the top of the return stack.
;  DOCOL        Pointer to DOCOL.
;  F_IMMED        The IMMEDIATE flag's actual value.
;  F_HIDDEN    The HIDDEN flag's actual value.
;  F_LENMASK    The length mask in the flags/len byte.
;  SYS_*        and the numeric codes of various Linux syscalls (from <asm/unistd.h>)


; Check it out! A const is just a word that pushes a value!
%macro DEFCONST 5 ; 1=name 2=namelen 3=flags 4=label 5=value
        DEFCODE %1,%2,%3,%4
        push %5
        NEXT
%endmacro

    DEFCONST "VERSION",7,,VERSION,NASMJF_VERSION
    DEFCONST "R0",2,,R0,return_stack_top
    DEFCONST "DOCOL",5,,__DOCOL,DOCOL
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
; Built-in vars:
;   STATE   Is the interpreter executing code (0) or compiling a word (non-zero)?
;   LATEST  Points to the latest (most recently defined) word in the dictionary.
;   HERE    Points to the next free byte of memory.  When compiling, compiled words go here.
;   S0      Stores the address of the top of the parameter stack.
;   BASE    The current base for printing and reading numbers.
;  
%macro DEFVAR 5 ; 1=name 2=namelen 3=flags 4=label 5=value
        DEFCODE %1,%2,%3,%4
        push dword [var_%4]
        NEXT
    section .data
        align 4
    var_%4:
        dd %5 ; note dd to reserve a "double" (4b)
%endmacro

    DEFVAR "STATE",5,,STATE,0
    DEFVAR "HERE",4,,HERE,0
    DEFVAR "S0",2,,SZ,0
    DEFVAR "BASE",4,,BASE,10
    DEFVAR "LATEST",6,,LATEST,name_LATEST ; points to last word defined...which will just
                                          ; happen to be self. We'll see if this works.



SECTION    .data
    align 4
currkey: db 0,0,0,0  ; Current place in input buffer (next character to read).
bufftop: db 0,0,0,0  ; Last valid data in input buffer + 1.
interpret_is_lit: db 0,0,0,0 ; 1 means "reading a literal"
errmsg: db "PARSE ERROR: "
errmsgend:
errmsgnl: db `\n`
