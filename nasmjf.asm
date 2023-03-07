; +----------------------------------------------------------------------------+
; | Dave's NASM port of JONESFORTH                                             |
; +----------------------------------------------------------------------------+
;
; This port will have explanitory comments in my own words.
;
; For the full "JONESFORTH experience", read the original source files which
; you should find in this repo at:
;
;     jonesforth/jonesforth.S
;     jonesforth/jonesforth.f
; 
%assign NASMJF_VERSION 48 ; One more than JONESFORTH

; Guide to register use:
;     esi - Next Forth word address pointer
;     ebp - Return stack pointer ("RSP")
;     esp - THE STACK (aka "parameter stack") pointer
;
; A Forth system is composed of words. Words are like functions: they contain
; a series of instructions. Those instructions are actually just a list of
; pointers to other words. So it's words all the way down.
;
; Well, actually, nothing really useful happens until you get to one of the
; base words that is written in machine code. These "code words" are the low
; level primitives that provide the "bootstrapping" fuctionality for all of
; the other words built on top of them.
;
; Whether a word is a regular word or a code word, it has the same basic
; structure, starting with a header:
;
; Here's one called "FOO":
;
;        Word Header      (For *all* word types)
;    +------------------+
; <----0x8C5FCD8        | Pointer to the previous word
;    +------------------+
;    | 3                | Name length (3 for "FOO") & flags (none)
;    +------------------+
;    | 'F'              |
;    +------------------+
;    | 'O'              |
;    +------------------+
;    | 'O'              |
;    +------------------+
;
; After the header, a code word looks like this:
;
;          ...            <--- Header ends here, word body begins
;        Code Word
;    +------------------+
;  +---0x80490A3        | Pointer to the machine code that follows!
;  | +------------------+
;  +-->D4 88 F8 02      | Machine code!
;    +------------------+
;    | A8 0F 98 C3      | More machine code...
;    +------------------+
;      ...
;    +------------------+
;    | NEXT             |
;    +------------------+
;
;
; Which seems weird and pointless (why not just start executing the machine
; code directly?) until we look at a regular word that is not a code word
; made of machine code. A regular word looks like this:
;
;          ...            <--- Header ends here, word body begins
;       Regular Word
;    +------------------+
; <----0x80490A3        | Pointer to special *interpreter code word*
;    +------------------+
; <----0x804A2F0        | Address of _another_ word's code word
;    +------------------+
; <----0x804A2F0        | And another...
;    +------------------+
;      ...
;    +------------------+
;    | EXIT             |
;    +------------------+
;
; Regular words use the "EXIT" word, a return stack, and the ebp register to
; do the same thing. And here's the fun part: EXIT ends with NEXT!
;
; What the regular words and "code words" have in common is that they both
; start (after the header) with a pointer that points to machine code to be
; executed. Also known as the *interpreter code word*. For many regular words,
; the interpreter code word will be:
;
;            DOCOL
;
; The interpreter code word executes the rest of the current word by
; incrementing the instruction pointer (esi) and calling the NEXT macro.
;
; This is called "indirect threaded code" because of the second level of
; pointer indirection.
;
; It may be helpful to summarize at this point:
;
;     |==============|=============|============|===========================|
;     | Type of word | Starts with | Ends with  | Which uses                |
;     |--------------|-------------|------------|---------------------------|
;     | Regular word | Ptr to code | EXIT ptr   | esi, main data memory     |
;     | Codeword     | Ptr to self | NEXT macro | ebp ("RSP"), return stack |
;     |==============|=============|============|===========================|
;
; Also, let's visualize the layout of a code word and regular word side-by side:
;
;         Code Word                    Regular Word
;    +------------------+          +------------------+
;    | Link pointer     |          | Link pointer     |
;    +------------------+          +------------------+
;    | Name/flags       |          | Name/flags       |
;    +------------------+          +------------------+      +------------+
;    | Pointer to code  | ---+     | Pointer to DOCOL | ---> | DOCOL      |
;    +------------------+    |     +------------------+      +------------+
;    | <machine code>   |<---+     | Pointer to word  | <--- | NEXT       |
;    +------------------+          +------------------+      +------------+
;    | <machine code>   |          | Pointer to EXIT  |   
;    +------------------+          +------------------+      +------------+
;    | NEXT             |          | Pointer to EXIT  | ---> + EXIT       |
;    +------------------+          +------------------+      +------------+
;                                                            | NEXT       |
;                                                            +------------+
;
; Without further ado, here's the next macro:
;
; +----------------------------------------------------------------------------+
; | The NEXT Macro                                                             |
; +----------------------------------------------------------------------------+
; Register esi is the instruction pointer. NEXT puts the pointer it's pointing
; to into register eax and advances esi 
;
;    +------------------+                NEXT:
; <----0x8000000        | <-- esi        * eax = 0x8000000
;    +------------------+           +--- * esi points to next pointer
; <----0x8AAAAAA        | <---------+    * jump to address in eax
;    +------------------+
;
; The only thing that keeps this whole thing moving is the fact that *every*
; word ends in NEXT. There is no other mechanism propelling this threaded
; interpreter forward.
%macro NEXT 0
    lodsd     ; NEXT: Load from memory into eax, inc esi to point to next word.
    jmp [eax] ; Jump to whatever code we're now pointing at.
%endmacro
 
; That's a lot of stuff pointing at stuff.
 
; By the way, the thing that makes Forth so hard to understand isn't all the
; little details. It's the fact that *none of it makes sense in pieces*. Only
; with the entire puzzle together in your head can you comprehend the machine.

; +----------------------------------------------------------------------------+
; | Return stack PUSH/POP macros                                               |
; +----------------------------------------------------------------------------+
; The ebp register will be the return stack pointer ("RSP")
; The PUSHRSP and POPRSP macros handle pushing registers onto stack memory.
; The return stack is used to 
; (NASM macros use placeholders %1, %2, etc. as sequential params to substitute
; into the machine code verbatim.)
%macro PUSHRSP 1
    lea ebp, [ebp-4]   ; "load effective address" of next stack position
    mov [ebp], %1      ; "push" the register value to the address at ebp
%endmacro
%macro POPRSP 1
    mov %1, [ebp]
    lea ebp, [ebp+4]
%endmacro

; +----------------------------------------------------------------------------+
; | System Call Numbers                                                        |
; +----------------------------------------------------------------------------+
; JONESFORTH uses an external include file which you may not have. I'm just
; gonna hardcode them here. I can't imagine these changing often.
; (I found them in Linux source in file arch/x86/include/asm/unistd_32.h)
%assign __NR_exit  1
%assign __NR_open  5
%assign __NR_close 6
%assign __NR_read  3
%assign __NR_write 4
%assign __NR_creat 8
%assign __NR_brk   45

; +----------------------------------------------------------------------------+
; | Return stack and main memory - initial memory allocations                  |
; +----------------------------------------------------------------------------+
; The BSS section is for uninitialized storage space. We'll reserve bytes (resb)
; and make labels so we can refer to these addresses later. The following are
; reserved:
;  * buffer - storage for user input
;  * return stack - addresses of words so EXIT can return to them
;  * emit scratch - just a 4-byte bit of memory to store characters to print
SECTION .bss
%define buffer_size 4096
return_stack: resb 8192
return_stack_top: resb 4
buffer:       resb buffer_size
emit_scratch: resb 4 ; (note: JF had this in .data as .space 1)

; +----------------------------------------------------------------------------+
; | A label used as a pointer to the first word that will be executed          |
; +----------------------------------------------------------------------------+
; We need to be able to indirectly address the first word because that's how
; NEXT works - it can only jump to an address _in memory_. So we'll use this
; cold_start label pointing to memory containing the address of the first
; codeword by putting it in the esi register and then calling NEXT. And yes,
; that's right, in standard Forth convention, we start with "QUIT"!
SECTION .data
cold_start: dd QUIT  ; we need a way to indirectly address the first word

; +----------------------------------------------------------------------------+
; | "LOADJF"                                                                   |
; +----------------------------------------------------------------------------+
; I load the rest of the interpreter - a Forth source file - upon startup.
;
; This is a major difference with my port. Search for 'LOADJF' in this file to
; see all of the places where I made changes or additions to support this.
;
; This path is relative by default to make it easy to run 'nasmjf' from
; the repo dir. But you can set it to an absolute path to allow running
; from any location.
jfsource:     db "jonesforth/jonesforth.f", 0h ; LOADJF path, null-terminated
jfsource_end: db 0h                            ; LOADJF null-terminated string


; +----------------------------------------------------------------------------+
; | Program entry point - start the interpreter!                               |
; +----------------------------------------------------------------------------+
; Now begins the real program. There's some housekeeping to do and almost all
; of it is setting up memory and pointers to memory.
SECTION .text
global _start

_start:
    ; Clear the "direction flag" which means the string instructions
    ; (such as LODSD) work in increment order instead of decrement.
    cld

    ; Save the current value of the stack pointer to S0. This is the first
    ; variable we've seen that is available in Forth. You can examine and change
    ; the value of this variable in the interpreter!
    mov [var_SZ], esp

    ; We will use ebp to keep track of the return stack, used by EXIT
    ; to return to the previous word when the current one is finished.
    mov ebp, return_stack_top

    ; Now allocate main memory for Forth dictionary and data!
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
    ; We store the start, end, and current "position" in this main
    ; memory in variables. HERE is particularly important!
    xor ebx, ebx
    mov eax, __NR_brk         ; syscall brk
    int 0x80
    mov [var_HERE], eax       ; eax has start addr of data segment
    mov [var_CSTART], eax     ; store info: start address of data segment
    add eax, 0x16000          ; add our desired number of bytes to break addr
    mov ebx, eax              ; reserve memory by setting this new break addr
    mov [var_CEND], eax       ; store info: end address of data segment
    mov eax, __NR_brk         ; syscall brk again
    int 0x80

    ; "LOADJF" Process jonesforth.f upon startup. Open the file.
    ; Then store the file descriptor (fd) so we can make the interpreter
    ; read from the file rather than from STDIN.
    mov ecx, 0                ; LOADJF read only flag for open
    mov ebx, jfsource         ; LOADJF address of string path for open
    mov eax, __NR_open        ; LOADJF open syscall
    int 80h                   ; LOADJF fd now in eax
    cmp eax, 0                ; LOADJF fd < 0 is an error!
    jl .loadjf_open_fail
    mov [read_from_fd], eax   ; LOADJF store fd and tell KEY to read from this

    ; Now "prime the pump" for the NEXT macro by sticking an indirect
    ; address in esi. NEXT will jump to whatever's stored there.
    ; Housekeeping stuff is over. The interpreter will start running now.
    mov esi, cold_start
    NEXT ; Start Forthing!

    ; Handle failure of LOADJF!
    ; I could have avoided a lot of code if I just exited when opening
    ; the jonesforth.f file fails. But I thought it was important to make
    ; a proper error message that was as helpful as possible. Because
    ; if it fails, it's going to be _very_ confusing. I'm sure it will
    ; happen to me years from now when I revisit this. And I don't want
    ; to be confused!
.loadjf_open_fail:             ; LOADJF
    ; For each of these write syscalls:
    ;     ebx = stderr fd
    ;     ecx = start address of string
    ;     edx = length of string
    ; Print first half of error message
    mov ebx, 2                 ; LOADJF
    mov ecx, loadjf_fail_msg   ; LOADJF
    mov edx, (loadjf_fail_msg_end - loadjf_fail_msg)
    mov eax, __NR_write        ; LOADJF
    int 80h                    ; LOADJF
    ; Print jonesforth source path
    mov ebx, 2                 ; LOADJF
    mov ecx, jfsource          ; LOADJF
    mov edx, (jfsource_end - jfsource)
    mov eax, __NR_write        ; LOADJF
    int 80h                    ; LOADJF
    ; Print second half of error message
    mov ebx, 2                 ; LOADJF
    mov ecx, loadjf_fail_msg2  ; LOADJF
    mov edx, (loadjf_fail_msg_end2 - loadjf_fail_msg2)
    mov eax, __NR_write        ; LOADJF
    int 80h                    ; LOADJF
    mov ebx, 1                 ; LOADJF exit code and fall through to exit

    ; Exit program.
    ; I define this here so the above LOADJF failure can fall through into it.
    ; But it is also called when the user ends input (Ctrl+d) in the normal use
    ; of the interpreter.
exit_with_grace_and_beauty: ; (don't forget to set ebx to exit code)
    mov eax,__NR_exit       ; syscall: exit
    int 0x80                ; invoke syscall

; +----------------------------------------------------------------------------+
; |                                                                            |
; |                           Part Two: Words!                                 |
; |                                                                            |
; +----------------------------------------------------------------------------+
; Everything from here on out is Forth bootstrapping itself as a series of word
; definitions - first in machine code (written in assembly), then as words
; defined as lists of addresses of other words. Lastly, as text in the Forth
; language!
;
; +----------------------------------------------------------------------------+
; | Forth DOCOL implementation                                                 |
; +----------------------------------------------------------------------------+
; This is the "interpreter" word - it is used at the beginning of "normal" Forth
; words (composed of other words, not machine code). All DOCOL does is gets the
; esi register pointed at the first word address and starts the NEXT macro.
; (See my ASCII art boxes at the top of this document. Search for "DOCOL".)
;
; Note that esi doesn't contain the address of the next word to run. Instead,
; it contains the next address that will *point to* the next word to run!
DOCOL:
    PUSHRSP esi     ; push esi on to the "RSP" return stack
    add eax, 4      ; eax currently points to DOCOL (me!), point to next addr
    mov esi, eax    ; Load the next word pointer into esi
    NEXT

; +----------------------------------------------------------------------------+
; | Word header flags                                                          |
; +----------------------------------------------------------------------------+
; These are bits that can be ANDed together to indicate special properties of
; the word: 
;  * IMMED - an "immediate" word runs in compile mode
;  * HIDDEN - a word is usually hidden while it's being compiled!
;  * LENMASK - to save space (!), the word name length is combined with flags
%assign F_IMMED 0x80
%assign F_HIDDEN 0x20
%assign F_LENMASK 0x1f

; Link holds address of last word defined (to make linked list)
; (NASM Note: Must be %define rather than %assign or we'll run afoul
; assigning the name_label address below.)
%define link 0           ; null link - beginning of the linked list

; +----------------------------------------------------------------------------+
; | DEFWORD and DEFCODE macros                                                 |
; +----------------------------------------------------------------------------+
; As mentioned in the beginning, there are two kinds of words in Forth: 
;
;   1. Code words are pure machine language
;   2. Regular words are defined as a series of pointers to other words
;
; Both start with a header: link, name length + flags, name.
;
; After the header, both start with an address: a pointer to code to be
; executed immediatly when we run the word.
;
; The big difference is that a code word points to its own instructions, while
; a regular word points to DOCOL, the "interpreter word", which sets the esi
; register and uses NEXT to execute the rest of the word definition.
;
; Refer again to the ASCII art boxes a the top of this file to see how the two
; types of words are laid out in memory.
;
; The following assembler macros help us create both types of words from
; within assembly.
;
; The two macros are very similar. But notice how DEFWORD begins the body
; of the word after the header with the address of DOCOL.
;
; Define a regular word. Create header from name and flags, then start the
; word body with the address of DOCOL.
%macro DEFWORD 3 ; 1=name 2=label 3=flags
    %strlen namelen %1 ; NASM calculates this for us!
    SECTION .data
    align 4            ; Everything is aligned on 4 byte boundaries.

    ; Start of the word header
    ; ------------------------
    global name_%2     ; name_<label> for use in assembly
    name_%2:
        dd link                ; link the previous word's addr
        %define link name_%2   ; store *my* link addr for next time
        db %3 + namelen        ; flags + namelen (packed into byte)
        db %1                  ; name string ("FOO")
        align 4

    ; Start of the word body
    ; ----------------------
     global %2    ; <label> for use in assembly
     %2:
         dd DOCOL ; Pointer to DOCOL code word that will execute the
                  ; word pointer that will follow the use of this macro.
%endmacro

; Define a code word. Create header from name and flags, then start the
; word body with the next address after itself. See comments in DEFWORD
; above for explanation of the header portion.
%macro DEFCODE 3 ; 1=name 2=label 3=flags
    %strlen namelen %1
    SECTION .data
    align 4

    ; Start of the word header
    ; ------------------------
    global name_%2
    name_%2:
        dd link
        %define link name_%2   ; store this link addr for next time
        db %3 + namelen        ; flags + namelen 
        db %1                  ; name string
        align 4

    ; Start of the word body
    ; ----------------------
    global %2
    %2:
        dd code_%2 ; The address of the label that follows...
        align 4

    SECTION .text  ; Assembly intructions (machine code) will follow.
    global code_%2
    code_%2:
        ; Whatever follows the use of this macro is the machine code
        ; definition of the code word. We can execute this word directly
        ; in assembly by jumping to this label. We can "compile it" into
        ; a regular word with the body label (%2). And like all words,
        ; we can execute it in Forth using it's string name.
%endmacro

; +----------------------------------------------------------------------------+
; +----------------------------------------------------------------------------+
; What follow are 9 regular words and 130 code words. Only some fraction of the
; code words are really be required here. They're just more efficient if they're
; implemented in assembly rather than Forth itself.

; +----------------------------------------------------------------------------+
; | QUIT: the "outer interpreter"                                              |
; +----------------------------------------------------------------------------+
; At the top of this file, I describe the shape of word definitions and the
; "interpreter word" and the NEXT and EXIT mechanisms. Now we can take a gander
; at the outer main loop that really holds all of this together and makes this
; Forth an actual interpreter in the sense most of us expect.
;
; You'll notice that QUIT contains neither a NEXT nor EXIT. This is the outer
; loop ; and a NEXT will eventually bring us back here, where an unconditional
; loop ; will keep looking for input and executing it.
;
; (And yes, "QUIT" is a bizarre name for this word.)
;
; I think this might be a helpful way to think of the nested nature of QUIT
; and the two types of words:
;
; QUIT (INTERPRET)
;     * regular word
;         DOCOL
;         NEXT
;         * regular word
;             DOCOL (codeword
;             NEXT
;             * code word
;                 <machine code>
;             NEXT
;             * code word
;                 <machine code>
;             NEXT
;         EXIT
;         NEXT
;    EXIT
;    NEXT
; QUIT (BRANCH -8 back to INTERPRET for more)
;   
; Notice how every code word ends in a NEXT and every regular word ends in
; an EXIT, which also has a NEXT to go to back up the call stack.
;
; And when those words are done, the next address to execute happens to
; be the unconditional branch in QUIT that starts the "outer interpreter"
; loop all over again.
;
; Here's the definition of QUIT. Notice how much easier it is to write than
; to describe!
DEFWORD "QUIT",QUIT,0
    dd R0           ; push R0 (addr of top of return stack)
    dd RSPSTORE     ; store R0 in return stack pointer (ebp)
    dd INTERPRET    ; interpret the next word
    dd BRANCH,-8    ; and loop (indefinitely)

; +----------------------------------------------------------------------------+
; | EXIT                                                                       |
; +----------------------------------------------------------------------------+
; And here's EXIT. Look at how tiny this is! This ends every regular word by
; popping the "return address" pushed by DOCOL when the word began.
DEFCODE "EXIT",EXIT,0
    POPRSP esi            ; pop return stack into esi
NEXT

; +----------------------------------------------------------------------------+
; | The Forth Interpreter words                                                |
; +----------------------------------------------------------------------------+
; The following three words contain some pretty beefy assembly code. They get
; input, split it into words, find the word definitions, and execute them:
;
; KEY        - Buffers input from STDIN (or a file)
; WORD       - Calls KEY, gets a whitespace-delimited "word" of text
; INTERPRET  - Calls WORD, looks up words in dictionary, attempts to
;              handle literal number values, and executes the results.
;
; Now, here they are in the opposite order:
;
; +----------------------------------------------------------------------------+
; | INTERPRET                                                                  |
; +----------------------------------------------------------------------------+
; Get's "word" of input (that term is overloaded here) and determines what to
; do with it.
DEFCODE "INTERPRET",INTERPRET,0
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
    cmp edx,40              ; if >= 40, then print only 40 characters
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

; +----------------------------------------------------------------------------+
; | WORD                                                                       |
; +----------------------------------------------------------------------------+
; Return a Forth string: an address and length (unlike C strings, we don't end
; with a sentinel NUL.) This should perhaps be called "token".
DEFCODE "WORD",FWORD,0  ; Note changed nasm reserved keyword WORD to FWORD!
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
    times 32 db 0x0 ; 32 bytes of buffer for word names
SECTION .text

; +----------------------------------------------------------------------------+
; | KEY                                                                        |
; +----------------------------------------------------------------------------+
; This should really be called "char" because it gets a character of input, not
; a "key". It's easy to imagine the historical implementation fitting the name.
DEFCODE "KEY",KEY,0
    call _KEY
    push eax        ; push return value on stack
    NEXT
_KEY:
    mov ebx, [currkey]
    cmp ebx, [bufftop]
    jge .get_more_input
    xor eax, eax
    mov al, [ebx]               ; get next key from input buffer

.continue_with_key:
    inc ebx
    mov [currkey], ebx        ; increment currkey
    ret

.get_more_input:  ; Use read(2) to fetch more input
    mov ebx, [read_from_fd] ; LOADJF 1st param: input file (STDIN when getting user input)
    ;xor ebx,ebx             ; 1st param: stdin
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

.eof: ; Error or end of input
    cmp dword [read_from_fd], 0 ; LOADJF If we were reading from STDIN (0)...
    je .eof_stdin               ; LOADJF ...then exit the program normally.
    mov ebx, [read_from_fd]     ; LOADJF Otherwise, close the file.
    mov eax, __NR_close         ; LOADJF
    int 80h
    mov dword [read_from_fd], 0 ; LOADJF Change the read-from fd to STDIN.
    jmp .get_more_input         ; LOADJF And continue reading!
.eof_stdin: ; Exit peacefully!
    xor ebx,ebx             ; set ebx to exit with no error (0)
    jmp exit_with_grace_and_beauty


; +----------------------------------------------------------------------------+
; | Some Forth primitives                                                      |
; +----------------------------------------------------------------------------+
; TICK (or single quote: ') gets the address of the word
; that matches the next word of input text. Uses the same
; lodsd trick as LIT to grab the next word of input without
; executing it. Only works while in compile state. (: ... ;)
; It's not an immediate word, so it executes at run time,
; which is why we end up with the address of the next word
; (which was matched at compile time) to put on the stack!
DEFCODE "'",TICK,0
    lodsd                   ; Moves value at esi to eax, esi++
    push eax                ; Push address on the stack
NEXT

; BRANCH is the simplest possible way to loop - it always
; moves the word pointer by the amount in the next value
; pointed to by esi! It's helpful to see how LIT works because
; it's a similar premise - the value after BRANCH isn't a
; word address, it's the amount to add to esi.
; To branch/loop back to a previous instruction, you provide
; a negative offset.
; esi currently points at the offset number.
DEFCODE "BRANCH",BRANCH,0
    add esi, [esi]          ; add the offset to the instruction pointer
NEXT

; 0BRANCH is the same thing, but with a condition: it only
; jumps if the top of the stack is zero.
DEFCODE "0BRANCH",ZBRANCH,0
    pop eax
    test eax, eax           ; top of stack is zero?
    jz code_BRANCH          ; if so, jump back to BRANCH
    lodsd                   ; or skip the offset (esi to eax, esi++)
NEXT

; Another primitive - this one is used to implement the string
; words in Forth (." and S"). I'll just port it for now, then
; test it later.
; The lodsd "trick" (see also LIT) to load the next 4 bytes of
; memory from the address at the current instruction pointer
; (esi) into eax and then increment esi to skip over it so
; NEXT doesnt try to execute it.
DEFCODE "LITSTRING",LITSTRING,0
    lodsd                   ; get the length of the string into eax
    push esi                ; push the address of the start of the string
    push eax                ; push it on the stack
    add esi, eax            ; skip past the string
    add esi, 3              ; but round up to next 4 byte boundary
    and esi, ~3
NEXT

; Same deal here - another primitive. This one uses a Linux syscall
; to print a string.
DEFCODE "TELL",TELL,0
    mov ebx, 1        ; 1st param: stdout
    pop edx        ; 3rd param: length of string
    pop ecx        ; 2nd param: address of string
    mov eax,__NR_write      ; write syscall
    int 80h
NEXT

; Turn a dictionary pointer into a codeword pointer.
; This is where we use the stored length of the word name
; to skip to the beginning of the code.
DEFCODE ">CFA",TCFA,0
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

; Turn a dictionary pointer into a "data" pointer.
; Data simply being the word addresses immediately
; following the codeword (4 bytes later).
DEFWORD ">DFA",TDFA,0
    dd TCFA                 ; get codeword address
    dd INCR4                ; advance 4 bytes
dd EXIT                 ; return from this word

; parse numeric literal from input using BASE as radix
DEFCODE "NUMBER",NUMBER,0
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

; esi always points to the next thing. Usually this is
; the next word. But in this case, it's the literal value
; to push onto the stack.
DEFCODE "LIT",LIT,0
    lodsd                   ; loads the value at esi into eax, increments esi
    push eax                ; push the literal number on to stack
NEXT

; Before this, we'll have called _WORD which pushed (returned):
;     ecx = length
;     edi = start of word (addr)
DEFCODE "FIND",FIND,0
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
    and al,(F_HIDDEN|F_LENMASK) ; al = length, but including hidden bit!
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
DEFCODE "CREATE",CREATE,0
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
DEFCODE ",",COMMA,0
    pop eax                ; Code pointer to store.
    call _COMMA
NEXT
    _COMMA:
    mov edi, [var_HERE]
    cmp edi, [var_CSTART]
    jl .oops
    cmp edi, [var_CEND]
    jg .oops
    stosd                  ; puts the value in eax at edi, increments edi
    mov [var_HERE], edi
    ret
.oops:
    nop

; LBRAC and RBRAC ([ and ])
; Simply toggle the STATE variable (0=immediate, 1=compile)
; So:
;       <compile mode> [ <immediate mode> ] <compile mode>
;
; Note that LBRAC has the immediate flag set because otherwise
; it would get compiled rather than switch modes then and there.
DEFCODE "[",LBRAC,F_IMMED
    xor eax, eax
    mov [var_STATE], eax      ; Set STATE to 0 (immediate)
NEXT

DEFCODE "]",RBRAC,0
    mov [var_STATE], word 1   ; Set STATE to 1 (compile)
NEXT

; HIDDEN toggles the hidden flag for the dictionary entry
; at the address on the stack
DEFCODE "HIDDEN",HIDDEN,0
    pop edi                 ; Dictionary entry, first byte is link
    add edi, 4              ; Move to name/flags byte.
    xor [edi], word F_HIDDEN  ; Toggle the HIDDEN bit in place.
NEXT


; +----------------------------------------------------------------------------+
; | COLON and SEMICOLON: The Compiler!                                         |
; +----------------------------------------------------------------------------+
; COLON (:) creates the new word header and starts compile mode
; It also sets the new definition to hidden so the word isn't
; discovered while it is being compiled.
DEFWORD ":",COLON,0
    dd FWORD                 ; Get the name of the new word
    dd CREATE               ; CREATE the dictionary entry / header
    dd LIT, DOCOL, COMMA    ; Append DOCOL  (the codeword).
    dd LATEST, FETCH, HIDDEN ; Make the word hidden while it's being compiled.
    ;dd LATEST, HIDDEN ; Make the word hidden while it's being compiled.
    dd RBRAC                ; Go into compile mode.
dd EXIT                 ; Return from the function.

; SEMICOLON (;) is an immediate word (F_IMMED) and it ends compile
; mode and unhides the word entry being compiled.
DEFWORD ";",SEMICOLON,F_IMMED
    dd LIT, EXIT, COMMA     ; Append EXIT (so the word will return).
    dd LATEST, FETCH, HIDDEN ; Unhide word now that it's been compiled.
    ;dd LATEST, HIDDEN ; Unhide word now that it's been compiled.
    dd LBRAC                ; Go back to IMMEDIATE mode.
dd EXIT                 ; Return from the function.

; EMIT just displays a character of output from the stack.
; It doesnt attempt to be efficient at all (no buffering, etc.)
DEFCODE "EMIT",EMIT,0
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

    ; DOT (temporary definiion) displays ascii decimal represention
    ; of numbers. Based on "echoi" proc written as part of asmtutor.com
    ; The real dot will be written as pure Forth later.
    DEFCODE ".",DOT,0
    pop eax
    call _DOT
    NEXT
_DOT:
    push esi ; preserve
    mov ecx, 0 ; counter of digits to print at the end
.divideloop:
    inc ecx
    mov edx, 0
    mov esi, 10
    idiv esi   ; divide eax by this
    add edx, 48 ; convert remainder to ascii digit
    push edx   ; push on stack to be echoed later (for correct order)
                ; what's clever about pushing the ascii digits onto the
                ; stack is that we can use the stack memory as our
                ; buffer by using the stack pointer at esp in our
                ; syscall to print the digits
    cmp eax, 0 ; are we done?
    jnz .divideloop
    mov esi, ecx ; printing... we use ecx for syscall, so count down with esi
.printloop:
    ; arguably, I should be making use of EMIT...but this is all temporary
    ; anyway so I'm just going to inline the syscall to print the digits...
    dec esi
    mov ebx, 1              ; syscall param 1: stdout
    mov ecx, esp            ; syscall param 2: address to print
    mov edx, 1              ; syscall param 3: length in bytes to print
    mov eax, __NR_write     ; syscall 'write'
    int 0x80                ; request syscall!
    pop eax      ; next digit
    cmp esi, 0   ; are we done?
    jnz .printloop
    pop esi      ;restore our word address pointer
    ret

    ; PRINTWORD
    ; Super killer debugging word! Prints the name of the word pointed to
    ; on the stack. Example: LATEST PRINTWORD
    DEFCODE "PRINTWORD",PRINTWORD,0
    pop eax
    call _PRINTWORD
    NEXT
_PRINTWORD:
    mov edx,eax             ; stack had addr of header of dictionary word
    xor eax,eax             ; zero out all of eax
    mov al, [edx+4]         ; al = flags+length field
    and al, F_LENMASK       ; al = just length of name
    add edx,5               ; move pointer to name string
    mov ebx,1               ; 1st param: stdout
    mov ecx,edx             ; 2nd param: address to print
    mov edx,eax             ; 3rd param: length of string
    mov eax,__NR_write      ; write syscall
    int 80h

; +----------------------------------------------------------------------------+
; | Stack manipulation words                                                   |
; +----------------------------------------------------------------------------+

; drop top of stack
DEFCODE "DROP",DROP,0
    pop eax
NEXT

; swap top two elements
DEFCODE "SWAP",SWAP,0
    pop eax
    pop ebx
    push eax
    push ebx
NEXT

; duplicate element on top of stack
DEFCODE "DUP",DUP,0
    mov eax, [esp]
    push eax
NEXT

; duplicate second element of stack to top
DEFCODE "OVER",OVER,0
    mov eax, [esp+4]
    push eax
NEXT

; rotate the top three items on stack (ABC -> BCA)
DEFCODE "ROT",ROT,0
    pop eax
    pop ebx
    pop ecx
    push ebx
    push eax
    push ecx
NEXT

; reverse rotate top three items on stack (ABC -> CAB)
DEFCODE "-ROT",NROT,0
    pop eax
    pop ebx
    pop ecx
    push eax
    push ecx
    push ebx
NEXT

; drop top two elements from stack
DEFCODE "2DROP",TWODROP,0
    pop eax
    pop eax
NEXT

; duplicate top two elements on stack
DEFCODE "2DUP",TWODUP,0
    mov eax, [esp]
    mov ebx, [esp + 4]
    push ebx
    push eax
NEXT

; swap top two pairs (ABCD -> CDAB)
DEFCODE "2SWAP",TWOSWAP,0
    pop eax
    pop ebx
    pop ecx
    pop edx
    push ebx
    push eax
    push edx
    push ecx
NEXT

; duplicate top element on stack if it's non-zero
DEFCODE "?DUP",QDUP,0
    mov eax, [esp]
    test eax, eax
    jz .skip
    push eax
.skip:
NEXT


; +----------------------------------------------------------------------------+
; | Math words                                                                 |
; +----------------------------------------------------------------------------+

DEFCODE "1+",INCR,0
    inc dword [esp]       ; increment top of stack
NEXT

DEFCODE "1-",DECR,0
    dec dword [esp]       ; decrement top of stack
NEXT

DEFCODE "4+",INCR4,0
    add dword [esp], 4    ; add 4 to top of stack
NEXT

DEFCODE "4-",DECR4,0
    sub dword [esp], 4   ; subtract 4 from top of stack
NEXT

DEFCODE "+",ADD,0
    pop eax       ; get top of stack
    add [esp], eax  ; and add it to next word on stack
NEXT

DEFCODE "-",SUB,0
    pop eax         ; get top of stack
    sub [esp], eax  ; and subtract it from next word on stack
NEXT

DEFCODE "*",MUL,0
    pop eax
    pop ebx
    imul eax, ebx
    push eax        ; ignore overflow
NEXT

; In JonesFORTH, /MOD is defined in asm. / and MOD will
; be defined later in FORTH. This is because i386 idiv
; gives us both the quotient and remainder.
DEFCODE "/MOD",DIVMOD,0
    xor edx, edx
    pop ebx
    pop eax
    idiv ebx
    push edx        ; push remainder
    push eax        ; push quotient
NEXT

; +----------------------------------------------------------------------------+
; | Comparison/conditional words                                               |
; +----------------------------------------------------------------------------+

DEFCODE "=",EQU,0      ;  top two values are equal?
    pop eax
    pop ebx
    cmp eax, ebx
    sete al          ; sete sets operand (al) to 1 if cmp was true
    movzx eax, al    ; movzx moves the value, then fills in zeros
    push eax         ; push answer on stack
NEXT
 
DEFCODE "<>",NEQU,0    ; top two words are not equal?
    pop eax
    pop ebx
    cmp eax, ebx
    setne al
    movzx eax, al
    push eax
NEXT
 
DEFCODE "<",LT,0
    pop eax
    pop ebx
    cmp ebx, eax
    setl al
    movzx eax, al
    push eax
NEXT
 
DEFCODE ">",GT,0
    pop eax
    pop ebx
    cmp ebx, eax
    setg al
    movzx eax, al
    push eax
NEXT
 
DEFCODE "<=",LE,0
    pop eax
    pop ebx
    cmp ebx, eax
    setle al
    movzx eax, al
    push eax
NEXT
 
DEFCODE ">=",GE,0
    pop eax
    pop ebx
    cmp ebx, eax
    setge al
    movzx eax, al
    push eax
NEXT
 
DEFCODE "0=",ZEQU,0    ; top of stack equals 0?
    pop eax
    test eax,eax
    setz al
    movzx eax, al
    push eax
NEXT
 
DEFCODE "0<>",ZNEQU,0    ; top of stack not 0?
    pop eax
    test eax,eax
    setnz al
    movzx eax, al
    push eax
NEXT
 
DEFCODE "0<",ZLT,0    ; greater than zero
    pop eax
    test eax,eax
    setl al
    movzx eax, al
    push eax
NEXT
 
DEFCODE "0>",ZGT,0   ; less than zero
    pop eax
    test eax,eax
    setg al
    movzx eax, al
    push eax
NEXT
 
DEFCODE "0<=",ZLE,0
    pop eax
    test eax,eax
    setle al
    movzx eax,al
    push eax
NEXT
 
DEFCODE "0>=",ZGE,0
    pop eax
    test eax,eax
    setge al
    movzx eax,al
    push eax
NEXT

; +----------------------------------------------------------------------------+
; | Bitwise logic words                                                        |
; +----------------------------------------------------------------------------+

DEFCODE "AND",AND,0
    pop eax
    and [esp],eax
NEXT

DEFCODE "OR",OR,0
    pop eax
    or [esp],eax
NEXT

DEFCODE "XOR",XOR,0
    pop eax
    xor [esp], eax
NEXT

DEFCODE "INVERT",INVERT,0
    not dword [esp]
NEXT

; +----------------------------------------------------------------------------+
; | Primitive memory words                                                     |
; +----------------------------------------------------------------------------+

DEFCODE "!",STORE,0
    pop ebx           ; address to store at
    pop eax           ; data to store there
    mov [ebx], eax
NEXT

DEFCODE "@",FETCH,0
    pop ebx                 ; address to fetch
    mov eax, [ebx]          ; fetch it
    push eax                ; push value onto stack
NEXT

DEFCODE "+!",ADDSTORE,0
    pop ebx                ; address
    pop eax                ; the amount to add
    add [ebx], eax
NEXT

DEFCODE "-!",SUBSTORE,0
    pop ebx                ; address
    pop eax                ; the amount to subtract
    sub [ebx], eax
NEXT

; Primitive byte-oriented operations are like the above 32-bit
; operations, but work on 8 bits. x86 has instructions for this
; so we can define these.
DEFCODE "C!",STOREBYTE,0
    pop ebx                ; address to store at
    pop eax                ; data to store there
    mov [ebx], al
NEXT

DEFCODE "C@",FETCHBYTE,0
    pop ebx               ; address to fetch
    xor eax, eax          ; clear the register
    mov al, [ebx]         ; grab a byte
    push eax
NEXT

DEFCODE "C@C!",CCOPY,0 ; byte copy
    mov ebx, [esp+4]      ; source address
    mov al, [ebx]         ; source byte
    pop edi               ; destination address
    stosb                 ; copy to destination
    push edi              ; increment destination address
    inc byte [esp+4]      ; increment source address
NEXT

DEFCODE "CMOVE",CMOVE,0 ; copy n bytes
    mov edx, esi          ; preserve esi
    pop ecx               ; length
    pop edi               ; destination address
    pop esi               ; source address
    rep movsb             ; copy source to destination
    mov esi, edx          ; restore esi
NEXT

; +----------------------------------------------------------------------------+
; | Return stack manipulation words                                            |
; +----------------------------------------------------------------------------+
; ebp is the return stack pointer (RSP)
; In traditional Forth implementations, you're encouraged to put temporary
; values on the return stack (and you'd better not forget to clean up after
; yourself! Can you imagine proposing that to someone today? You'd be burned
; at the stake as a heretic!

DEFCODE ">R",TOR,0  ; move value from param stack to return stack
    pop eax
    PUSHRSP eax
NEXT

DEFCODE "R>",FROMR,0 ; move value from return stack to param stack
    POPRSP eax
    push eax
NEXT

DEFCODE "RSP@",RSPFETCH,0 ; get the actual address RSP points to
    push ebp
NEXT

DEFCODE "RSP!",RSPSTORE,0 ; set the address RSP points to
    pop ebp
NEXT

DEFCODE "RDROP",RDROP,0 ; move RSP to "pop" value and throw it away
    add ebp, 4
NEXT

; +----------------------------------------------------------------------------+
; | Param stack manipulation words                                             |
; +----------------------------------------------------------------------------+
; esp is the param (or "data" or "main") stack pointer (DSP)

DEFCODE "DSP@",DSPFETCH,0
    mov eax, esp
    push eax
NEXT

DEFCODE "DSP!",DSPSTORE,0
    pop esp
NEXT

; +----------------------------------------------------------------------------+
; | Misc words needed for interpreter/compiler                                 |
; +----------------------------------------------------------------------------+

DEFCODE "IMMEDIATE",IMMEDIATE,F_IMMED ; makes latest word immediate
    mov edi, [var_LATEST]     ; addr of LATEST word.
    add edi, 4                ; Point to name/flags byte.
    xor byte [edi], F_IMMED   ; Toggle the IMMED bit.
NEXT

DEFWORD "HIDE",HIDE,0
    dd FWORD        ; Get the word (after HIDE).
    dd FIND        ; Look up in the dictionary.
    dd HIDDEN      ; Set F_HIDDEN flag.
dd EXIT        ; Return.

DEFCODE "CHAR",CHAR,0
    call _WORD              ; Returns %ecx = length, %edi = pointer to word.
    xor eax,eax
    mov al,[edi]            ; Get the first character of the word.
    push eax                ; Push it onto the stack.
NEXT

DEFCODE "EXECUTE",EXECUTE,0
    pop eax                ; Get xt into %eax
    jmp [eax]              ; and jump to it. After xt runs its NEXT will
                           ; continue executing the current word.

DEFCODE "SYSCALL3",SYSCALL3,0
    pop eax                ; System call number (see <asm/unistd.h>)
    pop ebx                ; First parameter.
    pop ecx                ; Second parameter
    pop edx                ; Third parameter
    int 80h
    push eax               ; Result (negative for -errno)
NEXT

DEFCODE "SYSCALL2",SYSCALL2,0
    pop eax                ; System call number (see <asm/unistd.h>)
    pop ebx                ; First parameter.
    pop ecx                ; Second parameter
    int 80h
    push eax               ; Result (negative for -errno)
NEXT

DEFCODE "SYSCALL1",SYSCALL1,0
    pop eax                ; System call number (see <asm/unistd.h>)
    pop ebx                ; First parameter.
    int 80h
    push eax               ; Result (negative for -errno)
NEXT

DEFCODE "SYSCALL0",SYSCALL0,0
    pop eax                ; System call number (see <asm/unistd.h>)
    int 80h
    push eax               ; Result (negative for -errno)
NEXT

; +----------------------------------------------------------------------------+
; | Forth constants                                                            |
; +----------------------------------------------------------------------------+
;
;  VERSION      Is the current version of this FORTH.
;  R0           The address of the top of the return stack.
;  DOCOL        Pointer to DOCOL.
;  F_IMMED      The IMMEDIATE flag's actual value.
;  F_HIDDEN     The HIDDEN flag's actual value.
;  F_LENMASK    The length mask in the flags/len byte.
;  SYS_*        and the numeric codes of various Linux syscalls
;
; Check it out! A const is just a word that pushes a value!
%macro DEFCONST 4 ; 1=name 2=label 3=flags 4=value
    DEFCODE %1,%2,%3
    push %4
    NEXT
%endmacro

DEFCONST "VERSION",VERSION,0,NASMJF_VERSION
DEFCONST "R0",R0,0,return_stack_top
DEFCONST "DOCOL",__DOCOL,0,DOCOL
DEFCONST "F_IMMED",__F_IMMED,0,F_IMMED
DEFCONST "F_HIDDEN",__F_HIDDEN,0,F_HIDDEN
DEFCONST "F_LENMASK",__F_LENMASK,0,F_LENMASK

DEFCONST "SYS_EXIT",SYS_EXIT,0,__NR_exit
DEFCONST "SYS_OPEN",SYS_OPEN,0,__NR_open
DEFCONST "SYS_CLOSE",SYS_CLOSE,0,__NR_close
DEFCONST "SYS_READ",SYS_READ,0,__NR_read
DEFCONST "SYS_WRITE",SYS_WRITE,0,__NR_write
DEFCONST "SYS_CREAT",SYS_CREAT,0,__NR_creat
DEFCONST "SYS_BRK",SYS_BRK,0,__NR_brk

DEFCONST "O_RDONLY",__O_RDONLY,0,0
DEFCONST "O_WRONLY",__O_WRONLY,0,1
DEFCONST "O_RDWR",__O_RDWR,0,2
DEFCONST "O_CREAT",__O_CREAT,0,0100
DEFCONST "O_EXCL",__O_EXCL,0,0200
DEFCONST "O_TRUNC",__O_TRUNC,0,01000
DEFCONST "O_APPEND",__O_APPEND,0,02000
DEFCONST "O_NONBLOCK",__O_NONBLOCK,0,04000

; +----------------------------------------------------------------------------+
; | Forth built-in variables                                                   |
; +----------------------------------------------------------------------------+
;
;   STATE   Is the interpreter executing code (0) or compiling a word (non-zero)?
;   LATEST  Points to the latest (most recently defined) word in the dictionary.
;   HERE    Points to the next free byte of memory.  When compiling, compiled words go here.
;   S0      Stores the address of the top of the parameter stack.
;   BASE    The current base for printing and reading numbers.
;  
; A variable is a word that leaves its *address* on the stack. Use '@' and '!' to
; read or write a *value* at that address.
%macro DEFVAR 4 ; 1=name 2=label 3=flags 4=value
        DEFCODE %1,%2,%3
        push dword var_%2
    NEXT
    section .data
        align 4
    var_%2:   ; Give it an asm label. Example: var_SZ for 'S0'
        dd %4 ; note dd to reserve a "double" (4b)
%endmacro

DEFVAR "STATE",STATE,0,0
DEFVAR "HERE",HERE,0,0
DEFVAR "S0",SZ,0,0
DEFVAR "BASE",BASE,0,10
DEFVAR "CSTART",CSTART,0,0
DEFVAR "CEND",CEND,0,0
DEFVAR "READFROM",READFROM,0,read_from_fd ; LOADJF - make available to Forth???
DEFVAR "LATEST",LATEST,0,name_LATEST      ; points to last word defined...which will just
                                          ; happen to be self. We'll see if this works.

; +----------------------------------------------------------------------------+
; | Data section - reserve memory for interpreter use                          |
; +----------------------------------------------------------------------------+
;
; db - "define byte(s)"
; dd - "define double"   (4 bytes)
;
SECTION    .data
    align 4
currkey: dd 0          ; Current place in input buffer (next character to read).
bufftop: dd 0          ; Last valid data in input buffer + 1.
interpret_is_lit: dd 0 ; 1 means "reading a literal"
read_from_fd: dd 0     ; 0=STDIN, etc.
errmsg: db "PARSE ERROR: "
errmsgend:
errmsgnl: db `\n`
loadjf_fail_msg: db "ERROR Could not open '" ; LOADJF
loadjf_fail_msg_end:                         ; LOADJF
loadjf_fail_msg2: db "'."                    ; LOADJF
db `\n`                                      ; LOADJF
loadjf_fail_msg_end2:                        ; LOADJF
