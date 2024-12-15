; -------------------------------------------------------------------
; 80386
; 32-bit x86 assembly language
; TASM
;
; author:	David Blinder
; date:		23/10/2017
; program:	IDEAL Function calls.
; -------------------------------------------------------------------

IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT

; -------------------------------------------------------------------
; CODE
; -------------------------------------------------------------------
CODESEG	

PROC setupBitboards
    ; PAWNS
    ; WHITE
    mov eax, 00000000h ; Higher 32 bits 
    ; 00000000 00000000 00000000 00000000
    ; 0
    mov ebx, 0000FF00h ; Lower 32 bits (pawns at rank 2)
    ; 00000000 00000000 11111111 00000000
    ; 65280

    ; Store the bitboards
    mov [white_pawns_high], eax
    mov [white_pawns_low], ebx

    ; BLACK
    mov eax, 00FF0000h ; Higher 32 bits (pawns at rank 7)
    ; 00000000 11111111 00000000 00000000
    ; 16711680
 
    mov ebx, 00000000h ; Lower 32 bits
    ; 00000000 00000000 00000000 00000000
    ; 0

    ; Store the bitboards
    mov [black_pawns_high], eax
    mov [black_pawns_low], ebx

    ; KNIGHTS
    ; WHITE
    mov eax, 00000000h ; Higher 32 bits
    ; 00000000 00000000 00000000 00000000
    ; 0

    mov ebx, 00000042h ; Low 32 bits (knights at B1 and G1)
    ; 00000000 00000000 00000000 01000010 
    ; 66

    mov [white_knights_high], eax
    mov [white_knights_low], ebx

    ; BLACK
    mov eax, 42000000h ; High 32 bits (knights at B8 and G8)
    ; 01000010 00000000 00000000 00000000
    ; 1107296256

    mov ebx, 00000000h ; Low 32 bits
    ; 00000000 00000000 00000000 00000000
    ; 0

    mov [black_knights_high], eax
    mov [black_knights_low], ebx

    ; BISHOPS
    ; WHITE
    mov eax, 00000000h ; Higher 32 bits
    ; 00000000 00000000 00000000 00000000
    ; 0

    mov ebx, 00000024h ; Low 32 bits (bishops at C1 and F1)
    ; 00000000 00000000 00000000 00100100 
    ; 36

    mov [white_bishops_high], eax
    mov [white_bishops_low], ebx

    ; BLACK
    mov eax, 24000000h ; High 32 bits (bishops at C8 and F8)
    ; 00100100 00000000 00000000 00000000
    ; 603979776

    mov ebx, 00000000h ; Low 32 bits
    ; 00000000 00000000 00000000 00000000
    ; 0

    mov [black_bishops_high], eax
    mov [black_bishops_low], ebx

    ; ROOKS
    ; WHITE
    mov eax, 00000000h ; Higher 32 bits
    ; 00000000 00000000 00000000 00000000
    ; 0

    mov ebx, 00000081h ; Low 32 bits (rooks at A1 and H1)
    ; 00000000 00000000 00000000 10000001
    ; 129

    mov [white_rooks_high], eax
    mov [white_rooks_low], ebx

    ; BLACK
    mov eax, 81000000h ; High 32 bits (rooks at A8 and H8)
    ; 10000001 00000000 00000000 00000000
    ; 2164260864

    mov ebx, 00000000h ; Low 32 bits
    ; 00000000 00000000 00000000 00000000
    ; 0

    mov [black_rooks_high], eax
    mov [black_rooks_low], ebx

    ; QUEENS
    ; WHITE
    mov eax, 00000000h ; Higher 32 bits
    ; 00000000 00000000 00000000 00000000
    ; 0

    mov ebx, 00000008h ; Low 32 bits (queen at D1)
    ; 00000000 00000000 00000000 00001000
    ; 8

    mov [white_queens_high], eax
    mov [white_queens_low], ebx

    ; BLACK

    mov eax, 08000000h ; High 32 bits (queen at D8)
    ; 00001000 00000000 00000000 00000000
    ; 134217728

    mov ebx, 00000000h ; Low 32 bits
    ; 00000000 00000000 00000000 00000000
    ; 0

    mov [black_queens_high], eax
    mov [black_queens_low], ebx

    ; KINGS
    ; WHITE

    mov eax, 00000000h ; Higher 32 bits
    ; 00000000 00000000 00000000 00000000
    ; 0

    mov ebx, 00000010h ; Low 32 bits (king at E1)
    ; 00000000 00000000 00000000 00010000
    ; 16

    mov [white_kings_high], eax
    mov [white_kings_low], ebx

    ; BLACK

    mov eax, 10000000h ; High 32 bits (king at E8)
    ; 00010000 00000000 00000000 00000000
    ; 268435456

    mov ebx, 00000000h ; Low 32 bits
    ; 00000000 00000000 00000000 00000000
    ; 0

    mov [black_kings_high], eax
    mov [black_kings_low], ebx

    ret
ENDP setupBitboards

PROC printBitboards
call printUnsignedNumber, [white_pawns_high]
    call printNewline

    call printUnsignedNumber, [white_pawns_low]
    call printNewline

    call printUnsignedNumber, [black_pawns_high]
    call printNewline

    call printUnsignedNumber, [black_pawns_low]
    call printNewline

    call printUnsignedNumber, [white_knights_high]
    call printNewline

    call printUnsignedNumber, [white_knights_low]
    call printNewline

    call printUnsignedNumber, [black_knights_high]
    call printNewline

    call printUnsignedNumber, [black_knights_low]
    call printNewline

    call printUnsignedNumber, [white_bishops_high]
    call printNewline

    call printUnsignedNumber, [white_bishops_low]
    call printNewline

    call printUnsignedNumber, [black_bishops_high]
    call printNewline

    call printUnsignedNumber, [black_bishops_low]
    call printNewline

    call printUnsignedNumber, [white_rooks_high]
    call printNewline

    call printUnsignedNumber, [white_rooks_low]
    call printNewline

    call printUnsignedNumber, [black_rooks_high]
    call printNewline

    call printUnsignedNumber, [black_rooks_low]
    call printNewline

    call printUnsignedNumber, [white_queens_high]
    call printNewline

    call printUnsignedNumber, [white_queens_low]
    call printNewline

    call printUnsignedNumber, [black_queens_high]
    call printNewline

    call printUnsignedNumber, [black_queens_low]
    call printNewline

    call printUnsignedNumber, [white_kings_high]
    call printNewline

    call printUnsignedNumber, [white_kings_low]
    call printNewline

    call printUnsignedNumber, [black_kings_high]
    call printNewline

    call printUnsignedNumber, [black_kings_low]
    call printNewline

    ret

ENDP printBitboards

PROC determineColour
    ARG @@xcoor:dword, @@ycoor:dword
    LOCAL @@tilex:dword, @@tiley:dword

    ; load x coordinate
    mov eax, [dword ptr @@xcoor]

    ; subtract padding
    mov ecx, PADDING
    ;sub eax, ecx


    ; divide by 25 to get the tile number
    mov edx, TILESIZE
    div edx
    ;mov [@@tilex], ax

    ; load y coordinate
    ;mov ax, [word ptr @@ycoor]
    ;mov dx, TILESIZE
    ;div dx
    ;mov [@@tiley], ax
;
    ;; check if the tile is black brown or white
    ;mov ax, [@@tilex]
    ;mov bx, [@@tiley]
    ;add ax, bx
    ;and ax, 1
    ;;check if ax is negative
    ;js @@black
;
    ;;check if even
    ;jnz @@white    ; Jump if not zero (odd number)
    ;jmp @@brown

@@black:
    mov [colour_to_draw], 0

    ret

@@white:
    mov [colour_to_draw], 15
    ret

@@brown:
    mov [colour_to_draw], 6
    ret

ENDP determineColour


PROC drawEmptyBoard
    call determineColour, 100, 0
    ret
ENDP drawEmptyBoard






PROC main
    sti                ; Enable interrupts.
    cld                ; Clear direction flag.

    VMEMADR EQU 0A0000h    ; Video memory address
    SCRWIDTH EQU 320       ; Screen width for mode 13h
    SCRHEIGHT EQU 200      ; Screen height

    PADDING EQU 60         ; black pixels left and right since 320x200 is not 1:1
    BOARDDIMENSION EQU 200 
    TILESIZE EQU 25 
    DEBUG EQU 1

    EXTRN printUnsignedNumber:PROC
    EXTRN printNewline:PROC

    ; Set video mode 13h
    mov AL, DEBUG
    cmp AL, 1
    je skip_video
    mov AX, 13h
    int 10h
skip_video:

    ; Set start of video memory
    mov EDI, VMEMADR
    
    call setupBitboards

    mov AL, DEBUG
    cmp AL, 1
    jne skip_print_bitboards
    call printBitboards

    call determineColour, 100, 0
    call printUnsignedNumber, [colour_to_draw]
skip_print_bitboards:
    



    
wait_for_key:
    ; Wait for keystroke
    mov ah, 0h
    int 16h
    ;text mode
    mov ax, 3
    int 10h

    ; Terminate program
    mov ax, 4C00h
    int 21h

ENDP main

; -------------------------------------------------------------------
; DATA
; -------------------------------------------------------------------
DATASEG

; Bitboards for pawns (64 bits each)
white_pawns_high DD 0 ; High 32 bits 
white_pawns_low  DD 0 ; Low 32 bits
black_pawns_high DD 0 ; High 32 bits
black_pawns_low  DD 0 ; Low 32 bits
white_knights_high DD 0 ; High 32 bits
white_knights_low  DD 0 ; Low 32 bits
black_knights_high DD 0 ; High 32 bits
black_knights_low  DD 0 ; Low 32 bits
white_bishops_high DD 0 ; High 32 bits
white_bishops_low  DD 0 ; Low 32 bits
black_bishops_high DD 0 ; High 32 bits
black_bishops_low  DD 0 ; Low 32 bits
white_rooks_high DD 0 ; High 32 bits
white_rooks_low  DD 0 ; Low 32 bits
black_rooks_high DD 0 ; High 32 bits
black_rooks_low  DD 0 ; Low 32 bits
white_queens_high DD 0 ; High 32 bits
white_queens_low  DD 0 ; Low 32 bits
black_queens_high DD 0 ; High 32 bits
black_queens_low  DD 0 ; Low 32 bits
white_kings_high DD 0 ; High 32 bits
white_kings_low  DD 0 ; Low 32 bits
black_kings_high DD 0 ; High 32 bits
black_kings_low  DD 0 ; Low 32 bits
colour_to_draw DD 0


    

; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 200h

END main
