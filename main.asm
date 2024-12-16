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
    USES eax, ebx, ecx, edx
    ARG @@xcoor:dword, @@ycoor:dword
    LOCAL @@tilex:dword, @@tiley:dword

    ; load x coordinate
    mov eax, [dword ptr @@xcoor]
    ; if x is less than padding, it is black
    cmp eax, PADDING
    jl @@black

    ; if x is greater than padding + board dimension, it is black
    mov eax, [dword ptr @@xcoor]
    mov ecx, BOARDDIMENSION
    add ecx, PADDING
    cmp eax, ecx
    jge @@black

    ; subtract padding from x
    mov ecx, PADDING
    sub eax, ecx

    ; divide by 25 to get the tile number
    mov ebx, TILESIZE
    xor edx, edx
    div ebx
    mov [@@tilex], eax

    ; load y coordinate
    mov eax, [dword ptr @@ycoor]

    ; divide by 25 to get the tile number
    xor edx, edx
    div ebx
    mov [@@tiley], eax

    ; check if the tile is black brown or white
    mov eax, [@@tilex]
    mov ebx, [@@tiley]
    add eax, ebx
    and eax, 1
    jnz @@brown    ; Jump if not zero (odd number)
    jmp @@white    ; Jump if zero (even number)

@@black:
    mov [colour_to_draw], 0
    ret

@@white:
    mov [colour_to_draw], 1
    ret

@@brown:
    mov [colour_to_draw], 2
    ret

ENDP determineColour


PROC drawEmptyBoard
    uses ecx, edx, edi, eax
    mov ecx, 0             ; Start with y = 0
    ;mov edi to start of video_buffer
    mov edi, offset _screenBuffer
    sub edi, 0

outer_loop:
    cmp ecx, 200           ; Stop after reaching y = 200 (height of the screen)
    jge end_outer_loop     ; If y >= 200, exit the loop
    
    ; Inner loop: x-coordinate (columns)
    mov edx, 0             ; Start with x = 0
inner_loop:
    cmp edx, 320           ; Stop after reaching x = 320 (width of the screen)
    jge end_inner_loop     ; If x >= 320, exit the loop

    ; Call determineColour for current (x, y)
    call determineColour, edx, ecx    ; Call the function with (x, y)
    mov eax, [colour_to_draw] ; Load the colour to draw
    mov [edi], al          ; Store the colour in the video memory
    inc edi                 ; Move to the next pixel

    inc edx                 ; Increment x-coordinate (move to next pixel in row)
    jmp inner_loop          ; Repeat for the next column

end_inner_loop:
    inc ecx                 ; Increment y-coordinate (move to next row)
    jmp outer_loop          ; Repeat for the next row

end_outer_loop:
    ret                     ; Return from the function
ENDP drawEmptyBoard

;PROC copyBackgroundToBuffer
;    USES esi, edi, ecx, eax
;    cld
;    mov esi , offset background_buffer ; points to a "db 64000 dup ( ? ) " array
;    mov edi , VMEMADR ; the video memory
;    mov ecx , 64000 / 4 ; 320 * 200 , but copy groups four b y t e s
;    rep movsd ; moves a dword and updates ecx , e s i and edi
;    ret
;ENDP copyBackgroundToBuffer

PROC copyBufferToVideoMemory
    USES esi, edi, ecx
    cld
    Lea esi, [offset _screenBuffer] ; points to a "db 64000 dup ( ? ) " array
    mov edi, VMEMADR ; the video memory
    mov ecx, 64000 / 4 ; 320 * 200 , but copy groups four b y t e s
    rep movsd ; moves a dword and updates ecx , e s i and edi
    ret
ENDP copyBufferToVideoMemory

PROC copyBufferToVideoMemoryLoop
    ; Set up segment registers
    mov esi, OFFSET _screenBuffer ; Source pointer
    mov edi, VMEMADR             ; Destination pointer
    mov ecx, 64000               ; Number of bytes to copy

@@loop:
    cmp ecx, 0                   ; Check if counter is zero
    je @@copyBufferToVideoMemoryLoopEnd
    mov al, [esi]                ; Load a byte from source into AL
    mov [edi], al                ; Store the byte in destination
    inc esi                      ; Increment source pointer
    inc edi                      ; Increment destination pointer
    dec ecx                      ; Decrement counter
    jmp @@loop

@@copyBufferToVideoMemoryLoopEnd:
    ret                          ; Return from procedure
ENDP copyBufferToVideoMemoryLoop

PROC drawSprite
    ARG @@spritePtr:dword, @@dstPtr:dword, @@x:dword, @@y:dword
    LOCAL @@w:dword, @@h:dword
    USES eax, ebx, ecx, edx, esi, edi

    ; Load sprite pointer and extract width/height
    mov esi, [@@spritePtr]    ; Load sprite pointer into ESI
    xor eax, eax
    lodsw                     ; Read sprite width (2 bytes) into AX
    mov [@@w], eax            ; Store width in local variable
    lodsw                     ; Read sprite height (2 bytes) into AX
    mov [@@h], eax            ; Store height in local variable

    ; Calculate destination starting pointer (vertical positioning)
    mov edi, [@@dstPtr]       ; Base address of destination buffer
    mov eax, [@@y]            ; Load y-coordinate
    inc eax                   ; Adjust for starting at tile row (y + 1)
    mov ebx, TILESIZE
    mul ebx                   ; eax = (y + 1) * TILESIZE
    sub eax, [@@h]            ; Adjust for sprite height
    mov ebx, SCRWIDTH         ; Screen width
    mul ebx                   ; eax = ((y + 1) * TILESIZE - sprite height) * SCRWIDTH
    add edi, eax              ; Add vertical offset to destination pointer

    ; Calculate horizontal starting pointer (x positioning)
    mov eax, [@@x]            ; Load x-coordinate
    inc eax                   ; Adjust for starting at tile column (x + 1)
    mov ebx, TILESIZE
    mul ebx                   ; eax = (x + 1) * TILESIZE
    add eax, PADDING          ; Add padding to the x-offset
    sub eax, [@@w]            ; Adjust for sprite width
    mov ebx, TILESIZE
    sub ebx, [@@w]            ; ebx = TILESIZE - sprite width
    shr ebx, 1                ; ebx = (TILESIZE - sprite width) / 2
    sub eax, ebx              ; Center the sprite within the tile
    add edi, eax              ; Add horizontal offset to destination pointer

    ; Loop through each line of the sprite
    mov edx, [@@h]            ; edx = sprite height (rows to draw)
@@drawLine:
    push edx                  ; Save outer loop counter (row count)
    mov ecx, [@@w]            ; ecx = sprite width (pixels per row)
@@drawPixel:
    mov al, [esi]             ; Load a pixel from the sprite (source buffer)
    cmp al, 9                 ; Check if the pixel is transparent (value 9)
    je @@skipPixel            ; Skip writing if transparent
    mov [edi], al             ; Write the pixel to the destination buffer
@@skipPixel:
    inc esi                   ; Advance sprite pointer
    inc edi                   ; Advance destination pointer
    dec ecx                   ; Decrement pixel counter
    jnz @@drawPixel           ; Repeat for all pixels in the row

    ; Move to the next row
    pop edx                   ; Restore row counter
    add edi, SCRWIDTH         ; Move to the next screen row
    sub edi, [@@w]            ; Adjust for sprite width
    dec edx                   ; Decrement row counter
    jnz @@drawLine            ; Repeat for all rows in the sprite

    ret                       ; Return from procedure
ENDP drawSprite


PROC main
    sti                ; Enable interrupts.
    cld                ; Clear direction flag.

    VMEMADR EQU 0A0000h    ; Video memory address
    BUFFERADR EQU 0B0000h  ; Buffer address
    SCRWIDTH EQU 320       ; Screen width for mode 13h
    SCRHEIGHT EQU 200      ; Screen height

    PADDING EQU 60         ; black pixels left and right since 320x200 is not 1:1
    BOARDDIMENSION EQU 200 
    TILESIZE EQU 25 
    DEBUG EQU 0

    EXTRN printUnsignedNumber:PROC
    EXTRN printNewline:PROC
    EXTRN updateColourPalette:PROC
    EXTRN setVideoMode:PROC

    ; Setup graphics
    mov AL, DEBUG
    cmp AL, 1
    je skip_video
    call setVideoMode, 13h
    call updateColourPalette, 5
skip_video:

    ; Set start of video memory
    mov EDI, VMEMADR
    
    call setupBitboards

    mov AL, DEBUG
    cmp AL, 1
    jne skip_print_bitboards
    call printBitboards
skip_print_bitboards:

    mov AL, DEBUG
    cmp AL, 1
    je skip_draw_board
    call drawEmptyBoard 
    call copyBufferToVideoMemoryLoop 

    ;call copyBackgroundToBuffer
    call drawSprite, offset _bishop_black, offset _screenBuffer, 0, 0
    call copyBufferToVideoMemoryLoop 

    call drawSprite, offset _king_black, offset _screenBuffer, 1, 0
    call copyBufferToVideoMemoryLoop 

    call drawSprite, offset _knight_black, offset _screenBuffer, 2, 0
    call copyBufferToVideoMemoryLoop 

    call drawSprite, offset _pawn_black, offset _screenBuffer, 3, 0
    call copyBufferToVideoMemoryLoop 

    call drawSprite, offset _queen_black, offset _screenBuffer, 4, 0
    call copyBufferToVideoMemoryLoop 

    call drawSprite, offset _rook_black, offset _screenBuffer, 5, 0
    call copyBufferToVideoMemoryLoop 


    call drawSprite, offset _bishop_white, offset _screenBuffer, 0, 1
    call copyBufferToVideoMemoryLoop 

    call drawSprite, offset _king_white, offset _screenBuffer, 1, 1
    call copyBufferToVideoMemoryLoop 

    call drawSprite, offset _knight_white, offset _screenBuffer, 2, 1
    call copyBufferToVideoMemoryLoop 

    call drawSprite, offset _pawn_white, offset _screenBuffer, 3, 1
    call copyBufferToVideoMemoryLoop 

    call drawSprite, offset _queen_white, offset _screenBuffer, 4, 1
    call copyBufferToVideoMemoryLoop 

    call drawSprite, offset _rook_white, offset _screenBuffer, 7, 7
    call copyBufferToVideoMemoryLoop 


skip_draw_board:
    


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
_screenBuffer db 64000 dup(?) ; Video buffer
background_buffer db 64000 dup(?) ; background buffer


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


_bishop_black   dw 21, 24
                db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9
                db 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9
                db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 0, 0, 9, 9
                db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 0, 0, 9, 9
                db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 3, 3, 0, 0, 9, 9
                db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 3, 3, 0, 0, 9, 9
                db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
                db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
                db 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9
                db 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9
                db 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9
                db 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0
                db 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0
                db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

_king_black dw 23, 24
            db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
            db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
            db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9
            db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9
            db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9
            db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9
            db 9, 9, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 9, 9
            db 9, 9, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 9, 9
            db 0, 0, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 3, 0, 0, 3, 3, 3, 3, 3, 3, 0, 0
            db 0, 0, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 3, 0, 0, 3, 3, 3, 3, 3, 3, 0, 0
            db 0, 0, 3, 3, 3, 3, 0, 0, 3, 3, 0, 0, 0, 3, 3, 0, 0, 3, 3, 3, 3, 0, 0
            db 0, 0, 3, 3, 3, 3, 0, 0, 3, 3, 0, 0, 0, 3, 3, 0, 0, 3, 3, 3, 3, 0, 0
            db 0, 0, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 3, 0, 0, 3, 3, 3, 3, 3, 3, 0, 0
            db 0, 0, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 3, 0, 0, 3, 3, 3, 3, 3, 3, 0, 0
            db 0, 0, 3, 3, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 0, 0
            db 0, 0, 3, 3, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 0, 0
            db 9, 9, 0, 0, 3, 3, 0, 0, 0, 0, 3, 3, 3, 0, 0, 0, 0, 3, 3, 0, 0, 9, 9
            db 9, 9, 0, 0, 3, 3, 0, 0, 0, 0, 3, 3, 3, 0, 0, 0, 0, 3, 3, 0, 0, 9, 9
            db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
            db 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9
            db 9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9, 9
            db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
            db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
            db 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9

_knight_black   dw 21, 24
                      db 9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9
                      db 9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9
                      db 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 9, 9, 9, 9
                      db 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
                      db 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
                      db 9, 9, 9, 9, 0, 0, 3, 3, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0
                      db 9, 9, 9, 9, 0, 0, 3, 3, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0
                      db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0
                      db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0
                      db 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 3, 3, 3, 0, 0
                      db 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 3, 3, 3, 0, 0
                      db 0, 0, 3, 3, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 3, 3, 3, 3, 3, 0, 0
                      db 0, 0, 3, 3, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 3, 3, 3, 3, 3, 0, 0
                      db 9, 9, 0, 0, 0, 0, 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 0, 0
                      db 9, 9, 0, 0, 0, 0, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0
                      db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0
                      db 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
                      db 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
                      db 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
                      db 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
                      db 9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9
                      db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0
                      db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0
                      db 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

_pawn_black dw 17, 23
            db 9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9, 9
            db 9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9, 9
            db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
            db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
            db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
            db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
            db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
            db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
            db 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9
            db 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9
            db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
            db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
            db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
            db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
            db 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9
            db 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9
            db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
            db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
            db 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0
            db 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0
            db 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0
            db 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0
            db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

_queen_black    dw 23, 24
                db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
                db 0, 0, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 0, 0
                db 0, 0, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 0, 0
                db 0, 0, 0, 0, 9, 9, 9, 9, 9, 9, 0, 0, 0, 9, 9, 9, 9, 9, 9, 0, 0, 0, 0
                db 0, 0, 0, 0, 9, 9, 9, 9, 9, 9, 0, 0, 0, 9, 9, 9, 9, 9, 9, 0, 0, 0, 0
                db 0, 0, 3, 3, 0, 0, 9, 9, 0, 0, 3, 3, 3, 0, 0, 9, 9, 0, 0, 3, 3, 0, 0
                db 0, 0, 3, 3, 0, 0, 9, 9, 0, 0, 3, 3, 3, 0, 0, 9, 9, 0, 0, 3, 3, 0, 0
                db 0, 0, 3, 3, 3, 3, 0, 0, 3, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 3, 3, 0, 0
                db 0, 0, 3, 3, 3, 3, 0, 0, 3, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 3, 3, 0, 0
                db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
                db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
                db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
                db 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9
                db 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9
                db 9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9, 9
                db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
                db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
                db 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9

_rook_black   dw 23, 24
                      db 9, 9, 0, 0, 0, 0, 0, 0, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9
                      db 9, 9, 0, 0, 0, 0, 0, 0, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9
                      db 9, 9, 0, 0, 3, 3, 0, 0, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
                      db 9, 9, 0, 0, 3, 3, 0, 0, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
                      db 9, 9, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
                      db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
                      db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
                      db 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9
                      db 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9
                      db 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9
                      db 9, 9, 9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9, 9, 9, 9
                      db 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9, 9, 9
                      db 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9, 9, 9
                      db 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9, 9, 9
                      db 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9, 9, 9
                      db 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9, 9, 9
                      db 9, 9, 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9, 9, 9
                      db 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9
                      db 9, 9, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9, 9, 9
                      db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
                      db 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 9, 9
                      db 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0
                      db 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0
                      db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0


_bishop_white   dw 21, 24
                db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9
                db 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9
                db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 4, 4, 0, 0, 9, 9
                db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 4, 4, 0, 0, 9, 9
                db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 0, 0, 4, 4, 4, 4, 0, 0, 9, 9
                db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 0, 0, 4, 4, 4, 4, 0, 0, 9, 9
                db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
                db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
                db 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9
                db 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9
                db 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9
                db 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0
                db 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0
                db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

_king_white dw 23, 24
            db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
            db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
            db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9
            db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9
            db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9
            db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9
            db 9, 9, 0, 0, 0, 0, 0, 0, 4, 4, 0, 0, 0, 4, 4, 0, 0, 0, 0, 0, 0, 9, 9
            db 9, 9, 0, 0, 0, 0, 0, 0, 4, 4, 0, 0, 0, 4, 4, 0, 0, 0, 0, 0, 0, 9, 9
            db 0, 0, 4, 4, 4, 4, 4, 4, 0, 0, 4, 4, 4, 0, 0, 4, 4, 4, 4, 4, 4, 0, 0
            db 0, 0, 4, 4, 4, 4, 4, 4, 0, 0, 4, 4, 4, 0, 0, 4, 4, 4, 4, 4, 4, 0, 0
            db 0, 0, 4, 4, 4, 4, 0, 0, 4, 4, 0, 0, 0, 4, 4, 0, 0, 4, 4, 4, 4, 0, 0
            db 0, 0, 4, 4, 4, 4, 0, 0, 4, 4, 0, 0, 0, 4, 4, 0, 0, 4, 4, 4, 4, 0, 0
            db 0, 0, 4, 4, 4, 4, 4, 4, 0, 0, 4, 4, 4, 0, 0, 4, 4, 4, 4, 4, 4, 0, 0
            db 0, 0, 4, 4, 4, 4, 4, 4, 0, 0, 4, 4, 4, 0, 0, 4, 4, 4, 4, 4, 4, 0, 0
            db 0, 0, 4, 4, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 4, 4, 0, 0
            db 0, 0, 4, 4, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 4, 4, 0, 0
            db 9, 9, 0, 0, 4, 4, 0, 0, 0, 0, 4, 4, 4, 0, 0, 0, 0, 4, 4, 0, 0, 9, 9
            db 9, 9, 0, 0, 4, 4, 0, 0, 0, 0, 4, 4, 4, 0, 0, 0, 0, 4, 4, 0, 0, 9, 9
            db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
            db 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9
            db 9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9, 9
            db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
            db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
            db 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9

_knight_white   dw 21, 24
                      db 9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9
                      db 9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9
                      db 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 9, 9, 9, 9
                      db 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
                      db 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
                      db 9, 9, 9, 9, 0, 0, 4, 4, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0
                      db 9, 9, 9, 9, 0, 0, 4, 4, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0
                      db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0
                      db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0
                      db 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 4, 4, 4, 4, 4, 0, 0
                      db 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 4, 4, 4, 4, 4, 0, 0
                      db 0, 0, 4, 4, 4, 4, 0, 0, 4, 4, 0, 0, 4, 4, 4, 4, 4, 4, 4, 0, 0
                      db 0, 0, 4, 4, 4, 4, 0, 0, 4, 4, 0, 0, 4, 4, 4, 4, 4, 4, 4, 0, 0
                      db 9, 9, 0, 0, 0, 0, 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 0, 0
                      db 9, 9, 0, 0, 0, 0, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0
                      db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0
                      db 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
                      db 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
                      db 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
                      db 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
                      db 9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9
                      db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0
                      db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0
                      db 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

_pawn_white dw 17, 23
            db 9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9, 9
            db 9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9, 9
            db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
            db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
            db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
            db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
            db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
            db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
            db 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9
            db 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9
            db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
            db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
            db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
            db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
            db 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9
            db 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9
            db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
            db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
            db 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0
            db 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0
            db 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0
            db 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0
            db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

_queen_white    dw 23, 24
                db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
                db 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 0, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
                db 0, 0, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 0, 0
                db 0, 0, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 0, 0
                db 0, 0, 0, 0, 9, 9, 9, 9, 9, 9, 0, 0, 0, 9, 9, 9, 9, 9, 9, 0, 0, 0, 0
                db 0, 0, 0, 0, 9, 9, 9, 9, 9, 9, 0, 0, 0, 9, 9, 9, 9, 9, 9, 0, 0, 0, 0
                db 0, 0, 4, 4, 0, 0, 9, 9, 0, 0, 4, 4, 4, 0, 0, 9, 9, 0, 0, 4, 4, 0, 0
                db 0, 0, 4, 4, 0, 0, 9, 9, 0, 0, 4, 4, 4, 0, 0, 9, 9, 0, 0, 4, 4, 0, 0
                db 0, 0, 4, 4, 4, 4, 0, 0, 4, 4, 4, 4, 4, 4, 4, 0, 0, 4, 4, 4, 4, 0, 0
                db 0, 0, 4, 4, 4, 4, 0, 0, 4, 4, 4, 4, 4, 4, 4, 0, 0, 4, 4, 4, 4, 0, 0
                db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
                db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
                db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
                db 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9
                db 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9
                db 9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9, 9
                db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
                db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
                db 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9

_rook_white   dw 23, 24
                      db 9, 9, 0, 0, 0, 0, 0, 0, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9
                      db 9, 9, 0, 0, 0, 0, 0, 0, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9
                      db 9, 9, 0, 0, 4, 4, 0, 0, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
                      db 9, 9, 0, 0, 4, 4, 0, 0, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
                      db 9, 9, 0, 0, 4, 4, 0, 0, 0, 0, 0, 0, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
                      db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
                      db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
                      db 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9
                      db 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9
                      db 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9
                      db 9, 9, 9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9, 9, 9, 9
                      db 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9, 9, 9
                      db 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9, 9, 9
                      db 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9, 9, 9
                      db 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9, 9, 9
                      db 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9, 9, 9
                      db 9, 9, 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9, 9, 9
                      db 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9
                      db 9, 9, 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9, 9, 9
                      db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
                      db 9, 9, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 9, 9
                      db 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0
                      db 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0
                      db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 200h

END main