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
    mov edi, offset _background_buffer
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


PROC copyBufferToVideoMemory
    USES esi, edi, ecx
    cld
    Lea esi, [offset _screenBuffer] ; points to a "db 64000 dup ( ? ) " array
    mov edi, VMEMADR ; the video memory
    mov ecx, 64000 / 4 ; 320 * 200 , but copy groups four b y t e s
    rep movsd ; moves a dword and updates ecx , e s i and edi
    ret
ENDP copyBufferToVideoMemory

PROC copyBackgroundToBuffer
    USES esi, edi, ecx
    cld
    Lea esi, [offset _background_buffer] ; points to a "db 64000 dup ( ? ) " array
    mov edi, offset _screenBuffer 
    mov ecx, 64000 / 4 ; 320 * 200 , but copy groups four b y t e s
    rep movsd ; moves a dword and updates ecx , e s i and edi
    ret
ENDP copyBackgroundToBuffer

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
    
    ;mov eax, [@@y]            ; Load y-coordinate
    ;inc eax                   ; Adjust for starting at tile row (y + 1)
    ;mov ebx, TILESIZE
    ;mul ebx                   ; eax = (y + 1) * TILESIZE
    ;sub eax, [@@h]            ; Adjust for sprite height
    ;mov ebx, SCRWIDTH         ; Screen width
    ;mul ebx                   ; eax = ((y + 1) * TILESIZE - sprite height) * SCRWIDTH

    mov eax, 7
    sub eax, [@@y]
    mov ebx, TILESIZE
    mul ebx
    mov ebx, TILESIZE
    sub ebx, [@@h]
    add eax, ebx
    mov ebx, SCRWIDTH
    mul ebx                 ; eax = ((7 - y) * TILESIZE) * SCRWIDTH

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

PROC drawBitBoard
    ARG @@bitboard:dword,  @@spritePtr:dword, @@bitBoardOffset:dword

    ; Load bitboard parts
    ;EDX:EAX
    mov ebx, [@@bitboard]    ; Low 32 bits
    xor ecx, ecx                ; bit index (counter), starting from 0
    xor edx, edx

@@check_bit:
    ; Test bit in eax (low part) or ebx (high part)
    push ebx
    test ebx, 1
    pop ebx
    jz @@skip_low
    ; Calculate file (ecx % 8) and rank (ecx // 8)
    mov eax, ecx
    add eax, [@@bitBoardOffset]
    mov esi, 8
    ;quotient eax = 0
    ;remainder edx = 0
    div esi
   
    call drawSprite, @@spritePtr, offset _screenBuffer, edx, eax
    
    ;jmp @@done

@@skip_low:
    shr ebx, 1                  ; shift bitboard right
    inc ecx                     ; increment bit index
    cmp ecx, 32
    jl @@check_bit                ; continue if in the low part

@@done:
    ret
ENDP drawBitBoard

PROC drawPieces
    call drawBitBoard, [white_pawns_low], offset _pawn_white, 0
    call drawBitBoard, [white_pawns_high], offset _pawn_black, 32

    call drawBitBoard, [black_pawns_low], offset _pawn_white, 0
    call drawBitBoard, [black_pawns_high], offset _pawn_black, 32

    call drawBitBoard, [white_knights_low], offset _knight_white, 0
    call drawBitBoard, [white_knights_high], offset _knight_black, 32

    call drawBitBoard, [black_knights_low], offset _knight_white, 0
    call drawBitBoard, [black_knights_high], offset _knight_black, 32

    call drawBitBoard, [white_bishops_low], offset _bishop_white, 0
    call drawBitBoard, [white_bishops_high], offset _bishop_black, 32

    call drawBitBoard, [black_bishops_low], offset _bishop_white, 0
    call drawBitBoard, [black_bishops_high], offset _bishop_black, 32

    call drawBitBoard, [white_rooks_low], offset _rook_white, 0
    call drawBitBoard, [white_rooks_high], offset _rook_black, 32

    call drawBitBoard, [black_rooks_low], offset _rook_white, 0
    call drawBitBoard, [black_rooks_high], offset _rook_black, 32

    call drawBitBoard, [white_queens_low], offset _queen_white, 0
    call drawBitBoard, [white_queens_high], offset _queen_black, 32

    call drawBitBoard, [black_queens_low], offset _queen_white, 0
    call drawBitBoard, [black_queens_high], offset _queen_black, 32

    call drawBitBoard, [white_kings_low], offset _king_white, 0
    call drawBitBoard, [white_kings_high], offset _king_black, 32

    call drawBitBoard, [black_kings_low], offset _king_white, 0
    call drawBitBoard, [black_kings_high], offset _king_black, 32

    ret
ENDP drawPieces

PROC combineWhiteBitBoards
    mov eax, [white_pawns_low]
    mov ebx, [white_knights_low]
    or eax, ebx
    mov [white_combine_low], eax

    mov eax, [white_pawns_high]
    mov ebx, [white_knights_high]
    or eax, ebx
    mov [white_combine_high], eax

    mov eax, [white_combine_low]
    mov ebx, [white_bishops_low]
    or eax, ebx
    mov [white_combine_low], eax

    mov eax, [white_combine_high]
    mov ebx, [white_bishops_high]
    or eax, ebx
    mov [white_combine_high], eax

    mov eax, [white_combine_low]
    mov ebx, [white_rooks_low]
    or eax, ebx
    mov [white_combine_low], eax

    mov eax, [white_combine_high]
    mov ebx, [white_rooks_high]
    or eax, ebx
    mov [white_combine_high], eax

    mov eax, [white_combine_low]
    mov ebx, [white_queens_low]
    or eax, ebx
    mov [white_combine_low], eax

    mov eax, [white_combine_high]
    mov ebx, [white_queens_high]
    or eax, ebx
    mov [white_combine_high], eax

    mov eax, [white_combine_low]
    mov ebx, [white_kings_low]
    or eax, ebx
    mov [white_combine_low], eax

    mov eax, [white_combine_high]
    mov ebx, [white_kings_high]
    or eax, ebx
    mov [white_combine_high], eax

    ret
ENDP combineWhiteBitBoards

PROC combineBlackBitBoards
    mov eax, [black_pawns_low]
    mov ebx, [black_knights_low]
    or eax, ebx
    mov [black_combine_low], eax

    mov eax, [black_pawns_high]
    mov ebx, [black_knights_high]
    or eax, ebx
    mov [black_combine_high], eax

    mov eax, [black_combine_low]
    mov ebx, [black_bishops_low]
    or eax, ebx
    mov [black_combine_low], eax

    mov eax, [black_combine_high]
    mov ebx, [black_bishops_high]
    or eax, ebx
    mov [black_combine_high], eax

    mov eax, [black_combine_low]
    mov ebx, [black_rooks_low]
    or eax, ebx
    mov [black_combine_low], eax

    mov eax, [black_combine_high]
    mov ebx, [black_rooks_high]
    or eax, ebx
    mov [black_combine_high], eax

    mov eax, [black_combine_low]
    mov ebx, [black_queens_low]
    or eax, ebx
    mov [black_combine_low], eax

    mov eax, [black_combine_high]
    mov ebx, [black_queens_high]
    or eax, ebx
    mov [black_combine_high], eax

    mov eax, [black_combine_low]
    mov ebx, [black_kings_low]
    or eax, ebx
    mov [black_combine_low], eax

    mov eax, [black_combine_high]
    mov ebx, [black_kings_high]
    or eax, ebx
    mov [black_combine_high], eax

    ret
ENDP combineBlackBitBoards

PROC generatePawnMovementBitBoar
    ARG @@bitBoardLow:dword, @@bitBoardHigh:dword, @@blockingBitBoardLow:dword, @@blockingBitBoardHigh:dword, @@position:dword
    LOCAL @@movementBitBoardLow:dword, @@movementBitBoardHigh:dword

    cmp @@position, 31              ; Check if position is in the low part (0-31)
    jg @@high_mask                  ; If position is greater than 31, it's in the high part

@@low_mask:
    mov edx, 1                      ; Start with 1 (a single bit set)
    mov ecx, @@position             ; Load the position (0-31) into ECX
    shl edx, cl                     ; Shift 1 by the amount in position
    mov eax, [@@bitBoardLow]        ; Load the low part of the bitboard
    ;and eax, edx                    ; Mask the pawn position in the low part
    jmp @@mask_done                 ; Skip high part processing

@@high_mask:
    sub ecx, 31                     ; Adjust the position to be within 0-31 for the high part
    mov edx, 1                      ; Start with 1 (a single bit set)
    shl edx, cl                     ; Shift 1 by the adjusted position (for high part)
    mov eax, 0                      ; Clear eax (no pawn movement in low part)
    mov ebx, [@@bitBoardHigh]       ; Load the high part of the bitboard
    ;and ebx, edx                    ; Mask the pawn position in the high part

@@mask_done:
    ; --- Handle the blocking bitboard ---
    not [@@blockingBitBoardLow]     ; Invert the blocking bitboard (low part)
    not [@@blockingBitBoardHigh]    ; Invert the blocking bitboard (high part)

    ; Mask the low and high parts of the bitboard against the blocking bitboard
    and eax, [@@blockingBitBoardLow]   ; Mask the low part with the blocking bitboard
    and ebx, [@@blockingBitBoardHigh]  ; Mask the high part with the blocking bitboard

    ; --- Combine the results ---
    shl eax, 8                      ; Shift eax left by 8 bits (to align with the high part)
    rcl ebx, 8                      ; Rotate the carry flag into ebx

    ; Store the results in the movement bitboards
    mov [movement_bit_board_low], eax
    mov [movement_bit_board_high], ebx

    ret
ENDP generatePawnMovementBitBoar


PROC isolateBitFromBitBoard
    ARG @@bitBoard:dword, @@position:dword
    USES eax, edx, ecx

    mov edx, 1                      ; Start with 1 (a single bit set)
    mov ecx, @@position             ; Load the position (0-31) into ECX
    shl edx, cl                     ; Shift 1 by the amount in position
    mov eax, [@@bitBoard]           ; Load the bitboard
    and eax, edx                    ; Mask the pawn position in the bitboard

    mov [isolated_bit_low], eax         ; Store the isolated bit
    ret
ENDP isolateBitFromBitBoard

PROC isolateBitFromBitBoards
    ARG @@bitBoardLow:dword, @@bitBoardHigh:dword, @@position:dword
    USES eax

    cmp @@position, 31              ; Check if position is in the low part (0-31)
    jg @@isolate_high               ; If position is greater than 31, it's in the high part

@@isolate_low:
    call isolateBitFromBitBoard, @@bitBoardLow, @@position
    mov [isolated_bit_high], 0     
    ret

@@isolate_high:
    sub @@position, 32              ; Adjust the position to be within 0-31 for the high part
    call isolateBitFromBitBoard, @@bitBoardHigh, @@position
    mov eax, [isolated_bit_low]
    mov [isolated_bit_low], 0
    mov [isolated_bit_high], eax          
    ret

ENDP isolateBitFromBitBoards

PROC generatePawnMovementBitBoard
    ARG @@bitBoardLow:dword, @@bitBoardHigh:dword, @@blockingBitBoardLow:dword, @@blockingBitBoardHigh:dword, @@position:dword
    LOCAL @@movementBitBoardLow:dword, @@movementBitBoardHigh:dword
    USES eax, ebx
    mov eax, [@@bitBoardLow]
    mov ebx, [@@bitBoardHigh]

    ;only get the bit at 'position' from the bitboards it can be in the low or high part
    call isolateBitFromBitBoards, @@bitBoardLow, @@bitBoardHigh, @@position
    
    ;res gets stored in isolated_bit_low and isolated_bit_high
    mov eax, [isolated_bit_low]
    mov ebx, [isolated_bit_high]

    ;mask / remove last row since they can't move forward
    mov ecx, TOPWALL
    not ecx
    and ebx, ecx

    ;shift the bit to the left 8 times this has the effect of moving the bit to the next row
    mov ecx, 8              ; Set loop counter to 8
@@shift_rotate_loop:
    shl eax, 1           
    rcl ebx, 1              ; Rotate the carry flag into ebx (shift ebx left and put carry in LSB of ebx)
    loop @@shift_rotate_loop
    
    mov [@@movementBitBoardLow], eax
    mov [@@movementBitBoardHigh], ebx

    ;if the pawn is in the second row it can move two steps forward
    cmp @@position, 15
    jg @@skipDoubleMove

    ;and so we need to shift the bit to the left 8 times again
    mov ecx, 8              ; Set loop counter to 8
@@shift_rotate_loop2:
    shl eax, 1           ; eax = eax << 1, carry flag set to the bit that was shifted out
    rcl ebx, 1           ; Rotate the carry flag into ebx (shift ebx left and put carry in LSB of ebx)
    loop @@shift_rotate_loop2

    ;we combine the two movement bitboards 1 step and 2 steps
    or eax, [@@movementBitBoardLow]
@@skipDoubleMove:

    ;mask the movement bitboard with the blocking bitboard
    ;we remove the positions that are already occupied by other pieces
    not [@@blockingBitBoardLow]     ; Invert the blocking bitboard (low part)
    not [@@blockingBitBoardHigh]    ; Invert the blocking bitboard (high part)

    ; Mask the low and high parts of the bitboard against the blocking bitboard
    and eax, [@@blockingBitBoardLow]   ; Mask the low part with the blocking bitboard
    and ebx, [@@blockingBitBoardHigh]  ; Mask the high part with the blocking bitboard

    ;store the result in the movement bitboards
    mov [movement_bit_board_low], eax
    mov [movement_bit_board_high], ebx

    ret
ENDP generatePawnMovementBitBoard

PROC repeatShiftRotateLeft
    ARG @@shift:dword, @@mask:dword, @@mask2:dword, @@iterations:dword
    LOCAL @@movementBitBoardLow:dword, @@movementBitBoardHigh:dword
    USES eax, ebx, ecx, edx

    mov [@@movementBitBoardLow], 0   ; Initialize movement bitboard (low part) to 0
    mov [@@movementBitBoardHigh], 0  ; Initialize movement bitboard (high part) to 0

    mov eax, [isolated_bit_low]      ; Load the low part of the bitboard
    mov ebx, [isolated_bit_high]     ; Load the high part of the bitboard

    mov ecx, @@iterations                       ; Set outer loop counter to 8 (for 8 iterations)
@@outer_loop:
    push ecx                         ; Save the outer loop counter

    mov edx, @@mask                  ; Load the mask
    not edx                          ; Invert the mask
    and eax, edx                     ; Apply mask to the low part (eax)
    and ebx, edx                     ; Apply mask to the high part (ebx)

    mov edx, @@mask2                  ; Load the mask
    not edx                          ; Invert the mask
    and ebx, edx                     ; Apply mask to the high part (ebx)

    mov ecx, @@shift                 ; Set inner loop counter for shifts and rotates
@@shift_rotate_loop_left_up:
    shl eax, 1                       ; Shift eax left by 1 bit
    rcl ebx, 1                       ; Rotate the carry flag into ebx
    loop @@shift_rotate_loop_left_up ; Repeat the inner loop for the specified number of shifts

    pop ecx                          ; Restore the outer loop counter

    ; Combine the shifted results with the movement bitboard
    or [@@movementBitBoardLow], eax  ; Update the movement bitboard (low part)
    or [@@movementBitBoardHigh], ebx ; Update the movement bitboard (high part)

    loop @@outer_loop                ; Repeat the outer loop for 8 iterations

    ; Store the final result in the output variables
    mov eax, [@@movementBitBoardLow]
    mov ebx, [@@movementBitBoardHigh]
    mov [movement_bit_board_low], eax
    mov [movement_bit_board_high], ebx

    ret
ENDP repeatShiftRotateLeft

PROC repeatShiftRotateRight
    ARG @@shift:dword, @@mask:dword, @@mask2:dword, @@iterations:dword
    LOCAL @@movementBitBoardLow:dword, @@movementBitBoardHigh:dword
    USES eax, ebx, ecx, edx

    mov [@@movementBitBoardLow], 0   ; Initialize movement bitboard (low part) to 0
    mov [@@movementBitBoardHigh], 0  ; Initialize movement bitboard (high part) to 0

    mov eax, [isolated_bit_low]      ; Load the low part of the bitboard
    mov ebx, [isolated_bit_high]     ; Load the high part of the bitboard

    mov ecx, @@iterations                       ; Set outer loop counter to 8 (for 8 iterations)
@@outer_loop:
    push ecx                         ; Save the outer loop counter

    mov edx, @@mask                  ; Load the mask
    not edx                          ; Invert the mask
    and eax, edx                     ; Apply mask to the low part (eax)
    and ebx, edx                     ; Apply mask to the high part (ebx)

    mov edx, @@mask2                  ; Load the mask
    not edx                          ; Invert the mask
    and eax, edx                     ; Apply mask to the low part (eax)

    mov ecx, @@shift                 ; Set inner loop counter for shifts and rotates
@@shift_rotate_loop_left_up:
    shr eax, 1                       ; Shift eax left by 1 bit
    rcr ebx, 1                       ; Rotate the carry flag into ebx
    loop @@shift_rotate_loop_left_up ; Repeat the inner loop for the specified number of shifts

    pop ecx                          ; Restore the outer loop counter

    ; Combine the shifted results with the movement bitboard
    or [@@movementBitBoardLow], eax  ; Update the movement bitboard (low part)
    or [@@movementBitBoardHigh], ebx ; Update the movement bitboard (high part)

    loop @@outer_loop                ; Repeat the outer loop for 8 iterations

    ; Store the final result in the output variables
    mov eax, [@@movementBitBoardLow]
    mov ebx, [@@movementBitBoardHigh]
    mov [movement_bit_board_low], eax
    mov [movement_bit_board_high], ebx

    ret
ENDP repeatShiftRotateRight




PROC generateBishopMovementBitBoard
ARG @@bitBoardLow:dword, @@bitBoardHigh:dword, @@blockingBitBoardLow:dword, @@blockingBitBoardHigh:dword, @@position:dword, @@iterations:dword
    LOCAL @@movementBitBoardLow:dword, @@movementBitBoardHigh:dword
    USES eax, ebx

    mov [@@movementBitBoardLow], 0
    mov [@@movementBitBoardHigh], 0

    ;only get the bit at 'position' from the bitboards it can be in the low or high part
    call isolateBitFromBitBoards, [@@bitBoardLow], [@@bitBoardHigh], [@@position]
    
    ;res gets stored in isolated_bit_low and isolated_bit_high
    mov eax, 0
    mov ebx, 0

    ;mask / remove left column since they can't move left
    
    call repeatShiftRotateLeft, 7, LEFTWALL, TOPWALL, @@iterations

    mov eax, [movement_bit_board_low]
    mov ebx, [movement_bit_board_high]
    or eax, [@@movementBitBoardLow]
    or ebx, [@@movementBitBoardHigh]
    mov [@@movementBitBoardLow], eax
    mov [@@movementBitBoardHigh], ebx


    call repeatShiftRotateLeft, 9, RIGHTWALL, TOPWALL, @@iterations

    mov eax, [movement_bit_board_low]
    mov ebx, [movement_bit_board_high]
    or eax, [@@movementBitBoardLow]
    or ebx, [@@movementBitBoardHigh]
    mov [@@movementBitBoardLow], eax
    mov [@@movementBitBoardHigh], ebx

    call repeatShiftRotateRight, 7, LEFTWALL, BOTTOMWALL, @@iterations

    mov eax, [movement_bit_board_low]
    mov ebx, [movement_bit_board_high]
    or eax, [@@movementBitBoardLow]
    or ebx, [@@movementBitBoardHigh]
    mov [@@movementBitBoardLow], eax
    mov [@@movementBitBoardHigh], ebx

    call repeatShiftRotateRight, 9, RIGHTWALL, BOTTOMWALL, @@iterations

    mov eax, [movement_bit_board_low]
    mov ebx, [movement_bit_board_high]
    or eax, [@@movementBitBoardLow]
    or ebx, [@@movementBitBoardHigh]


    ;mask the movement bitboard with the blocking bitboard
    ;we remove the positions that are already occupied by other pieces
    not [@@blockingBitBoardLow]     ; Invert the blocking bitboard (low part)
    not [@@blockingBitBoardHigh]    ; Invert the blocking bitboard (high part)

    ; Mask the low and high parts of the bitboard against the blocking bitboard
    and eax, [@@blockingBitBoardLow]   ; Mask the low part with the blocking bitboard
    and ebx, [@@blockingBitBoardHigh]  ; Mask the high part with the blocking bitboard

    ;store the result in the movement bitboards
    mov [movement_bit_board_low], eax
    mov [movement_bit_board_high], ebx

    ret
ENDP generateBishopMovementBitBoard

PROC generateKnightMovementBitBoard
    ARG @@bitBoardLow:dword, @@bitBoardHigh:dword, @@blockingBitBoardLow:dword, @@blockingBitBoardHigh:dword, @@position:dword, @@iterations:dword
    LOCAL @@movementBitBoardLow:dword, @@movementBitBoardHigh:dword
    USES eax, ebx

    
    ret
ENDP generateKnightMovementBitBoard

PROC generateRookMovementBitBoard
    ARG @@bitBoardLow:dword, @@bitBoardHigh:dword, @@blockingBitBoardLow:dword, @@blockingBitBoardHigh:dword, @@position:dword, @@iterations:dword
    LOCAL @@movementBitBoardLow:dword, @@movementBitBoardHigh:dword
    USES eax, ebx

    mov [@@movementBitBoardLow], 0
    mov [@@movementBitBoardHigh], 0

    ;only get the bit at 'position' from the bitboards it can be in the low or high part
    call isolateBitFromBitBoards, [@@bitBoardLow], [@@bitBoardHigh], [@@position]
    
    ;res gets stored in isolated_bit_low and isolated_bit_high
    mov eax, 0
    mov ebx, 0

    ;mask / remove left column since they can't move left
    
    call repeatShiftRotateLeft, 8, 0, TOPWALL, @@iterations

    mov eax, [movement_bit_board_low]
    mov ebx, [movement_bit_board_high]
    or eax, [@@movementBitBoardLow]
    or ebx, [@@movementBitBoardHigh]
    mov [@@movementBitBoardLow], eax
    mov [@@movementBitBoardHigh], ebx


    call repeatShiftRotateLeft, 1, RIGHTWALL, 0, @@iterations

    mov eax, [movement_bit_board_low]
    mov ebx, [movement_bit_board_high]
    or eax, [@@movementBitBoardLow]
    or ebx, [@@movementBitBoardHigh]
    mov [@@movementBitBoardLow], eax
    mov [@@movementBitBoardHigh], ebx

    call repeatShiftRotateRight, 8, 0, BOTTOMWALL, @@iterations

    mov eax, [movement_bit_board_low]
    mov ebx, [movement_bit_board_high]
    or eax, [@@movementBitBoardLow]
    or ebx, [@@movementBitBoardHigh]
    mov [@@movementBitBoardLow], eax
    mov [@@movementBitBoardHigh], ebx

    call repeatShiftRotateRight, 1, LEFTWALL, 0, @@iterations

    mov eax, [movement_bit_board_low]
    mov ebx, [movement_bit_board_high]
    or eax, [@@movementBitBoardLow]
    or ebx, [@@movementBitBoardHigh]


    ;mask the movement bitboard with the blocking bitboard
    ;we remove the positions that are already occupied by other pieces
    not [@@blockingBitBoardLow]     ; Invert the blocking bitboard (low part)
    not [@@blockingBitBoardHigh]    ; Invert the blocking bitboard (high part)

    ; Mask the low and high parts of the bitboard against the blocking bitboard
    and eax, [@@blockingBitBoardLow]   ; Mask the low part with the blocking bitboard
    and ebx, [@@blockingBitBoardHigh]  ; Mask the high part with the blocking bitboard

    ;store the result in the movement bitboards
    mov [movement_bit_board_low], eax
    mov [movement_bit_board_high], ebx

    ret
ENDP generateRookMovementBitBoard

PROC generateQueenkMovementBitBoard
    ARG @@bitBoardLow:dword, @@bitBoardHigh:dword, @@blockingBitBoardLow:dword, @@blockingBitBoardHigh:dword, @@position:dword, @@iterations:dword
    USES eax, ebx

    call generateBishopMovementBitBoard, [@@bitBoardLow], [@@bitBoardHigh], [@@blockingBitBoardLow], [@@blockingBitBoardHigh], [@@position], [@@iterations]
    
    mov eax, [movement_bit_board_low]
    mov ebx, [movement_bit_board_high]

    call generateRookMovementBitBoard, [@@bitBoardLow], [@@bitBoardHigh], [@@blockingBitBoardLow], [@@blockingBitBoardHigh], [@@position], [@@iterations]
    or eax, [movement_bit_board_low]
    or ebx, [movement_bit_board_high]

    mov [movement_bit_board_low], eax
    mov [movement_bit_board_high], ebx

    ret
ENDP generateQueenkMovementBitBoard

PROC generateKingMovementBitBoard
    ARG @@bitBoardLow:dword, @@bitBoardHigh:dword, @@blockingBitBoardLow:dword, @@blockingBitBoardHigh:dword, @@position:dword

    call generateQueenkMovementBitBoard, [@@bitBoardLow], [@@bitBoardHigh], [@@blockingBitBoardLow], [@@blockingBitBoardHigh], [@@position], 1
    ret
ENDP generateKingMovementBitBoard



PROC main
    sti                ; Enable interrupts.
    cld                ; Clear direction flag.

    push ds            ;used for buffers DO NOT REMOVE!
    pop es             ;used for buffers DO NOT REMOVE!

    VMEMADR EQU 0A0000h    ; Video memory address
    BUFFERADR EQU 0B0000h  ; Buffer address
    SCRWIDTH EQU 320       ; Screen width for mode 13h
    SCRHEIGHT EQU 200      ; Screen height


    PADDING EQU 60         ; black pixels left and right since 320x200 is not 1:1
    BOARDDIMENSION EQU 200 
    TILESIZE EQU 25 

    TOPWALL EQU 0FF000000h
    BOTTOMWALL EQU 0000000FFh
    LEFTWALL EQU 001010101h
    RIGHTWALL EQU 080808080h
    
    EXTRN printUnsignedNumber:PROC
    EXTRN printNewline:PROC
    EXTRN updateColourPalette:PROC
    EXTRN setVideoMode:PROC


    call setVideoMode, 13h
    call updateColourPalette, 6
    mov EDI, VMEMADR

    call setupBitboards
    call combineWhiteBitBoards
    call combineBlackBitBoards

    ;TODO: combine the white and black bitboards into block_low and block_high

    call drawEmptyBoard 

    ;since background never changes we can store it and copy it instead of redrawing
    ;so first copy the background to the buffer
    ;then draw the pieces and other stuff
    call copyBackgroundToBuffer
    call drawPieces
    ;call drawBitBoard, [white_combine_low], offset _pawn_white, 0
    ;call drawBitBoard, [white_combine_high], offset _pawn_black, 32
    call copyBufferToVideoMemory

    call generateQueenkMovementBitBoard, [test_low], [test_high], [white_combine_low], [black_combine_high], 28, 8
    
    ;call drawBitBoard, [test_low], offset _pawn_white, 0
    ;call drawBitBoard, [test_high], offset _pawn_white, 32
    call drawBitBoard, [block_low], offset _bishop_black, 0
    call drawBitBoard, [block_high], offset _bishop_black, 32
    call drawBitBoard, [movement_bit_board_low], offset _indicator, 0
    call drawBitBoard, [movement_bit_board_high], offset _indicator, 32

    call copyBufferToVideoMemory


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
_background_buffer db 64000 dup(?) ; background buffer


; Bitboards for pawns (64 bits each)
white_pawns_high    DD 0 ; High 32 bits 
white_pawns_low     DD 0 ; Low 32 bits
black_pawns_high    DD 0 ; High 32 bits
black_pawns_low     DD 0 ; Low 32 bits
white_knights_high  DD 0 ; High 32 bits
white_knights_low   DD 0 ; Low 32 bits
black_knights_high  DD 0 ; High 32 bits
black_knights_low   DD 0 ; Low 32 bits
white_bishops_high  DD 0 ; High 32 bits
white_bishops_low   DD 0 ; Low 32 bits
black_bishops_high  DD 0 ; High 32 bits
black_bishops_low   DD 0 ; Low 32 bits
white_rooks_high    DD 0 ; High 32 bits
white_rooks_low     DD 0 ; Low 32 bits
black_rooks_high    DD 0 ; High 32 bits
black_rooks_low     DD 0 ; Low 32 bits
white_queens_high   DD 0 ; High 32 bits
white_queens_low    DD 0 ; Low 32 bits
black_queens_high   DD 0 ; High 32 bits
black_queens_low    DD 0 ; Low 32 bits
white_kings_high    DD 0 ; High 32 bits
white_kings_low     DD 0 ; Low 32 bits
black_kings_high    DD 0 ; High 32 bits
black_kings_low     DD 0 ; Low 32 bits

white_combine_high    DD 0 ; High 32 bits
white_combine_low     DD 0 ; Low 32 bits
black_combine_high    DD 0 ; High 32 bits
black_combine_low     DD 0 ; Low 32 bits

test_low DD 0FFFFFFFFh  ; Define a 32-bit value with all bits set to 1
test_high DD 0FFFFFFFFh ; Same for the high part

block_low DD 0 ; Low 32 bits
block_high DD 0 ; High 32 bits

movement_bit_board_high DD 0 ; High 32 bits
movement_bit_board_low  DD 0 ; Low 32 bits
isolated_bit_low DD 0
isolated_bit_high DD 0

colour_to_draw      DD 0



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

_indicator dw 3, 15
           db 9, 9, 9
           db 5, 5, 5
           db 5, 5, 5
           db 5, 5, 5
           db 9, 9, 9
           db 9, 9, 9
           db 9, 9, 9
           db 9, 9, 9
           db 9, 9, 9
           db 9, 9, 9
           db 9, 9, 9
           db 9, 9, 9
           db 9, 9, 9
           db 9, 9, 9
           db 9, 9, 9
; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 200h

END main