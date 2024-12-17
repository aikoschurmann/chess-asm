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

PROC drawSpritePixel
    ARG @@spritePtr:dword, @@dstPtr:dword, @@x:dword, @@y:dword
    LOCAL @@w:dword, @@h:dword
    USES eax, ebx, ecx, edx, esi, edi
    
    ; Load sprite pointer and extract width/height
    mov esi, [@@spritePtr]
    xor eax, eax
    lodsw                      ; Read width into AX
    mov [@@w], eax             ; Store width in local variable
    lodsw                      ; Read height into AX
    mov [@@h], eax             ; Store height in local variable

    ; Calculate destination starting pointer
    mov edi, [@@dstPtr]        ; Base address of destination
    mov eax, [@@y]             ; Row (y-coordinate)
    mov ebx, SCRWIDTH          ; Screen width
    mul ebx                    ; eax = y * SCRWIDTH
    add edi, eax               ; Add row offset to destination pointer
    add edi, [@@x]             ; Add column offset (x-coordinate)

    ; Loop through each line of the sprite
    mov edx, [@@h]             ; Number of rows in sprite (height)
@@drawLine:
    push edx                   ; Save outer loop counter
    mov ecx, [@@w]             ; Number of pixels in the current row (width)
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

    ret                 ; Return from procedure
ENDP drawSpritePixel



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

    cmp [@@position], 31              ; Check if position is in the low part (0-31)
    jg @@high_mask                  ; If position is greater than 31, it's in the high part

@@low_mask:
    mov edx, 1                      ; Start with 1 (a single bit set)
    mov ecx, [@@position]             ; Load the position (0-31) into ECX
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
    mov ecx, [@@position]           ; Load the position (0-31) into ECX
    shl edx, cl                     ; Shift 1 by the amount in position
    mov eax, [@@bitBoard]           ; Load the bitboard
    and eax, edx                    ; Mask the pawn position in the bitboard

    mov [isolated_bit_low], eax         ; Store the isolated bit
    ret
ENDP isolateBitFromBitBoard

PROC positionToBitBoard
    ARG @@position:dword
    USES eax, edx, ecx

    mov edx, 1                      ; Start with 1 (a single bit set)
    mov ecx, [@@position]           ; Load the position (0-63) into ECX
    shl edx, cl                     ; Shift 1 by the amount in position

    mov [active_bit_board_mask_low], edx
    ret
ENDP positionToBitBoard


PROC positionToBitBoards
    ARG @@position:dword
    USES eax

    cmp [@@position], 31              ; Check if position is in the low part (0-31)
    jg @@isolate_high               ; If position is greater than 31, it's in the high part

@@isolate_low:
    call positionToBitBoard, [@@position]
    mov [active_bit_board_mask_high], 0     
    ret

@@isolate_high:
    sub @@position, 32              ; Adjust the position to be within 0-31 for the high part
    call positionToBitBoard, [@@position]
    mov eax, [active_bit_board_mask_low]
    mov [active_bit_board_mask_low], 0
    mov [active_bit_board_mask_high], eax          
    ret
ENDP positionToBitBoards


PROC isolateBitFromBitBoards
    ARG @@bitBoardLow:dword, @@bitBoardHigh:dword, @@position:dword
    USES eax

    call positionToBitBoards, [@@position]

    mov eax, [active_bit_board_mask_low]
    and eax, [@@bitBoardLow]
    mov [isolated_bit_low], eax

    mov eax, [active_bit_board_mask_high]
    and eax, [@@bitBoardHigh]
    mov [isolated_bit_high], eax

    ret

ENDP isolateBitFromBitBoards


PROC repeatShiftRotateLeft
    ARG @@shift:dword, @@mask:dword, @@mask2:dword, @@iterations:dword, @@enemyBitBoardLow:dword, @@enemyBitBoardHigh:dword
    LOCAL @@movementBitBoardLow:dword, @@movementBitBoardHigh:dword
    USES eax, ebx, ecx, edx

    mov [@@movementBitBoardLow], 0   ; Initialize movement bitboard (low part) to 0
    mov [@@movementBitBoardHigh], 0  ; Initialize movement bitboard (high part) to 0

    mov eax, [isolated_bit_low]      ; Load the low part of the bitboard
    mov ebx, [isolated_bit_high]     ; Load the high part of the bitboard

    call bitBoardToPosition, eax, ebx ; Convert the bitboard to a position (0-63)

    mov ecx, [@@iterations]            ; Set outer loop counter to 8 (for 8 iterations)
@@outer_loop:
    push ecx                         ; Save the outer loop counter

    mov edx, [@@mask]                  ; Load the mask
    not edx                          ; Invert the mask
    and eax, edx                     ; Apply mask to the low part (eax)
    and ebx, edx                     ; Apply mask to the high part (ebx)

    mov edx, [@@mask2]                  ; Load the mask
    not edx                          ; Invert the mask
    and ebx, edx                     ; Apply mask to the high part (ebx)

    mov ecx, [@@shift]                 ; Set inner loop counter for shifts and rotates
@@shift_rotate_loop_left_up:
    shl eax, 1                       ; Shift eax left by 1 bit
    rcl ebx, 1                       ; Rotate the carry flag into ebx
    loop @@shift_rotate_loop_left_up ; Repeat the inner loop for the specified number of shifts

    pop ecx                          ; Restore the outer loop counter
    
    ;increment the position by the shift
    mov edx, [position]
    add edx, [@@shift]
    mov [position], edx


    ; Combine the shifted results with the movement bitboard
    or [@@movementBitBoardLow], eax  ; Update the movement bitboard (low part)
    or [@@movementBitBoardHigh], ebx ; Update the movement bitboard (high part)

    call isEnemyPieceAtPosition, [@@enemyBitBoardLow], [@@enemyBitBoardHigh], [position]
    cmp [piece_at_position], 1
    je @@done

    loop @@outer_loop                ; Repeat the outer loop for 8 iterations

@@done:
    ; Store the final result in the output variables
    mov eax, [movement_bit_board_low]
    mov ebx, [movement_bit_board_high]
    or eax, [@@movementBitBoardLow]
    or ebx, [@@movementBitBoardHigh]
    mov [movement_bit_board_low], eax
    mov [movement_bit_board_high], ebx

    ret
ENDP repeatShiftRotateLeft

PROC repeatShiftRotateRight
    ARG @@shift:dword, @@mask:dword, @@mask2:dword, @@iterations:dword, @@enemyBitBoardLow:dword, @@enemyBitBoardHigh:dword
    LOCAL @@movementBitBoardLow:dword, @@movementBitBoardHigh:dword
    USES eax, ebx, ecx, edx

    mov [@@movementBitBoardLow], 0   ; Initialize movement bitboard (low part) to 0
    mov [@@movementBitBoardHigh], 0  ; Initialize movement bitboard (high part) to 0

    mov eax, [isolated_bit_low]      ; Load the low part of the bitboard
    mov ebx, [isolated_bit_high]     ; Load the high part of the bitboard

    call bitBoardToPosition, eax, ebx ; Convert the bitboard to a position (0-63)

    mov ecx, [@@iterations]            ; Set outer loop counter to 8 (for 8 iterations)
@@outer_loop:
    push ecx                         ; Save the outer loop counter

    mov edx, [@@mask]                  ; Load the mask
    not edx                          ; Invert the mask
    and eax, edx                     ; Apply mask to the low part (eax)
    and ebx, edx                     ; Apply mask to the high part (ebx)

    mov edx, [@@mask2]                 ; Load the mask
    not edx                          ; Invert the mask
    and eax, edx                     ; Apply mask to the low part (eax)

    mov ecx, [@@shift]                 ; Set inner loop counter for shifts and rotates
@@shift_rotate_loop_left_up:
    shr ebx, 1                       ; Shift eax left by 1 bit
    rcr eax, 1                       ; Rotate the carry flag into ebx
    loop @@shift_rotate_loop_left_up ; Repeat the inner loop for the specified number of shifts

    pop ecx                          ; Restore the outer loop counter

    ; Decrement the position by the shift
    mov edx, [position]
    sub edx, [@@shift]
    mov [position], edx

    ; Combine the shifted results with the movement bitboard
    or [@@movementBitBoardLow], eax  ; Update the movement bitboard (low part)
    or [@@movementBitBoardHigh], ebx ; Update the movement bitboard (high part)

    call isEnemyPieceAtPosition, [@@enemyBitBoardLow], [@@enemyBitBoardHigh], [position]
    cmp [piece_at_position], 1
    je @@done

    loop @@outer_loop                ; Repeat the outer loop for 8 iterations
@@done:
    mov eax, [movement_bit_board_low]
    mov ebx, [movement_bit_board_high]
    or eax, [@@movementBitBoardLow]
    or ebx, [@@movementBitBoardHigh]
    mov [movement_bit_board_low], eax
    mov [movement_bit_board_high], ebx

    ret
ENDP repeatShiftRotateRight


PROC generateWhitePawnMovementBitBoard
    ARG @@bitBoardLow:dword, @@bitBoardHigh:dword, @@blockingBitBoardLow:dword, @@blockingBitBoardHigh:dword, @@position:dword, @@iterations:dword, @@enemyBitBoardLow:dword, @@enemyBitBoardHigh:dword
    LOCAL @@movementBitBoardLow:dword, @@movementBitBoardHigh:dword
    USES eax, ebx

    mov [movement_bit_board_low], 0
    mov [movement_bit_board_high], 0

    ;only get the bit at 'position' from the bitboards it can be in the low or high part
    call isolateBitFromBitBoards, [@@bitBoardLow], [@@bitBoardHigh], [@@position]


    cmp [@@position], 15
    jg @@single_shift
    call repeatShiftRotateLeft, 8, 00000000h, TOPWALL,  2, [@@enemyBitBoardLow], [@@enemyBitBoardHigh]

@@single_shift:
    call repeatShiftRotateLeft, 8, 00000000h, TOPWALL,  1, [@@enemyBitBoardLow], [@@enemyBitBoardHigh]


    ;store one or two steps forward in the movement bitboard
    mov eax, [movement_bit_board_low]
    mov ebx, [movement_bit_board_high]

    ; Remove straight ahead blocked positions since only pawn can't capture in its moving direction
    not [@@enemyBitBoardLow]
    not [@@enemyBitBoardHigh]

    and eax, [@@enemyBitBoardLow]
    and ebx, [@@enemyBitBoardHigh]

    not [@@enemyBitBoardLow]
    not [@@enemyBitBoardHigh]

    ; Store the result in the movement bitboards

    mov [@@movementBitBoardLow], eax
    mov [@@movementBitBoardHigh], ebx

    mov [movement_bit_board_low], 0
    mov [movement_bit_board_high], 0

    ;calculate diagonal captures
    call repeatShiftRotateLeft, 7, LEFTWALL, TOPWALL, 1, [@@enemyBitBoardLow], [@@enemyBitBoardHigh]
    call repeatShiftRotateLeft, 9, RIGHTWALL, TOPWALL, 1, [@@enemyBitBoardLow], [@@enemyBitBoardHigh]

    mov eax, [movement_bit_board_low]
    mov ebx, [movement_bit_board_high]

    and eax, [@@enemyBitBoardLow]
    and ebx, [@@enemyBitBoardHigh]

    ; combine the diagonal captures with the movement bitboard
    or [@@movementBitBoardLow], eax
    or [@@movementBitBoardHigh], ebx


    mov eax, [@@movementBitBoardLow]
    mov ebx, [@@movementBitBoardHigh]

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
    
ENDP generateWhitePawnMovementBitBoard

PROC generateBlackPawnMovementBitBoard
    ARG @@bitBoardLow:dword, @@bitBoardHigh:dword, @@blockingBitBoardLow:dword, @@blockingBitBoardHigh:dword, @@position:dword, @@iterations:dword, @@enemyBitBoardLow:dword, @@enemyBitBoardHigh:dword
    LOCAL @@movementBitBoardLow:dword, @@movementBitBoardHigh:dword
    USES eax, ebx

    mov [movement_bit_board_low], 0
    mov [movement_bit_board_high], 0

    ;only get the bit at 'position' from the bitboards it can be in the low or high part
    call isolateBitFromBitBoards, [@@bitBoardLow], [@@bitBoardHigh], [@@position]


    cmp [@@position], 48
    jl @@single_shift
    call repeatShiftRotateRight, 8, 00000000h, BOTTOMWALL, 2, [@@enemyBitBoardLow], [@@enemyBitBoardHigh]

@@single_shift:
    call repeatShiftRotateRight, 8, 00000000h, BOTTOMWALL, 1, [@@enemyBitBoardLow], [@@enemyBitBoardHigh]


    ;store one or two steps forward in the movement bitboard
    mov eax, [movement_bit_board_low]
    mov ebx, [movement_bit_board_high]

    ; Remove straight ahead blocked positions since only pawn can't capture in its moving direction
    not [@@enemyBitBoardLow]
    not [@@enemyBitBoardHigh]

    and eax, [@@enemyBitBoardLow]
    and ebx, [@@enemyBitBoardHigh]

    not [@@enemyBitBoardLow]
    not [@@enemyBitBoardHigh]

    ; Store the result in the movement bitboards

    mov [@@movementBitBoardLow], eax
    mov [@@movementBitBoardHigh], ebx

    mov [movement_bit_board_low], 0
    mov [movement_bit_board_high], 0

    ;calculate diagonal captures
    call repeatShiftRotateRight, 7, LEFTWALL, BOTTOMWALL, 1, [@@enemyBitBoardLow], [@@enemyBitBoardHigh]
    call repeatShiftRotateRight, 9, RIGHTWALL, BOTTOMWALL, 1, [@@enemyBitBoardLow], [@@enemyBitBoardHigh]

    mov eax, [movement_bit_board_low]
    mov ebx, [movement_bit_board_high]

    and eax, [@@enemyBitBoardLow]
    and ebx, [@@enemyBitBoardHigh]

    ; combine the diagonal captures with the movement bitboard
    or [@@movementBitBoardLow], eax
    or [@@movementBitBoardHigh], ebx


    mov eax, [@@movementBitBoardLow]
    mov ebx, [@@movementBitBoardHigh]

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
    
ENDP generateBlackPawnMovementBitBoard


PROC generateBishopMovementBitBoard
    ARG @@bitBoardLow:dword, @@bitBoardHigh:dword, @@blockingBitBoardLow:dword, @@blockingBitBoardHigh:dword, @@position:dword, @@iterations:dword, @@enemyBitBoardLow:dword, @@enemyBitBoardHigh:dword
    LOCAL @@movementBitBoardLow:dword, @@movementBitBoardHigh:dword
    USES eax, ebx

    mov [movement_bit_board_low], 0
    mov [movement_bit_board_high], 0

    ;only get the bit at 'position' from the bitboards it can be in the low or high part
    call isolateBitFromBitBoards, [@@bitBoardLow], [@@bitBoardHigh], [@@position]

    ;mask / remove left column since they can't move left
    
    call repeatShiftRotateLeft, 7, LEFTWALL, TOPWALL, [@@iterations], [@@enemyBitBoardLow], [@@enemyBitBoardHigh]
    call repeatShiftRotateLeft, 9, RIGHTWALL, TOPWALL, [@@iterations], [@@enemyBitBoardLow], [@@enemyBitBoardHigh]
    call repeatShiftRotateRight, 7, RIGHTWALL, BOTTOMWALL, [@@iterations], [@@enemyBitBoardLow], [@@enemyBitBoardHigh]
    call repeatShiftRotateRight, 9, LEFTWALL, BOTTOMWALL, [@@iterations], [@@enemyBitBoardLow], [@@enemyBitBoardHigh]

    mov eax, [movement_bit_board_low]
    mov ebx, [movement_bit_board_high]

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
    ARG @@bitBoardLow:dword, @@bitBoardHigh:dword, @@blockingBitBoardLow:dword, @@blockingBitBoardHigh:dword, @@position:dword, @@iterations:dword, @@enemyBitBoardLow:dword, @@enemyBitBoardHigh:dword
    LOCAL @@movementBitBoardLow:dword, @@movementBitBoardHigh:dword
    USES eax, ebx

    mov [movement_bit_board_low], 0
    mov [movement_bit_board_high], 0

    call isolateBitFromBitBoards, [@@bitBoardLow], [@@bitBoardHigh], [@@position]
    call repeatShiftRotateLeft, 6, LEFTWALLDOUBLE, TOPWALL, 1, [@@enemyBitBoardLow], [@@enemyBitBoardHigh]
    call repeatShiftRotateLeft, 15, LEFTWALL, TOPWALLDOUBLE, 1, [@@enemyBitBoardLow], [@@enemyBitBoardHigh]
    call repeatShiftRotateLeft, 10, RIGHTWALLDOUBLE, TOPWALL, 1, [@@enemyBitBoardLow], [@@enemyBitBoardHigh]
    call repeatShiftRotateLeft, 17, RIGHTWALL, TOPWALLDOUBLE, 1, [@@enemyBitBoardLow], [@@enemyBitBoardHigh]
    call repeatShiftRotateRight, 6, RIGHTWALLDOUBLE, BOTTOMWALL, 1, [@@enemyBitBoardLow], [@@enemyBitBoardHigh]
    call repeatShiftRotateRight, 15, RIGHTWALL, BOTTOMWALLDOUBLE, 1, [@@enemyBitBoardLow], [@@enemyBitBoardHigh]
    call repeatShiftRotateRight, 10, LEFTWALLDOUBLE, BOTTOMWALL, 1, [@@enemyBitBoardLow], [@@enemyBitBoardHigh]
    call repeatShiftRotateRight, 17, LEFTWALL, BOTTOMWALLDOUBLE, 1, [@@enemyBitBoardLow], [@@enemyBitBoardHigh]

    mov eax, [movement_bit_board_low]
    mov ebx, [movement_bit_board_high]
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
ENDP generateKnightMovementBitBoard

PROC generateRookMovementBitBoard
    ARG @@bitBoardLow:dword, @@bitBoardHigh:dword, @@blockingBitBoardLow:dword, @@blockingBitBoardHigh:dword, @@position:dword, @@iterations:dword, @@enemyBitBoardLow:dword, @@enemyBitBoardHigh:dword
    LOCAL @@movementBitBoardLow:dword, @@movementBitBoardHigh:dword
    USES eax, ebx

    mov [movement_bit_board_low], 0
    mov [movement_bit_board_high], 0

    ;only get the bit at 'position' from the bitboards it can be in the low or high part
    call isolateBitFromBitBoards, [@@bitBoardLow], [@@bitBoardHigh], [@@position]
        
    ;call repeatShiftRotateLeft, 8, 0, TOPWALL, @@iterations, @@enemyBitBoardLow, @@enemyBitBoardHigh
    ;call repeatShiftRotateLeft, 1, RIGHTWALL, 0, @@iterations, @@enemyBitBoardLow, @@enemyBitBoardHigh
    ;call repeatShiftRotateRight, 8, 0, BOTTOMWALL, @@iterations, @@enemyBitBoardLow, @@enemyBitBoardHigh
    ;call repeatShiftRotateRight, 1, LEFTWALL, 0, @@iterations, @@enemyBitBoardLow, @@enemyBitBoardHigh

    ; For some vague reason, the above code doesn't work, so I'm using the following code instead
    call repeatShiftRotateLeft, 8, 00000000h, TOPWALL, [@@iterations], [@@enemyBitBoardLow], [@@enemyBitBoardHigh]
    call repeatShiftRotateLeft, 1, RIGHTWALL, TOPWALL, [@@iterations], [@@enemyBitBoardLow], [@@enemyBitBoardHigh]
    call repeatShiftRotateRight, 8, 00000000h, BOTTOMWALL, [@@iterations], [@@enemyBitBoardLow], [@@enemyBitBoardHigh]
    call repeatShiftRotateRight, 1, LEFTWALL, 00000000h, [@@iterations], [@@enemyBitBoardLow], [@@enemyBitBoardHigh]


    mov eax, [movement_bit_board_low]
    mov ebx, [movement_bit_board_high]
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

PROC generateQueenMovementBitBoard
    ARG @@bitBoardLow:dword, @@bitBoardHigh:dword, @@blockingBitBoardLow:dword, @@blockingBitBoardHigh:dword, @@position:dword, @@iterations:dword, @@enemyBitBoardLow:dword, @@enemyBitBoardHigh:dword
    USES eax, ebx

    call generateBishopMovementBitBoard, [@@bitBoardLow], [@@bitBoardHigh], [@@blockingBitBoardLow], [@@blockingBitBoardHigh], [@@position], [@@iterations], [@@enemyBitBoardLow], [@@enemyBitBoardHigh]
    
    mov eax, [movement_bit_board_low]
    mov ebx, [movement_bit_board_high]

    call generateRookMovementBitBoard, [@@bitBoardLow], [@@bitBoardHigh], [@@blockingBitBoardLow], [@@blockingBitBoardHigh], [@@position], [@@iterations], [@@enemyBitBoardLow], [@@enemyBitBoardHigh]
    or eax, [movement_bit_board_low]
    or ebx, [movement_bit_board_high]

    mov [movement_bit_board_low], eax
    mov [movement_bit_board_high], ebx

    ret
ENDP generateQueenMovementBitBoard

PROC generateKingMovementBitBoard
    ARG @@bitBoardLow:dword, @@bitBoardHigh:dword, @@blockingBitBoardLow:dword, @@blockingBitBoardHigh:dword, @@position:dword, @@iterations:dword, @@enemyBitBoardLow:dword, @@enemyBitBoardHigh:dword

    call generateQueenMovementBitBoard, [@@bitBoardLow], [@@bitBoardHigh], [@@blockingBitBoardLow], [@@blockingBitBoardHigh], [@@position], 1, [@@enemyBitBoardLow], [@@enemyBitBoardHigh]
    
    ret
ENDP generateKingMovementBitBoard

PROC isEnemyPieceAtPosition
    ARG @@bitBoardLow:dword, @@bitBoardHigh:dword, @@position:dword
    LOCAL @@result:dword
    USES eax, ecx, edx

    mov eax, 1                     
    mov ecx, [@@position]     
    cmp ecx, -1
    je @@not_found      
    cmp ecx, 31                    
    jg @@high                      

    ; Handle low bitboard
    shl eax, cl                    
    and eax, [@@bitBoardLow]       
    test eax, eax                  
    jnz @@found                   

    jmp @@not_found                

@@high:
    sub ecx, 32                    
    shl eax, cl                    
    and eax, [@@bitBoardHigh]      
    test eax, eax                  
    jnz @@found                    

@@not_found:
    mov [piece_at_position], 0
    ret

@@found:
    mov [piece_at_position], 1
    ret
ENDP isEnemyPieceAtPosition

PROC bitBoardToPosition
    ARG @@bitBoardLow:dword, @@bitBoardHigh:dword
    USES eax, ebx, ecx

    ; Check low part of the bitboard
    mov eax, [@@bitBoardLow]       ; Load low bitboard
    test eax, eax                  ; Check if low part is non-zero
    jnz @@process_low              ; If non-zero, process low bitboard

    ; If low part is zero, check the high part
    mov eax, [@@bitBoardHigh]      ; Load high bitboard
    test eax, eax                  ; Check if high part is non-zero
    jz @@error                     ; If both are zero, return an error (-1)

    ; Process high part
    bsf ecx, eax                   ; Find the first set bit in the high bitboard
    add ecx, 32                    ; Adjust position to account for high bitboard
    mov [position], ecx          ; Store the position (32–63)
    ret

@@process_low:
    bsf ecx, eax                   ; Find the first set bit in the low bitboard
    mov [position], ecx          ; Store the position (0–31)
    ret

@@error:
    mov [position], -1           ; Return -1 to indicate an error
    ret
ENDP bitBoardToPosition

;copied from MOUSE.ASM example
PROC terminateProcess
	USES eax
	call setVideoMode, 03h
	mov	ax,04C00h
	int 21h
	ret
ENDP terminateProcess

;copied from MOUSE.ASM example
PROC waitForSpecificKeystroke
	ARG 	@@key:byte
	USES 	eax

	@@waitForKeystroke:
		mov	ah,00h
		int	16h
		cmp	al,[@@key]
	jne	@@waitForKeystroke

	ret
ENDP waitForSpecificKeystroke


;copied from MOUSE.ASM example
PROC mouse_present
    USES    ebx

    mov     eax, 0
    int     33h

    and     eax, 1

    ret
ENDP mouse_present


;copied from MOUSE.ASM example
PROC mouse_internal_handler NOLANGUAGE
    push    ds
    push    es
    push    ax

    mov     ax, [cs:theDS]
    mov     ds, ax
    mov     es, ax

    pop     ax

    call    [custom_mouse_handler]
    
    pop     es
    pop     ds
    
    retf

    ; Internal variable to keep track of DS
    theDS   dw  ?
ENDP mouse_internal_handler


;copied from MOUSE.ASM example
PROC mouse_install
    ARG     @@custom_handler
    USES    eax, ecx, edx, es

    call    mouse_present
    cmp     eax, 1
    jne     @@no_mouse

    mov     eax, [@@custom_handler]
    mov     [custom_mouse_handler], eax

    push    ds
    mov     ax, cs
    mov     ds, ax
    ASSUME  ds:_TEXT
    mov     [theDS], ax
    ASSUME  ds:FLAT
    pop     ds

    mov     eax, 0ch
    mov     ecx, 255
    push    cs
    pop     es
    mov     edx, offset mouse_internal_handler
    int     33h

@@no_mouse:
    ret
ENDP mouse_install


;copied from MOUSE.ASM example
PROC mouse_uninstall
    USES    eax, ecx, edx

    mov     eax, 0ch
    mov     ecx, 0
    mov     edx, 0
    int     33h

    ret
ENDP mouse_uninstall

PROC convertScreenPositionToBitBoardPosition
    ARG @@xp:dword, @@yp:dword
    USES eax, ebx, ecx, edx

    ; convert y position to bitboard position
    mov eax, [@@yp]
    mov ebx, TILESIZE
    xor edx, edx
    div ebx
    mov ebx, 7
    sub ebx, eax
    mov [board_click_y], ebx

    ; chech if the click is on the board
    mov eax, [@@xp]
    cmp eax, PADDING
    jl @@off_screen

    mov ebx, PADDING
    add ebx, BOARDDIMENSION
    cmp eax, ebx
    jge @@off_screen

    sub eax, PADDING
    xor edx, edx
    mov ebx, TILESIZE
    div ebx
    mov [board_click_x], eax
    ret

@@off_screen:
    mov [board_click_x], -1
    mov [board_click_y], -1
    ret
ENDP convertScreenPositionToBitBoardPosition

PROC locateWhitePiece
    ARG @@pieceMaskLow:dword, @@pieceMaskHigh:dword

    mov eax, [white_pawns_low]
    mov ebx, [white_pawns_high]
    and eax, @@pieceMaskLow
    and ebx, @@pieceMaskHigh
    cmp eax, 0
    jnz @@pawn_found
    cmp ebx, 0
    jz @@no_pawn

@@pawn_found:
    mov [active_bit_board_low], eax
    mov [active_bit_board_high], ebx
    mov eax, TYPEPAWN
    mov [active_bit_board_type], eax
    ret

@@no_pawn:
    mov eax, [white_knights_low]
    mov ebx, [white_knights_high]
    and eax, @@pieceMaskLow
    and ebx, @@pieceMaskHigh
    cmp eax, 0
    jnz @@knight_found
    cmp ebx, 0
    jz @@no_knight

@@knight_found:
    mov [active_bit_board_low], eax
    mov [active_bit_board_high], ebx
    mov eax, TYPEKNIGHT
    mov [active_bit_board_type], eax
    ret

@@no_knight:
    mov eax, [white_bishops_low]
    mov ebx, [white_bishops_high]
    and eax, @@pieceMaskLow
    and ebx, @@pieceMaskHigh
    cmp eax, 0
    jnz @@bishop_found
    cmp ebx, 0
    jz @@no_bishop

@@bishop_found:
    mov [active_bit_board_low], eax
    mov [active_bit_board_high], ebx
    mov eax, TYPEBISHOP
    mov [active_bit_board_type], eax
    ret

@@no_bishop:
    mov eax, [white_rooks_low]
    mov ebx, [white_rooks_high]
    and eax, @@pieceMaskLow
    and ebx, @@pieceMaskHigh
    cmp eax, 0
    jnz @@rook_found
    cmp ebx, 0
    jz @@no_rook

@@rook_found:
    mov [active_bit_board_low], eax
    mov [active_bit_board_high], ebx
    mov eax, TYPEROOK
    mov [active_bit_board_type], eax
    ret

@@no_rook:
    mov eax, [white_queens_low]
    mov ebx, [white_queens_high]
    and eax, @@pieceMaskLow
    and ebx, @@pieceMaskHigh
    cmp eax, 0
    jnz @@queen_found
    cmp ebx, 0
    jz @@no_queen

@@queen_found:
    mov [active_bit_board_low], eax
    mov [active_bit_board_high], ebx
    mov eax, TYPEQUEEN
    mov [active_bit_board_type], eax
    ret

@@no_queen:
    mov eax, [white_kings_low]
    mov ebx, [white_kings_high]
    and eax, @@pieceMaskLow
    and ebx, @@pieceMaskHigh
    cmp eax, 0
    jnz @@king_found
    cmp ebx, 0
    jz @@no_king

@@king_found:
    mov [active_bit_board_low], eax
    mov [active_bit_board_high], ebx
    mov eax, TYPEKING
    mov [active_bit_board_type], eax
    ret

@@no_king:
    mov [active_bit_board_low], 0
    mov [active_bit_board_high], 0
    mov eax, 0
    mov [active_bit_board_type], eax
    ret
    
ENDP locateWhitePiece

PROC locateBlackPiece
    ARG @@pieceMaskLow:dword, @@pieceMaskHigh:dword

    mov eax, [black_pawns_low]
    mov ebx, [black_pawns_high]
    and eax, @@pieceMaskLow
    and ebx, @@pieceMaskHigh
    cmp eax, 0
    jnz @@pawn_found
    cmp ebx, 0
    jz @@no_pawn

@@pawn_found:
    mov [active_bit_board_low], eax
    mov [active_bit_board_high], ebx
    mov eax, TYPEPAWN
    mov [active_bit_board_type], eax
    ret

@@no_pawn:
    mov eax, [black_knights_low]
    mov ebx, [black_knights_high]
    and eax, @@pieceMaskLow
    and ebx, @@pieceMaskHigh
    cmp eax, 0
    jnz @@knight_found
    cmp ebx, 0
    jz @@no_knight

@@knight_found:
    mov [active_bit_board_low], eax
    mov [active_bit_board_high], ebx
    mov eax, TYPEKNIGHT
    mov [active_bit_board_type], eax
    ret

@@no_knight:
    mov eax, [black_bishops_low]
    mov ebx, [black_bishops_high]
    and eax, @@pieceMaskLow
    and ebx, @@pieceMaskHigh
    cmp eax, 0
    jnz @@bishop_found
    cmp ebx, 0
    jz @@no_bishop

@@bishop_found:
    mov [active_bit_board_low], eax
    mov [active_bit_board_high], ebx
    mov eax, TYPEBISHOP
    mov [active_bit_board_type], eax
    ret

@@no_bishop:
    mov eax, [black_rooks_low]
    mov ebx, [black_rooks_high]
    and eax, @@pieceMaskLow
    and ebx, @@pieceMaskHigh
    cmp eax, 0
    jnz @@rook_found
    cmp ebx, 0
    jz @@no_rook

@@rook_found:
    mov [active_bit_board_low], eax
    mov [active_bit_board_high], ebx
    mov eax, TYPEROOK
    mov [active_bit_board_type], eax
    ret

@@no_rook:
    mov eax, [black_queens_low]
    mov ebx, [black_queens_high]
    and eax, @@pieceMaskLow
    and ebx, @@pieceMaskHigh
    cmp eax, 0
    jnz @@queen_found
    cmp ebx, 0
    jz @@no_queen

@@queen_found:
    mov [active_bit_board_low], eax
    mov [active_bit_board_high], ebx
    mov eax, TYPEQUEEN
    mov [active_bit_board_type], eax
    ret

@@no_queen:
    mov eax, [black_kings_low]
    mov ebx, [black_kings_high]
    and eax, @@pieceMaskLow
    and ebx, @@pieceMaskHigh
    cmp eax, 0
    jnz @@king_found
    cmp ebx, 0
    jz @@no_king

@@king_found:
    mov [active_bit_board_low], eax
    mov [active_bit_board_high], ebx
    mov eax, TYPEKING
    mov [active_bit_board_type], eax
    ret

@@no_king:
    mov [active_bit_board_low], 0
    mov [active_bit_board_high], 0
    mov eax, 0
    mov [active_bit_board_type], eax
    ret

ENDP locateBlackPiece

PROC renderWhiteMovementBitBoard
    ARG @@position:dword
    USES eax, ebx, ecx, edx
    mov eax, [active_bit_board_type]
    cmp eax, TYPEPAWN
    jne @@not_pawn
    call generateWhitePawnMovementBitBoard, [active_bit_board_low], [active_bit_board_low], [combine_low], [combine_high], [@@position], 1, [black_combine_low], [black_combine_high]
    ret

@@not_pawn:
    cmp eax, TYPEKNIGHT
    jne @@not_knight
    call generateKnightMovementBitBoard, [active_bit_board_low], [active_bit_board_low], [combine_low], [combine_high], [@@position], 1, [black_combine_low], [black_combine_high]
    ret

@@not_knight:
    cmp eax, TYPEBISHOP
    jne @@not_bishop
    call generateBishopMovementBitBoard, [active_bit_board_low], [active_bit_board_low], [combine_low], [combine_high], [@@position], 1, [black_combine_low], [black_combine_high]
    ret

@@not_bishop:
    cmp eax, TYPEROOK
    jne @@not_rook
    call generateRookMovementBitBoard, [active_bit_board_low], [active_bit_board_low], [combine_low], [combine_high], [@@position], 1, [black_combine_low], [black_combine_high]
    ret

@@not_rook:
    cmp eax, TYPEQUEEN
    jne @@not_queen
    call generateQueenMovementBitBoard, [active_bit_board_low], [active_bit_board_low], [combine_low], [combine_high], [@@position], 1, [black_combine_low], [black_combine_high]
    ret

@@not_queen:
    cmp eax, TYPEKING
    jne @@not_king
    call generateKingMovementBitBoard, [active_bit_board_low], [active_bit_board_low], [combine_low], [combine_high], [@@position], 1, [black_combine_low], [black_combine_high]
    ret

@@not_king:
    ret
ENDP renderWhiteMovementBitBoard

PROC mouseHandler
    USES eax, ebx, ecx, edx
    LOCAL @@x:dword, @@y:dword, @@xp:dword, @@yp:dword, @@buttonstates:dword, @@position:dword

    mov [@@buttonstates], ebx

    movzx eax, dx		; get mouse height
    mov [@@yp], eax     ; store y position

    sar cx, 1			; horizontal cursor position is doubled in input 
    mov [@@xp], ecx     ; store x position

    ; Stores board position in [board_click_x] and [board_click_y]
    call convertScreenPositionToBitBoardPosition, [@@xp], [@@yp]

    call drawEmptyBoard 
    ;since background never changes we can store it and copy it instead of redrawing
    ;so first copy the background to the buffer
    ;then draw the pieces and other stuff
    call copyBackgroundToBuffer
    call drawPieces


    and [@@buttonstates], 3			; check for two mouse buttons (2 low end bits)
	jz @@no_press		; only execute if a mousebutton is pressed



@@press:

    ; Handle of board click
    cmp [board_click_x], -1
    je @@no_press

    ; convert x, y to bitboard position (0-63)
    mov eax, [board_click_y]
    mov ebx, 8
    mul ebx
    ;add [@@x] to the result
    add eax, [board_click_x]
    mov [@@position], eax

    ; Reset movement bitboards
    mov [movement_bit_board_low], 0
    mov [movement_bit_board_high], 0

    ; Generate a bitboard masks for piece at position
    call positionToBitBoards, [@@position]
    ; Check if there is a white piece at the position using the bitboard masks
    call locateWhitePiece, [active_bit_board_mask_low], [active_bit_board_mask_high]
    ; If there was a piece at the position, render the movement bitboard
    call renderWhiteMovementBitBoard, [@@position]

    call drawBitBoard, [movement_bit_board_low], offset _indicator, 0
    call drawBitBoard, [movement_bit_board_high], offset _indicator, 32
@@no_press:
    call drawSpritePixel, offset _arrow, offset _screenBuffer, [@@xp], [@@yp]

    call copyBufferToVideoMemory

    ret
ENDP mouseHandler

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

    TOPWALLDOUBLE EQU 0FFFF0000h
    BOTTOMWALLDOUBLE EQU 0000000FFFFh
    LEFTWALLDOUBLE EQU 003030303h
    RIGHTWALLDOUBLE EQU 0C0C0C0C0h

    TYPEPAWN EQU 1
    TYPEKNIGHT EQU 2
    TYPEBISHOP EQU 3
    TYPEROOK EQU 4
    TYPEQUEEN EQU 5
    TYPEKING EQU 6

    
    EXTRN printUnsignedNumber:PROC
    EXTRN printNewline:PROC
    EXTRN updateColourPalette:PROC
    EXTRN setVideoMode:PROC

    call setupBitboards
    call combineWhiteBitBoards
    call combineBlackBitBoards

    mov eax, [white_combine_low]
    mov ebx, [white_combine_high]
    or eax, [black_combine_low]
    or ebx, [black_combine_high]
    mov [combine_low], eax
    mov [combine_high], ebx

    call    mouse_present
    cmp     eax, 1
    je      @@mouse_present

    mov     ah, 9
    mov     edx, offset msg_no_mouse
    int     21h

@@mouse_present:
    call setVideoMode, 13h
    call updateColourPalette, 6
    mov EDI, VMEMADR

    call drawEmptyBoard 
    ;since background never changes we can store it and copy it instead of redrawing
    ;so first copy the background to the buffer
    ;then draw the pieces and other stuff
    call copyBackgroundToBuffer
    call drawPieces
    call copyBufferToVideoMemory

    call mouse_install, offset mouseHandler

    call waitForSpecificKeystroke, 001Bh ; keycode for ESC
    call terminateProcess
ENDP main



; -------------------------------------------------------------------
; DATA
; -------------------------------------------------------------------
DATASEG
_screenBuffer db 64000 dup(?) ; Video buffer
_background_buffer db 64000 dup(?) ; background buffer

msg_no_mouse    db 'No mouse found!', 0dh, 0ah, '$'

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

combine_low DD 0 ; Low 32 bits
combine_high DD 0 ; High 32 bits

test_low DD 0FFFFFFFFh  ; Define a 32-bit value with all bits set to 1
test_high DD 0FFFFFFFFh ; Same for the high part

block_low DD 0 ; Low 32 bits
block_high DD 0 ; High 32 bits

movement_bit_board_high DD 0 ; High 32 bits
movement_bit_board_low  DD 0 ; Low 32 bits

piece_at_position DD 0
position DD 0


isolated_bit_low DD 0
isolated_bit_high DD 0

colour_to_draw      DD 0

custom_mouse_handler    dd ?

board_click_x dd 0
board_click_y dd 0
active_bit_board_type dd 0
active_bit_board_low dd 0
active_bit_board_high dd 0
active_bit_board_mask_low dd 0
active_bit_board_mask_high dd 0

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

_arrow dw 7, 10
       db 0, 0, 9, 9, 9, 9, 9
       db 0, 1, 0, 9, 9, 9, 9
       db 0, 1, 1, 0, 9, 9, 9
       db 0, 1, 1, 1, 0, 9, 9
       db 0, 1, 1, 1, 1, 0, 9
       db 0, 1, 1, 1, 1, 1, 0
       db 0, 1, 1, 1, 0, 0, 0
       db 0, 1, 0, 1, 1, 0, 9
       db 0, 0, 0, 1, 1, 0, 9
       db 9, 9, 9, 0, 0, 9, 9

; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 400h

END main