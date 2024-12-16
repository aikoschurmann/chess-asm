; -------------------------------------------------------------------
; 80386
; 32-bit x86 assembly language
; TASM
;
; author:	Stijn Bettens, David Blinder
; date:		25/09/2017
; program:	Hello World!
; -------------------------------------------------------------------

IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT

; -------------------------------------------------------------------
; CODE
; -------------------------------------------------------------------
CODESEG
PUBLIC setVideoMode
PROC setVideoMode
	ARG 	@@VM:byte
	USES 	eax

	movzx ax,[@@VM]
	int 10h

	ret
ENDP setVideoMode

PUBLIC printUnsignedNumber
PROC printUnsignedNumber
    ARG @@number:dword
    USES eax, ebx, ecx, edx
    
    mov eax, [@@number] ; eax holds input integer
    mov ebx, 10         ; divider
    xor ecx, ecx        ; reset counter for digits to be printed (used by loop)

@@getNextDigit:
    ;loop decrements ecx until it reaches 0 
    ;later used for eg. printing digits
    inc ecx
    ; DIV uses EDX:EAX as input and output (64-bit dividend)
    ; We often divide 32-bit numbers, so we need to clear EDX first
    ; EDX is the high 32 bits of the dividend (64-bit number)
    xor edx, edx
    div ebx
    push edx            ;32-bit remainder is pushed onto the stack
    test eax, eax       ;sets the zero flag if eax is zero
    jnz @@getNextDigit  ;if not zero flag, repeat the process

@@printDigits:
    pop edx             ;pop the last digit from the stack
    add dl, '0'         ;convert the digit to ASCII
    mov ah, 2h          ;function for printing single characters
    int 21h             ;print the digit to the screen
    loop @@printDigits  ;untill ecx = 0

    ret
ENDP printUnsignedNumber


PUBLIC printNewline
PROC printNewline
    USES eax, ebx, ecx, edx

    mov dl, 0Dh         ; Carriage return.
    mov ah, 2h          ; Function for printing single characters.
    int 21h             ; Print the character to the screen.

    mov dl, 0Ah         ; New line.
    mov ah, 2h          ; Function for printing single characters.
    int 21h             ; Print the character to the screen.

    ret
ENDP printNewline

PUBLIC updateColourPalette
PROC updateColourPalette
	ARG	 	@@Ncolours: word
	USES 	eax, ecx, edx, esi

	mov esi, offset palette	; pointer to source palette
	movzx ecx, [@@Ncolours] ; amount of colors to read (movzx = zero extend)
	
	; multiply ecx by 3 (three color components per color)
	; do it efficiently (2*ecx + ecx)
	mov eax, ecx
	sal eax, 1
	add ecx, eax

	mov dx, 03C8h 	; DAC write port
	xor al, al		; index of first color to change (0)
	out dx, al		; write to IO

	inc dx
	rep outsb		; update all colors

	ret
ENDP updateColourPalette


DATASEG
palette		db 0, 0, 0
			db 63, 63, 63
			db 42, 28, 14
			db 63, 63, 63

END