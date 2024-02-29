.model tiny
.186
.code
org 100h

locals @@

HexNumArray      db     4 DUP(0)

TableWidth      equ          11d
TableHeight     equ          18d
TableColour     equ          4ch
TableCode       equ           2d
Hotkey          equ          11d
IntCellSize     equ           4d
RegisterCount   equ          12d
HotkeyPressed   equ           '*'
HotkeyUnPressed equ           '&'
LinePos			equ           3d
RowPos 			equ (80 - 7) + 1                                ; 7 - width of the functional part

;-------------------------------------------------------------------------------
;This macro remembers the address of the standart handler and replaces it with
;a new one.
;
;Used registers:    es - the segment where the interruption data is.
;                   ax - the address in which the interruption data is stored.
;                   cs - programm segment.
;
;Input registers:   es
;
;Return value:      OldHandler - full address of the standart handler.
;-------------------------------------------------------------------------------
.ExchangeInt    macro IntNum, NewHandler, OldHandler

                mov ax, es:[IntNum * IntCellSize]
                mov word ptr cs:OldHandler, ax

                mov es:[IntNum * IntCellSize], offset NewHandler

                mov ax, es:[IntNum * IntCellSize + 2]
                mov word ptr cs:OldHandler + 2, ax

                push cs
                pop  ax

                mov es:[IntNum * IntCellSize + 2], ax

                endm
;-------------------------------------------------------------------------------
;This macro converts a part of a number into a symbol and writes it to the
;provided memory.
;
;Used registers:    dh - the fourth part of the register.
;                   al - the decimal part of the register value.
;                   ah - the single part of the register value.
;                      - the symbol code of the number in the hex number system.
;                   bx - the address pf the cell in the array where the symbol
;                        is written.
;
;Input registers:   bx
;-------------------------------------------------------------------------------
.SetNumSym      macro HexPart, SetSym

                push dx
                xor  dl, dl

                mov  al, dh
                push dx                                         ; сохраняем значение dh
                mov  dh, 10
                div  dh                                         ; теперь в al лежит десятичная часть,
                                                                ; а в ah единичная.
                pop dx
                cmp dh, 10
                jae HexPart

                add ah, '0'

                mov byte ptr cs:[bx], ah                        ; так как десятичной части нет.

                jmp SetSym

HexPart:        add ah, 'A'
                mov byte ptr cs:[bx], ah

SetSym:         inc bx
                xor ax, ax
                pop dx

                endm
;-------------------------------------------------------------------------------
.SetVidMemAddr  macro

				mov dx, 0b800h
				mov es, dx

				endm

.GetCmdLineSym  macro

				mov ah, es:[bx]
				call SkipSpaces

				endm


.GetDecNumPart  macro

				sub ah, '0'
				mov al, ah
				mov cl, 10
				mul cl

				endm

;===============================================================================
Start:      cli

            xor bx, bx
            mov es, bx

            .ExchangeInt 08h, Int08NewHandler, Int08SysHandler
            .ExchangeInt 09h, Int09NewHandler, Int09SysHandler

            sti                                                 ; разрешает прерывание

            mov ax, 3100h                                       ; сохраняет программу в память
            mov dx, offset ProgEnd
            ;shr dx, 4
            ;inc dx
            int 21h

jmp ProgEnd

include ./u_test.asm

;===============================================================================
;This function replaces the original handler, and with each tick of the timer
;prints the register value in the frame.
;
;Used registers:    bx - hot button press detection address.
;                      - the value of the register to be processed.
;                   cs - segment address.
;                   cx - numbers of registers.
;                      - number of digit in the register.
;                   si - the address of the character in the number string.
;                   dx - offset to the place in the video memory where part of
;                        the register value is stored.
;-------------------------------------------------------------------------------

Int08NewHandler     proc

                    push bx
                    lea  bx, HotkeyPass
                    cmp  byte ptr cs:[bx], HotkeyUnPressed
                    je @@SysIntHandler

                    push ss es ds sp bp di si dx cx bx ax
                    push cs ss es ds sp bp di si dx cx bx ax    ; для получения функцией информации о них

                    mov cx, RegisterCount                       ; 13 проходов в цикле.
                    lea si, HexNumArray
                    .SetVidMemAddr
                    mov dx, (80 * 2) * LinePos + RowPos + 3 * 2

@@RegValPrinting:   pop  bx
                    push cx                                     ; сохраняем значение счетчика.
                    push dx

                    call SetHexNumStr                           ; в массиве лежит чсло из регистра в 16 системе

                    pop dx
                    mov bx, dx                                  ; старт печати значения регистра.

                    mov cx, 4

@@PrintRegVal:      mov al, cs:[si]
                    mov byte ptr es:[bx], al                    ; положили в видео память кусочек от
                                                                ; значения регистра.
                    add bx, 2
                    inc si
                    loop @@PrintRegVal

                    sub si, 4
                    add dx, (80 * 2)

                    pop cx
                    loop @@RegValPrinting

                    pop ax bx cx dx si di bp sp ds es ss
;________________________________
                    call UnitTest
;________________________________
@@SysIntHandler:    pop bx
                    db 0EAh
                    Int08SysHandler dd 0

                    endp

;===============================================================================
;This function replaces the standart keyboard signal handler and, depending on
;the button pressed, displays a table with registers on the screen.
;
;Used registers:    al - scan code of the symbol pressed on the keyboard.
;                   bx - hot button press detection address.
;                      - offset from the start of video memory to the place with
;                        the register.
;                   cs - segment address.
;                   dl - frame width.
;                   dh - frame height.
;                   cl - frame code.
;                   ch - frame colour.
;                   ds - data segment address - default segment address.
;                   cx - number of letters in the name of the register.
;                   si - the address of the cell in the string with the name
;                        of the registers.
;-------------------------------------------------------------------------------
Int09NewHandler proc
                push ax

                in  al,    60h                                  ; берем из порта 60 значение символа клавиатуры
                cmp al, Hotkey                                  ; hot key - 0

                je @@PrintRegs
Int09End:       pop ax

                db 0EAh
                Int09SysHandler dd 0

@@PrintRegs:    push ax bx cx dx bp si di ds es

                lea bx, HotkeyPass
                cmp byte ptr cs:[bx], HotkeyPressed
                je @@IntEnd

                mov  byte ptr cs:[bx], HotkeyPressed

                push ax bx cx dx bp si di ds es

                mov  dl, TableWidth
		        mov  dh, TableHeight
		        mov  ch, TableColour
		        mov  cl, TableCode

                push cs
                pop ds

                call DrawBox

                pop es ds di si bp dx cx bx ax

                mov cx, 02h                                     ; 2 буквы в названии регистра
                lea si, RegsNames
			    mov bx, LinePos * (80 * 2) + RowPos

                call PrintRow                                   ; печатает названия всех регистров в столбик

                in  al, 61h
                or  al, 80h
                out 61h, al
                and al, not 80h
                out 61h, al

                mov al, 20h
                out 20h, al

@@IntEnd:       pop es ds di si bp dx cx bx ax

                jmp Int09End

                HotkeyPass db            HotkeyUnPressed
                RegsNames  db 'axbxcxdxsidibpspdsessscs$'

                endp

;===============================================================================
;This function divides the register value into parts and starts a function that
;converts the value to a string.
;
;Used registers:    bl - stores each of the four parts of the register value.
;                   cl - four lower bits of the bl register.
;                   ch - top four bits of the bl register.
;                   dl - four lower bits of the bh register.
;                   dh - top four bits of the bh register.
;
;Input registers:   bx - the register whose value needs to be converted.
;
;Return value:      HexNumArray string with the value of the register.
;                   cx
;                   dx
;                   bx
;-------------------------------------------------------------------------------
SetHexNumStr    proc

                xor cx, cx
                xor dx, dx

                push bx                                         ; сохраняем начальное значение регистра.
                xor  bh, bh                                     ; сохраняем bl
                push bx                                         ; работаем с нижними битами.

                shl bl, 4                                       ; xxxx1110 --> 00001110
                shr bl, 4                                       ; получили нижние 4 бита bl.

                mov cl, bl
;___________________________
                pop bx
                shr bl, 4                                       ; получили верхние 4 бита в bl
                                                                ; 1110xxxx --> 00001110
                mov ch, bl
;___________________________
                pop  bx
                xor  bl, bl
                push bx                                         ; save bh

                shl bh, 4
                shr bh, 4                                       ; получили нижние 4 бита верхнего регистра

                mov dl, bh
;___________________________
                pop bx                                          ; pop bh

                shr bh, 4                                       ; получили верхние 4 бита верхнего регистра

                mov dh, bh
;___________________________

                call ConvBinToHexNumStr                         ; после выполнения в массиве лежит число
                                                                ; в строчном виде.
                ret
                endp

;===============================================================================
;This function converts the passed parts of the register to a string woth a
;value.
;
;Used registers:    bx - the address of the array cell into which part of the
;                        number is inserted.
;                   dh - intermediate register that transmits part of the number
;                        of the function to be performed.
;
;Input registers:   cl - four lower bits of the bl register.
;                   ch - top four bits of the bl register.
;                   dl - four lower bits of the bh register.
;                   dh - top four bits of the bh register.
;
;Return value:      HexNumArray string with the value of the register.
;                   ax
;                   bx
;-------------------------------------------------------------------------------
ConvBinToHexNumStr  proc

                    xor ax, ax
                    xor bx, bx

                    lea bx, HexNumArray                         ; в bx нулевая ячейка массива

                    .SetNumSym HexFirstDig, SetFirstDigSym

                    mov dh, dl
                    .SetNumSym HexSecDig, SetSecDigSym

                    mov dh, ch
                    .SetNumSym HexThirdDig, SetThirdDigSym

                    mov dh, cl
                    .SetNumSym HexLastDig, SetLastDigSym

                    xor bx, bx

                    ret
                    endp

;===============================================================================

include ./box.asm

ProgEnd:

end         Start
