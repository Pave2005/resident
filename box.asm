space     		equ 20h
bttm_line 		equ 00h
top_line 		equ 01h
mid_line 		equ 02h

top_left_angl   equ   0
top_fr_line     equ   1
top_right_angl  equ   2
left_fr_side    equ   3
inner_place     equ   4
right_fr_side   equ   5
bttm_left_angl  equ   6
bttm_fr_line 	equ   7
bttm_right_angl equ   8

fr_code 		equ   4
colour 			equ   6
height 			equ   8
fr_width 		equ  10

shadow_colour 	equ 56h

descr_len 		equ   9

end_of_string   equ '$'

;===============================================================================
;Labels that return the character code that should be printed in the one part
;of the frame.
;
;Used registers:	bx - address of the cell from the array with the
;			     		 characteristics of the frame that stores the
;			     		 symbol that is contained on the left side of the
;			     		 frame.
;					ax - the register in which the offset is stored to the
;			     		 beginning of the part with the pattern of a
;			     		 certain frame.
;					cx - offset to the cell with symbol code.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------

;===============================================================================
;This macros, depending on the value of the di register, transmits a character
;to the al register, which should be drawn in the one of the parts of the frame.
;
;Used registers:			di - stores the value of the frame string mode.
;
;Labels:             NotTopSym - the label after which the top frame
;					   		     symbol is not returned.
;			        NotBttmSym - the label after which the bottom
;					   		     frame symbol is not returned.
;			     FinishSetCode - the label that returns the program to
;					   		     the end of macros.
;
;Input arguments:		    ax - offset to the beginning of the frame pattern.
;					top_fr_sym - offset to array cell that contains the
;								 symbol of the top of the frame.
;				   bttm_fr_sym - offset to array cell that contains the
;								 symbol of the bottom of the frame.
;				 middle_fr_sym - offset to array cell that contains the
;								 symbol of the middle of the frame.
;
;Returned value:	        al - symbol of the frame line.
;-------------------------------------------------------------------------------
.SetSymCode 	macro top_fr_sym, bttm_fr_sym, middle_fr_sym, NotTopSym, NotBttmSym, FinishSetCode

				cmp di, top_line
				jne NotTopSym

				mov al, 03h
				jmp FinishSetCode

NotTopSym: 		cmp di, bttm_line
				jne NotBttmSym

				mov al, 03h
				jmp FinishSetCode

NotBttmSym:		mov al, 20h

FinishSetCode:
				endm
;-------------------------------------------------------------------------------
;===============================================================================
;This function checks for parity the number that lies in the ax register.
;
;Used registers: 	ax
;
;Increases the ax value by 1 if it is odd.
;-------------------------------------------------------------------------------
MakeEven    proc

			test ax, 1
			jnz OddNum
			jmp EvenNum

OddNum:		inc ax

EvenNum:	ret

endp
;===============================================================================
;This function draws an empty part of the frame on the screen, that is, a
;a coloured line.
;
;Used registers:	dx - takes the intermediate value of the video memory
;			     		 start address and passed it to the es register.
;					es - stores the start address of the video memory.
;					dl - stores the colour code of the frame element.
;					cx - stores the width of the frame.
;					bx - the number of the video memory cell in which the
; 			     		 the symbol should be placed.
;
;Input arguments:	bx - transmitted in the frame drawing function.
;
;Return value:		bx - incresed value by the width of the frame, given
;			     		 that each symbil occupies two cells.
;-------------------------------------------------------------------------------
DrawEmptyLine	proc

				push cx
				push dx

				mov dl, [bp + colour]			; color

				mov cx, [bp + fr_width]			; ax = width

PrintLine:		mov byte ptr es:[bx],  space
				mov byte ptr es:[bx + 1], dl 	; color
				add bx, 2
				loop PrintLine

				pop dx
				pop cx

				ret
				endp
;===============================================================================
;This function draws a string with a frame pattern.
;
;Used registers: 	ax - offset to the first code of a certain frame in
;			     		 array.
;					dx - intermediate storage location of the video memory
;			     		 start address.
;					es - video memory start address.
;					dl - color code.
;					cx - length of the inner part of the frame.
;					bp - the start of the reference in the stack, from
;			     		 which the places of the arguments specified in the
;			     		 main function are counted.
;					si - the end of the stack.
;					bx - offset from the start of the video memory to the
;			     		 cell in which the symbol should be placed.
;
;Input arguments:	bx
;					bp
;
;Return value:		bx - offset changed to the length of the frame string.
;-------------------------------------------------------------------------------
DrawFrLines 	proc

				push cx
				push dx

				mov dl, [bp + colour]			; color

				mov byte ptr es:[bx],    space
				mov byte ptr es:[bx + 1],   dl

				mov byte ptr es:[bx + 2],  03h 	; angle code in video mem
				mov byte ptr es:[bx + 3],   dl 	; color code in video mem

				; select a symbol for the middle part of the frame
				.SetSymCode top_fr_line, bttm_fr_line, inner_place, NotTopLine, NotBttmLine, FinishLines

				mov cx, [bp + fr_width]			; width of frame
				sub cx, 4

				add bx, 4						; bx = bx + 4 start of --
				mov dl, [bp + colour]			; al = color code

PrintFrameTop:	mov byte ptr es:[bx],       al	; -- code in vid mem
				mov byte ptr es:[bx + 1],   dl 	; color code in vid mem
				add bx, 2
				loop PrintFrameTop

				mov dl, [bp + colour]			; color code

				mov byte ptr es:[bx],      03h
				mov byte ptr es:[bx + 1],   dl

				mov byte ptr es:[bx+ 2], space 	; space code
				mov byte ptr es:[bx + 3],   dl

				pop dx
				pop cx

				ret
				endp
;===============================================================================
;This function draws a frame depending on the arguments passed on the command
;line.
;
;Used registers:	ax - offset from the edge of the screen to the edge of
;			     		 the frame.
;					cx - temporarily stores the width of the frame.
;			  		   - at the begininning of the program, it takes the
;			     		 value of the frame height, at the end it is reset
;			     		 thus the program draws the frame element in the loop.
;					dx - stores the value of the frame height, because of
;			     		 this, when the value of the cycle counter is
;			     		 compared with dx, a certain frame element is drawn.
;					al - contains offset in bytes from the top edge of the
;			     		 screen to the beginning of a certain line of the frame.
;					bx - offset along the edge of the beginning of the
;			     		 frame line.
;					si - pointer to the start of the stack.
;
;Input arguments:	0
;
;Return value:		bx - depending on the line you want to draw, it changes
;			     	     its value.
;					cx - reset in the cycle.
;-------------------------------------------------------------------------------
DrawFrame		proc
				push bp 						; stack pointer to pushed elems
				mov bp, sp

                .SetVidMemAddr

				mov ax, 80
				mov cx, [bp + fr_width]			; width
				sub ax, cx  					; ax = Start place of farme
				call MakeEven

				push ax  						; 1 elem of frame line
				xor ax, ax
				xor cx, cx
				mov cx, [bp + height]			; height of picture
				mov dx, cx 						; dx = height

PrintCollmn:	mov al, 160
				mul cl
				mov bx, ax 						; bx = idx of 0 elem in our line
				xor ax, ax

                add bx, [bp - 2]

				cmp cx, dx
				jne NotBEmptyLine

				call DrawEmptyLine
				jmp Finish

NotBEmptyLine:	cmp cx, 1
				jne NotTEmptyLine

				call DrawEmptyLine
				jmp Finish

NotTEmptyLine:	cmp cx, 2
				jne NotFrameTop

				mov di, top_line
				call DrawFrLines
				jmp Finish

NotFrameTop:	sub dx, 1
				cmp cx, dx
				jne NotFrameBttm

				add dx, 1
				mov di, bttm_line
				call DrawFrLines
				jmp Finish

NotFrameBttm:	add dx, 1

				mov di, mid_line
				call DrawFrLines

Finish:			loop PrintCollmn

				pop ax 							; shift to start place of frame
				pop bp

				ret
				endp
;===============================================================================
;This function draws a shadow from the frame.
;
;Used registers:	bp - address in teh stack from which the arguments are
;			     		 counted from the command line.
;					ax - initial shadow cell in the row.
;			   		   - offset from the edge of teh screen to the starting
;			     		 point of the lower shadow.
;					bx - full offset to the shadow element from the
;			     		 beginning of the video memory.
;			   		   - line offset to the bottom of teh shadow.
;					cx - number of lines in the side of the shadow.
;			   		   - length of the bottom of the shadow.
;					es - beginning of the video memory.
;					dl - number of the lines to the bottom of teh frame.
;
;Input arguments:	es
;
;Return value:		bx
;-------------------------------------------------------------------------------
DrawShadow 		proc
				push bp
				mov bp, sp

				.SetVidMemAddr

				mov ax, 80
				add ax, [bp + fr_width]			; (160 - (80 - width))

				call MakeEven 					; start x position of shadow

				mov bx, (160 * 2)				; start y position of shadow

				add bx, ax 						; full shift

				mov cx, [bp + height]			; cx - height of frame
				sub cx, 1						; height of shadow part

PrintSidePart:	mov byte ptr es:[bx + 1], shadow_colour
				add bx, 160
				loop PrintSidePart

				mov dl, [bp + height]
				add dl, 1						; y shift

				mov ax, 160
				mul dl
				mov bx, ax 						;shift

				mov ax, 80
				sub ax, [bp + fr_width]
				add ax, 2						; 2 = 1 video elem

				call MakeEven

				add bx, ax

				mov cx, [bp + fr_width]

PrintBttmPart:	mov byte ptr es:[bx + 1], shadow_colour
				add bx, 2
				loop PrintBttmPart

				pop bp

				ret
				endp
;===============================================================================
;This function skips the cells in the command line occupied by spaces between
;the arguments.
;Recursive is included in the function if the next character is also a space.
;
;Used registers:	ah - the value of the byte in the command line.
;					bx - offset from th ebeginning of the command line.
;					es - comand line start address.
;
;Input arguments:	ah - transmitted from DrawFrame function.
;					bx
;
;Return value:		ah - in the SkipedSpaces label changes ah value to the
;			     		 next byte, if there is a space in the current one.
;					bx - increases by one if there is a space in the cell.
;-------------------------------------------------------------------------------
SkipSpaces 		proc

				cmp ah, space  					; space
				jne SkipedSpace

				inc bx
				mov ah, es:[bx]
				call SkipSpaces

SkipedSpace: 	ret
				endp


DrawBox proc

		xor ax, ax

		mov al, dl								; width
		push ax

		mov al, dh 								; height
		push ax

		mov al, ch 								; color code
		push ax

		mov al, cl 								; frame code
		push ax

		xor cx, cx
		xor dx, dx
		xor ax, ax

		mov bp, sp 								; frame code

		call DrawFrame

		pop ax ax ax ax

		ret
		endp

;===============================================================================
PrintRow	proc

            .SetVidMemAddr
			mov dx, cx

@@DrawRow: 	mov al, cs:[si]
			cmp al, '$'
            je @@Exit

            mov byte ptr es:[bx], al

			inc si
            add bx, 2

            loop @@DrawRow

            mov cx, dx

            sub bx, 4
            add bx, 80 * 2

            jmp @@DrawRow

@@Exit:		ret
			endp
;===============================================================================
