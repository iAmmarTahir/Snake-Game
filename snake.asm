org 100h
jmp start


message:	db	'Game Over :('
message2:	db	'Game Won :)'


flag: dw 0
buffer:times 240 dw 0
location:	dw	0
qwerty: dw 0
length:	dw	20
lives:	dw	3
gameOver:	dw	0
bigPrime:	dw	4001
Edge:	dw 	0, 160, 320, 480, 640, 800, 960, 1120, 1280, 1440, 1600, 1760, 1920, 2080, 2240, 2400, 2560, 2720, 2880, 3040, 3200, 3360, 3520, 3680, 3840
row:	dw	12
col:	dw	12
fruitPresent:	db 	0
score:	db	0
count:	db  0
stop:	db	1
speed:	dw	2300
counterSec:	dw	0

Minute: dw 3
Seconds: dw 59
ticks: dw 0
milliseconds:	dw	0

randomNumber:	dw	4001

fruity: dw	12
fruitx:	dw  12

printChange:	push es
		push ax
		push di

		mov ax, 0xb800
		mov es, ax
		mov di, 0

nextchar12:      mov word[es:di], 0x7020
		add di, 2
		cmp di, 4000
                jne nextchar12

		pop di
		pop ax
		pop es
		ret


printGameOver:	pusha
				call printChange
				push 0xb800
				pop es

				
				
				mov di, 1500
				mov si, message
				mov cx, 12
				mov ah, 0x78

				cld
				next4:	lodsb
						stosw
						loop next4

				mov     al, 182         ; Prepare the speaker for the
        		out     43h, al         ;  note.
        		mov     ax, 9121        ; Frequency number (in decimal)
        		                        ;  for middle C.
        		out     42h, al         ; Output low byte.
        		mov     al, ah          ; Output high byte.
        		out     42h, al 
        		in      al, 61h         ; Turn on note (get value from
        		                        ;  port 61h).
        		or      al, 00000011b   ; Set bits 1 and 0.
        		out     61h, al         ; Send new value.
        		mov     bx, 25          ; Pause for duration of note.
				.pause1:
        			mov     cx, 65535
				.pause2:
        		dec     cx
        		jne     .pause2
        		dec     bx
        		jne     .pause1
        		in      al, 61h         ; Turn off note (get value from
        		                        ;  port 61h).
        		and     al, 11111100b   ; Reset bits 1 and 0.
        		out     61h, al         ; Send new value.
				
				popa
				ret
makeRandom:	add dx, 3
			jmp back1

makeRandom2:	add dx, 4
				jmp back2


printFood:	pusha

random3:		mov ah, 0
				int 1ah
				add dx, [cs:randomNumber]

regenerate:	push dx
			add dx, 4001
			mov ax, dx
			xor dx, dx
			xor bh, bh
			mov bl, [fruitx]
			add bl, 11
			dec bl
			div bx
			mov [fruitx], dx

			cmp dx, 0
			je makeRandom

			cmp dx, 1
			je makeRandom

			cmp dx, 23
			jg redCol


back1:		pop ax
			mov bl, [fruity]
			add bl, 11
			dec dl
			xor bh, bh
			xor dx, dx
			div bx
			mov [fruity], dx

			cmp dx, 78
			jg redRow

			cmp dx, 0
			je makeRandom2

			jmp back2


redCol:		mov ax, 23
			mov bx, [fruitx]
			xor dx, dx
			div bx
			mov [fruitx], dx
			jmp back1
			

redRow:		mov ax, 78
			mov bx, [fruity]
			xor dx, dx
			div bx
			mov [fruity], dx
			jmp back2


back2:		push 0xb800
			pop es

			mov al, 80
			mov bx, [fruitx]
			mul bl
			mov bx, [fruity]
			add ax, bx
			shl ax, 1
			mov di, ax

			mov ax, [es:di]	

			cmp ax, 0x042e
			je regenerate

			cmp ax, 0x0440
			je random3

			cmp ax, 0x743a
			je regenerate

			cmp al, '<'
			je regenerate

			cmp al, '3'
			je regenerate

			mov word [es:di], 0x702a
			mov word [fruitPresent], 1

			popa
		    ret


printLives:	push es
			push di
			push cx

			push 0xb800
			pop es
			mov di, 170
			mov cx, [lives]
			mov dx, 3
			sub dx, cx
			live:	mov word [es:di], 0x043c
					add di, 2
					mov word [es:di], 0x0433
					add di, 4
					loop live
			
			mov cx, dx
			cmp cx, 0
			je terminate
			dead: 	mov word [es:di], 0x073c
					add di, 2
					mov word [es:di], 0x0733
					add di, 4	
					loop dead
terminate:		pop cx
			pop di
			pop es
			ret

printBorder:
			push cx
			push es
			push di

			push 0xb800
			pop es

			mov di, 0
			mov cx, 160

			rightBorder:	mov word [es:di], 0x042e
							add di, 2
							loop rightBorder

			mov cx, 22
			downBorder:		mov word [es:di], 0x042e
							add di, 160
							loop downBorder
			mov cx, 158
			leftBorder:		mov word [es:di], 0x042e
							add di, 2
							loop leftBorder

			mov word [es:di], 0x7020
			add di, 2
			mov cx, 24
			upBorder:		mov word [es:di], 0x042e
							sub di, 160
							loop upBorder

			pop di
			pop es
			pop cx
			ret

clrscr:		
		push es
		push ax
		push di

		mov ax, 0xb800
		mov es, ax
		mov di, 0

nextchar1:      mov word[es:di], 0x7020
		add di, 2
		cmp di, 4000
                jne nextchar1

		pop di
		pop ax
		pop es
		ret

delay:
		push bp
		mov bp, sp
		pusha
		mov ax,70

		_l1:

		mov cx,[bp+4]
		_l2:

		dec cx
		loop _l2

		dec ax
		cmp ax,0
		jne _l1

		popa
		pop bp
		ret 2

kbisr:

		pusha
		in al,0x60

		cmp al,0x48	;w
		jne nextcmp
		mov word [cs:stop], 0
		mov word[cs:flag],1
		call moveup
		jmp end

		nextcmp:
				cmp al,0x4b	;a
				jne nextcmp1
				mov word[cs:flag],2
				mov word [cs:stop], 0
				call moveleft
				jmp end

		nextcmp1:
				cmp al,0x50	;s
				jne nextcmp2
				mov word[cs:flag],3
				mov word [cs:stop], 0
				call movedown
				jmp end

		nextcmp2:

				cmp al,0x4d	;d
				jne end
				mov word[cs:flag],4
				mov word [cs:stop], 0
				call moveright

		end:
				mov al,0x20
				out 0x20,al
		popa
iret


deathUp: 	dec word [lives]
			cmp word [lives], 0
			je game1

			call printLives
			mov     al, 182         ; Prepare the speaker for the
        		out     43h, al         ;  note.
        		mov     ax, 1715        ; Frequency number (in decimal)
        		                        ;  for middle C.
        		out     42h, al         ; Output low byte.
        		mov     al, ah          ; Output high byte.
        		out     42h, al 
        		in      al, 61h         ; Turn on note (get value from
        		                        ;  port 61h).
        		or      al, 00000011b   ; Set bits 1 and 0.
        		out     61h, al         ; Send new value.
        		mov     bx, 25          ; Pause for duration of note.
				.pause1:
        			mov     cx, 23000
				.pause2:
        		dec     cx
        		jne     .pause2
        		dec     bx
        		jne     .pause1
        		in      al, 61h         ; Turn off note (get value from
        		                        ;  port 61h).
        		and     al, 11111100b   ; Reset bits 1 and 0.
        		out     61h, al         ; Send new value.

			mov ax, 160
			add [location], ax
			inc word [row]
			jmp gameOver1

game1:	mov word [gameOver], 1
		jmp gameOver1

enlarge1:	mov     al, 182         ; Prepare the speaker for the
        		out     43h, al         ;  note.
        		mov     ax, 1140        ; Frequency number (in decimal)
        		                        ;  for middle C.
        		out     42h, al         ; Output low byte.
        		mov     al, ah          ; Output high byte.
        		out     42h, al 
        		in      al, 61h         ; Turn on note (get value from
        		                        ;  port 61h).
        		or      al, 00000011b   ; Set bits 1 and 0.
        		out     61h, al         ; Send new value.
        		mov     bx, 25          ; Pause for duration of note.
				.pause1:
        			mov     cx, 5000
				.pause2:
        		dec     cx
        		jne     .pause2
        		dec     bx
        		jne     .pause1
        		in      al, 61h         ; Turn off note (get value from
        		                        ;  port 61h).
        		and     al, 11111100b   ; Reset bits 1 and 0.
        		out     61h, al         ; Send new value.
			mov word [fruitPresent], 0
			add byte [count], 4
			mov al, [count]
			mov [score], al
			mov si, [length]
			add word [length], 4
			mov cx, si
			shl si, 1
			sub si, 2
			enl1:	mov di, [cs:buffer+si]
					mov [cs:buffer+si+8], di
					sub si, 2
					loop enl1
			call printScore
			jmp term1

moveup:
		pusha
		mov ax,0xb800
		mov es,ax
		mov cx,[length]
		dec cx
		mov bx, 160
		sub [location], bx
		cmp word [location], 320
		jle deathUp

		

cont1:	mov si,2
		mov di,[cs:buffer]
		mov word[es:di],0x7020
		
loop3:
		mov di,[cs:buffer+si]
		mov word[cs:buffer+si-2],di
		add si,2
		loop loop3
		sub si,2
		sub di,160
		mov word[cs:buffer+si],di
		mov ax, [es:di]
		cmp ax, 0x702a
		je enlarge1
		cmp ax, 0x0440
		je deathUp
		

term1:		call printsnake
			push word [speed]
			call delay
gameOver1:	popa
			ret


deathDown:	dec word [lives]
			cmp word [lives], 0
			je game2

			call printLives

			mov     al, 182         ; Prepare the speaker for the
        		out     43h, al         ;  note.
        		mov     ax, 1715        ; Frequency number (in decimal)
        		                        ;  for middle C.
        		out     42h, al         ; Output low byte.
        		mov     al, ah          ; Output high byte.
        		out     42h, al 
        		in      al, 61h         ; Turn on note (get value from
        		                        ;  port 61h).
        		or      al, 00000011b   ; Set bits 1 and 0.
        		out     61h, al         ; Send new value.
        		mov     bx, 25          ; Pause for duration of note.
				.pause1:
        			mov     cx, 23000
				.pause2:
        		dec     cx
        		jne     .pause2
        		dec     bx
        		jne     .pause1
        		in      al, 61h         ; Turn off note (get value from
        		                        ;  port 61h).
        		and     al, 11111100b   ; Reset bits 1 and 0.
        		out     61h, al         ; Send new value.
			mov ax, 160
			sub [location], ax
			dec word [row]
			jmp term2

game2:	mov word [gameOver], 1
		jmp gameOver1

enlarge2:	mov     al, 182         ; Prepare the speaker for the
        		out     43h, al         ;  note.
        		mov     ax, 1140        ; Frequency number (in decimal)
        		                        ;  for middle C.
        		out     42h, al         ; Output low byte.
        		mov     al, ah          ; Output high byte.
        		out     42h, al 
        		in      al, 61h         ; Turn on note (get value from
        		                        ;  port 61h).
        		or      al, 00000011b   ; Set bits 1 and 0.
        		out     61h, al         ; Send new value.
        		mov     bx, 25          ; Pause for duration of note.
				.pause1:
        			mov     cx, 5000
				.pause2:
        		dec     cx
        		jne     .pause2
        		dec     bx
        		jne     .pause1
        		in      al, 61h         ; Turn off note (get value from
        		                        ;  port 61h).
        		and     al, 11111100b   ; Reset bits 1 and 0.
        		out     61h, al         ; Send new value.

			mov word [fruitPresent], 0
			add byte [count], 4
			mov al, [count]
			mov [score], al
			mov si, [length]
			add word [length], 4
			mov cx, si
			shl si, 1
			sub si, 2
			enl2:	mov di, [cs:buffer+si]
					mov [cs:buffer+si+8], di
					sub si, 2
					loop enl2
			call printScore
			jmp term2

movedown:
		pusha
		mov ax,0xb800
		mov es,ax
		mov cx,[length]
		dec cx
		mov bx, 160
		add [location], bx
		cmp word [location], 3840
		jge deathDown
		
		mov si,2
		mov word di,[cs:buffer]
		mov word[es:di],0x7020
		
		loop4:
		mov word di,[cs:buffer+si]
		mov word[cs:buffer+si-2],di
		add si,2
		loop loop4

		sub si,2
		add di,160
		mov word[cs:buffer+si],di

		mov ax, [es:di]
		cmp ax, 0x702a
		je enlarge2
		cmp ax, 0x0440
		je deathDown


term2:		call printsnake
			push word [speed]
			call delay
gameOver2:	popa
			ret

deathLeft:	dec word [lives]
			cmp word [lives], 0
			je game3

			call printLives

			mov     al, 182         ; Prepare the speaker for the
        		out     43h, al         ;  note.
        		mov     ax, 1715        ; Frequency number (in decimal)
        		                        ;  for middle C.
        		out     42h, al         ; Output low byte.
        		mov     al, ah          ; Output high byte.
        		out     42h, al 
        		in      al, 61h         ; Turn on note (get value from
        		                        ;  port 61h).
        		or      al, 00000011b   ; Set bits 1 and 0.
        		out     61h, al         ; Send new value.
        		mov     bx, 25          ; Pause for duration of note.
				.pause1:
        			mov     cx, 23000
				.pause2:
        		dec     cx
        		jne     .pause2
        		dec     bx
        		jne     .pause1
        		in      al, 61h         ; Turn off note (get value from
        		                        ;  port 61h).
        		and     al, 11111100b   ; Reset bits 1 and 0.
        		out     61h, al         ; Send new value.
			mov ax, 2
			add word [location], ax
			inc word [col]
			jmp term3

game3:	mov word [gameOver], 1
		jmp gameOver3

enlarge3:	mov     al, 182         ; Prepare the speaker for the
        		out     43h, al         ;  note.
        		mov     ax, 1140        ; Frequency number (in decimal)
        		                        ;  for middle C.
        		out     42h, al         ; Output low byte.
        		mov     al, ah          ; Output high byte.
        		out     42h, al 
        		in      al, 61h         ; Turn on note (get value from
        		                        ;  port 61h).
        		or      al, 00000011b   ; Set bits 1 and 0.
        		out     61h, al         ; Send new value.
        		mov     bx, 25          ; Pause for duration of note.
				.pause1:
        			mov     cx, 5000
				.pause2:
        		dec     cx
        		jne     .pause2
        		dec     bx
        		jne     .pause1
        		in      al, 61h         ; Turn off note (get value from
        		                        ;  port 61h).
        		and     al, 11111100b   ; Reset bits 1 and 0.
        		out     61h, al         ; Send new value.

			mov word [fruitPresent], 0
			add byte [count], 4
			mov al, [count]
			mov [score], al
			mov si, [length]
			add word [length], 4
			mov cx, si
			shl si, 1
			sub si, 2
			enl3:	mov di, [cs:buffer+si]
					mov [cs:buffer+si+8], di
					sub si, 2
					loop enl3
			call printScore
			jmp term3

moveleft:
		pusha
		mov ax,0xb800
		mov es,ax
		mov cx,[length]
		dec cx
		
		mov si,2
		mov bx, Edge
		sub [location], si
		push si
		mov si, [row]
		shl si, 1
		mov di, [bx+si]
		pop si
		cmp [location], di 
		je deathLeft

		mov di,[cs:buffer]
		mov word[es:di],0x7020
		
		loop5:
		mov word di,[cs:buffer+si]
		mov word[cs:buffer+si-2],di
		add si,2
		loop loop5

		sub si,2
		sub di,2
		mov word[cs:buffer+si],di

		mov ax, [es:di]
		cmp ax, 0x702a
		je enlarge3
		cmp ax, 0x0440
		je deathLeft

term3:		call printsnake
			push word [speed]
			call delay
gameOver3:	popa
			ret



deathRight:	dec word [lives]
			cmp word [lives], 0
			je game4

			call printLives

			mov     al, 182         ; Prepare the speaker for the
        		out     43h, al         ;  note.
        		mov     ax, 1715        ; Frequency number (in decimal)
        		                        ;  for middle C.
        		out     42h, al         ; Output low byte.
        		mov     al, ah          ; Output high byte.
        		out     42h, al 
        		in      al, 61h         ; Turn on note (get value from
        		                        ;  port 61h).
        		or      al, 00000011b   ; Set bits 1 and 0.
        		out     61h, al         ; Send new value.
        		mov     bx, 25          ; Pause for duration of note.
				.pause1:
        			mov     cx, 23000
				.pause2:
        		dec     cx
        		jne     .pause2
        		dec     bx
        		jne     .pause1
        		in      al, 61h         ; Turn off note (get value from
        		                        ;  port 61h).
        		and     al, 11111100b   ; Reset bits 1 and 0.
        		out     61h, al         ; Send new value.
			mov ax, 2
			sub word [location], ax
			dec word [col]
			jmp term4

game4:	mov word [gameOver], 1
		jmp gameOver4

enlarge4:	mov     al, 182         ; Prepare the speaker for the
        		out     43h, al         ;  note.
        		mov     ax, 1140        ; Frequency number (in decimal)
        		                        ;  for middle C.
        		out     42h, al         ; Output low byte.
        		mov     al, ah          ; Output high byte.
        		out     42h, al 
        		in      al, 61h         ; Turn on note (get value from
        		                        ;  port 61h).
        		or      al, 00000011b   ; Set bits 1 and 0.
        		out     61h, al         ; Send new value.
        		mov     bx, 25          ; Pause for duration of note.
				.pause1:
        			mov     cx, 5000
				.pause2:
        		dec     cx
        		jne     .pause2
        		dec     bx
        		jne     .pause1
        		in      al, 61h         ; Turn off note (get value from
        		                        ;  port 61h).
        		and     al, 11111100b   ; Reset bits 1 and 0.
        		out     61h, al         ; Send new value.

			mov word [fruitPresent], 0
			add byte [count], 4
			mov al, [count]
			mov [score], al
			mov si, [length]
			add word [length], 4
			mov cx, si
			shl si, 1
			sub si, 2
			enl4:	mov di, [cs:buffer+si]
					mov [cs:buffer+si+8], di
					sub si, 2
					loop enl4
			call printScore
			jmp term4

moveright:
		pusha
		mov ax,0xb800
		mov es,ax

		mov cx,[length]
		dec cx
		mov si,2
		mov bx, Edge
		add [location], si
		push si
		mov si, [row]
		shl si, 1
		mov di, [bx+si]
		add di, 158
		pop si
		cmp [location], di 
		je deathRight
		
		mov word di,[cs:buffer]
		mov word[es:di],0x7020
		
		loop6:
		mov word di,[cs:buffer+si]
		mov word[cs:buffer+si-2],di
		add si,2
		loop loop6

		sub si,2
		add di,2
		mov word[cs:buffer+si],di

		mov ax, [es:di]
		cmp ax, 0x702a
		je enlarge4
		cmp ax, 0x0440
		je deathRight

term4:	call printsnake
		push word [speed]
		call delay
gameOver4:	popa
			ret

decRows:	dec word [row]
			jmp continue


incRows:	inc word [row]
			jmp continue

incCols:	inc word [col]
			jmp  continue

decCols:	dec word [col]
			jmp continue

printsnake:
		pusha

		mov ax, 1
		cmp word [flag], ax
		je decRows

		mov ax, 2
		cmp word [flag], ax
		je incCols

		mov ax, 3
		cmp word [flag], ax
		je incRows

		mov ax, 4
		cmp word [flag], ax
		je decCols


continue:
		mov cx, [length]
		sub cx, 1
		mov si,0
		mov ax,0xb800
		mov es,ax
		loop2:
		mov di,[cs:buffer+si]
		mov word[es:di],0x0440
		add si,2
		loop loop2

		mov di,[cs:buffer+si]
		mov word[es:di],0x743a
		add si, 2
		mov [location], di
		popa
		ret

printnum: 
    push bp
    mov bp,sp
	pusha

	mov ax, 0xb800
	mov es, ax                     ; point es to video base
	mov al, [bp + 4]                 ; load number in ax
	mov ah, 0
	mov bx, 10                     ; use base 10 for division
	mov cx, 0                      ; initialize count of digits
	mov di, [bp + 6]
nextdigit1:
	mov dx, 0                      ; zero upper half of dividend
	div bx                         ; divide by 10
	add dl, 0x30                   ; convert digit into ascii value	
	push dx                        ; save ascii value on stack
	inc cx                         ; increment count of values
	cmp ax, 0                      ; is the quotient zero
	jnz nextdigit1                 ; if no divide it again

nextpos1: 
	pop dx                         ; remove a digit from the stack
	mov dh, 0x07                   ; use normal attribute
	mov [es:di], dx                ; print char on screen
	add di, 2                      ; move to next screen location
	loop nextpos1                  ; repeat for all digits on stack
	popa
	pop bp
    ret 4


printScore: 
	pusha
	call clearScore
	mov ax, 0xb800
	mov es, ax                     ; point es to video base
	mov al, [score]                 ; load number in ax
	mov ah, 0
	mov bx, 10                     ; use base 10 for division
	mov cx, 0                      ; initialize count of digits
	mov di, 300
nextdigit:
	mov dx, 0                      ; zero upper half of dividend
	div bx                         ; divide by 10
	add dl, 0x30                   ; convert digit into ascii value	
	push dx                        ; save ascii value on stack
	inc cx                         ; increment count of values
	cmp ax, 0                      ; is the quotient zero
	jnz nextdigit                  ; if no divide it again

nextpos: 
	pop dx                         ; remove a digit from the stack
	mov dh, 0x07                   ; use normal attribute
	mov [es:di], dx                ; print char on screen
	add di, 2                      ; move to next screen location
	loop nextpos                   ; repeat for all digits on stack

	popa
	ret


initialize:
		pusha
		mov cx, [length]
		mov si, 0
		mov di, 1960
		loop1:
		mov word[cs:buffer+si],di
		add si,2
		add di,2
		loop loop1
		mov [location], di
		popa
		ret 



clearScore:
		push es
		push di
		push 0xb800
		pop es
		mov di, 140
		mov word [es:di], 0x0720
		pop di
		pop es
		ret

incSpeed:	sub word [cs: speed], 100
			mov word [cs: counterSec], 0
			jmp contTimer3

change: mov word [cs:milliseconds], 239
		jmp contTimer

exit1:	 mov al,0x20
    out 0x20,al
    popa
iret

timer:
    pusha
	cmp word [cs: stop], 1
	je exit1
    inc word [cs: ticks]
	inc word [cs: milliseconds]
	mov ax, [cs: milliseconds]
	add [cs: randomNumber], ax 

	cmp word [cs:milliseconds], 10000
	je change

contTimer:    cmp word [cs: ticks],18
    jne exit

    push 216
    call clrTimer
    
    push 224
    push word [cs:Seconds]
    call printnum

    push 216
    push word [cs:Minute]
    call printnum

	push 0xb800
	pop es
	mov di, 220
	mov ah, 0x07
	mov al, ':'
	mov word [es:di], ax

    mov word [cs: ticks],0
    
	cmp word [cs: counterSec], 20
	je incSpeed
contTimer3:  cmp word [cs: Seconds],0
    		 jne checkNext
			 mov word [cs: Seconds],60
   			 dec word [cs: Minute]


checkNext:
    cmp word [cs: Minute], -1
    jne skipAll
	cmp word [score], 240
	jl deathByTime
	jge gameWon
contTimer2:    mov word [cs: Minute],0

skipAll:

    dec word[cs: Seconds]
	inc word[cs:counterSec]

exit:
    mov al,0x20
    out 0x20,al
    popa
iret

gameWon:	pusha
			call printChange
			push 0xb800
			pop es
			
			mov di, 1500
			mov si, message2
			mov cx, 11
			mov ah, 0x78
			cld
			next41:	lodsb
					stosw
					loop next41

			mov     al, 182         ; Prepare the speaker for the
        		out     43h, al         ;  note.
        		mov     ax, 1140        ; Frequency number (in decimal)
        		                        ;  for middle C.
        		out     42h, al         ; Output low byte.
        		mov     al, ah          ; Output high byte.
        		out     42h, al 
        		in      al, 61h         ; Turn on note (get value from
        		                        ;  port 61h).
        		or      al, 00000011b   ; Set bits 1 and 0.
        		out     61h, al         ; Send new value.
        		mov     bx, 25          ; Pause for duration of note.
				.pause1:
        			mov     cx, 2031
				.pause2:
        		dec     cx
        		jne     .pause2
        		dec     bx
        		jne     .pause1
        		in      al, 61h         ; Turn off note (get value from
        		                        ;  port 61h).
        		and     al, 11111100b   ; Reset bits 1 and 0.
        		out     61h, al         ; Send new value.
			popa
			ret

deathByTime:	dec word [lives]
				call printLives
				jmp contTimer2

clrTimer:
        push bp
        mov bp, sp
		push es
		push ax
		push di
        push cx

		mov ax, 0xb800
		mov es, ax
		mov di, [bp+4]
        mov cx, 8

nextchar2:      mov word[es:di], 0x0720
		        add di, 2
                loop nextchar2
        pop cx
		pop di
		pop ax
		pop es
        pop bp
		ret 2



start:
		call clrscr
		call printBorder
		call printLives
		push 216
		call clrTimer
		push 224
    	push word [cs:Seconds]
    	call printnum

    	push 216
    	push word [cs:Minute]
    	call printnum

		push 0xb800
		pop es
		mov di, 220
		mov ah, 0x07
		mov al, ':'
		mov word [es:di], ax
		call printScore
		call initialize
		call printsnake
		xor ax,ax
		mov es,ax
		
		cli
		mov word[es:9*4],kbisr
		mov [es:9*4+2],cs
		mov word [es:8*4],timer
    	mov [es:8*4 + 2],cs
		sti

		l1:		cmp word[cs:gameOver], 1
				je quit
				cmp word[cs:fruitPresent], 1
				jne fruitGenerate
		r1:		cmp word[cs:flag],1
				jne next1
				call moveup
				jmp l1

		next1:
				cmp word[cs:flag],2
				jne next2
				call moveleft
				jmp l1

		next2:
				cmp word[cs:flag],3
				jne next3
				call movedown
				jmp l1

		next3:
				cmp word[cs:flag],4
				jne l1
				call moveright

				jmp l1

fruitGenerate:	call printFood
				jmp  r1
		
quit:	call printGameOver
		mov word [location], 0
		mov word [length], 20
		mov word [lives], 3
		mov word [gameOver], 0
		mov word [flag], 0
		mov word [row], 12
		mov word [col], 12
		mov word [qwerty], 0
		mov word [fruitPresent], 0
		mov word [count], 0
		mov word [score], 0
		mov word [ticks], 0
		mov word [Seconds], 59
		mov word [Minute], 3
		mov word [speed], 2300
		call clrscr
		call printBorder
		call printLives
		push 216
		call clrTimer
		push 224
    	push word [cs:Seconds]
    	call printnum
	
    	push 216
    	push word [cs:Minute]
    	call printnum
	
		push 0xb800
		pop es
		mov di, 220
		mov ah, 0x07
		mov al, ':'
		mov word [es:di], ax
		call printScore
		call initialize
		call printsnake
		jmp l1
		

		mov ah,4ch
		int 21h