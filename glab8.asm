.model tiny
.code 
.386
org 100h

start:
	jmp main 
	screen_buffer dw 2000 dup(0001011100100000b)
	screen_size equ 2000
	cmd_buf db 126 dup(0)
	bad_cmd_arg db 0dh,0ah,"Bad command line arguments",0dh,0ah
	empty_cmd db 0dh,0ah,"Empty cmd",0dh,0ah
	too_many_cmd_args db 0dh,0ah,"Too many cmd arguments",0dh,0ah
	wrong_number_cmd db 0dh,0ah,"Wrong time in cmd",0dh,0ah
	time dw 0
	counter  dw  0
	old_Timer_Offset dw ?
	old_timer_segment dw ?         
	old_Keyboard_Offset dw ?
	old_Keyboard_Segment dw ?
	counter_ticks dw 0
	symbol dw (1000010000000011b)  ;red heart


new_Keyboard_Interrupt proc far
    cli
    pushf
	pusha
    
    push ds
    push dx
    
    push cs
    pop ds   ;ds = cs
	
	
    
    mov ax, time
    cmp counter, ax
    jb end_Keyboard_interrupt
    push es

    mov ax, 0B800h
	mov es,ax
	
	mov di, 0000h
	mov cx, screen_size 
	lea si, screen_buffer 
	rep movsw
	
	
	pop es    
	
end_Keyboard_interrupt:
    mov counter, 0
	
    pop dx
    pop ds
    popa
    popf
    sti
    jmp dword ptr cs:[old_Keyboard_Offset]  
endp


new_Timer_Interrupt proc far 
    cli          ;
    pushf
	pusha
	push ds
	push dx
	
	push cs
	pop ds ;ds = cs
	
	inc counter
	mov ax,time
	cmp counter,ax
	jne end_timer_interrupt
save_screen:
	push es
	mov ax,0b800h
	mov es,ax
	mov si,0
	mov di,0
	mov cx,screen_size
save_screen_loop:
	mov ax,es:[di]
	mov [screen_buffer+si],ax
	add di,2
	add si,2
	loop save_screen_loop
	
	mov di,0
	
	mov cx,screen_size
output_screen:
	mov si,offset symbol
	movsw
	loop output_screen
	
	pop es
	
end_timer_interrupt:
	
	pop dx
	pop ds
	popa
	popf
	sti
	jmp dword ptr cs:[old_timer_offset]
endp

main:
	call getCMDString
	call checkCmdArg
	call atoi
	call calc_tics
	
	mov ax, 3508h      
    int 21h
    mov old_Timer_Offset, bx
    mov old_timer_segment, es
	
	mov ax, 2508h
    mov dx, offset new_Timer_Interrupt
    int 21h
	
	mov ax, 3509h
    int 21h
    mov old_Keyboard_Offset, bx
    mov old_Keyboard_Segment, es
    
    mov ax, 2509h
    mov dx, offset new_Keyboard_Interrupt
    int 21h
	
	mov dx, offset main
    int 27h 
	
	
	
	
	exit proc  
    mov ax, 4C00h
    int 21h 
    ret
endp

print macro string  
    lea dx, string
    mov ah, 09h
    int 21h
endm 


getCMDString proc  
    xor cx, cx
    mov cl, es:[80h] 
    cmp cx, 0 
    je emptyCommandLineError
    mov di, 82h       ;start cmd
    lea si, cmd_buf     
    getSymbols:
    mov al, es:[di]    
    cmp al, 0Dh       ;compare with end  
    je endGet  
    cmp al, ' '      ;compare with next arg
    je TooManyArgs
    mov [si], al       
    inc di            
    inc si            
    jmp getSymbols 
    endGet:         
    ret
    emptyCommandLineError:
	print bad_cmd_arg
    print empty_cmd
    call exit 
    TooManyArgs:
	print bad_cmd_arg
    print too_many_cmd_args 
    call exit
endp 


checkCmdArg proc 
	xor cx,cx
    lea si, cmd_buf 
    cmp byte ptr [si], '0'
    je checkFailed
    startOfCheck:
    cmp byte ptr [si], 0
    je endOfCheck
	inc cx
    cmp byte ptr [si], '0'
    jb checkFailed  
    cmp byte ptr [si], '9'
    ja checkFailed 
    jmp checkPassed
checkFailed:
	print bad_cmd_arg
    print wrong_number_cmd
    call exit  
checkPassed:

    inc si
    jmp startOfCheck  
endOfCheck: 
	cmp cx,4
	jbe good_check
	print bad_cmd_arg
    print wrong_number_cmd
	call exit
good_check:
	
    ret
endp 


atoi proc
	xor ax,ax
	xor dx,dx
	mov bx,10
	lea si,cmd_buf

loop_atoi:
	cmp byte ptr [si], 0
	je  end_convert
	mul bx 
	xor cx,cx
	mov cl,[si]
	sub cl,'0'
	add ax,cx
	inc si
	jmp loop_atoi
end_convert:
	cmp ax,3600
	jbe good_convert
	print bad_cmd_arg
    print wrong_number_cmd
	
good_convert:
	
	mov time,ax

ret
endp

calc_tics proc
	mov bx,5
	xor dx,dx
	mov ax,time
	div bx
	mov bx,ax
	
	xor dx,dx
	mov ax,time
	mov cx,18
	mul cx
	add ax,bx
	mov time, ax
	

ret 
endp
	end start
	