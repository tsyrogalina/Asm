.model small

.stack 300h     


.data      

flagLastStr db 0

maxCMDSize equ 127  
cmdText              db  maxCMDSize +2 dup(0)  
 CmdFileName    db  maxCMDSize +2 dup(0)  
 cmdSize              db  ?        
                               
EPB dw ?   
dw 0     
run_address dw 0

overlay_seg dw ?   

pathAddSub db "arAddSub.bin",0 
pathMulDiv db "arMulDiv.bin",0
 

flagAddSub db 0
flagMulDiv db 0
 

stackLen db 0

                               
posInFile dw 0       
degree dw 0    

firstOfStrPos dw 0  
firstDegree dw 0

countSizeStr dw 0  
countSizeStrCopy dw 0
flagEqu db 0   


endOfStrPos dw 0  
endDegree dw 0


                               

                               

    




sourceID              dw  0
          
buffer                db    0  
bufferSize equ 1


           

spaceSymbol           equ ' '
newLineSymbol         equ 0Dh
returnSymbol          equ 0Ah
tabSymbol           equ 9
endLineSymbol       equ 0 
availableSymbols db "0123456789+-*/",0dh     
signs db "+-*/",0Dh
addSymbol equ '+'
subSymbol equ '-'
mulSymbol equ '*'
divSymbol equ '/'
equSymbol equ '='
nullSymbol equ '0'
nineSymbol equ '9'   
eofSymbol equ 0      



      
errorFormatString db "Error format string",'$' 
cannotOpenFile         db  "Cannot open source file", '$'
fileNotFound      db  "File not found", '$'
errorClosingFile    db  "Cannot close source file",'$' 
errorReadFile   db  "Error reading from source file", '$' 
badCMDArgumentsError     db  "Bad command-line arguments. ",'$'    

regSiBeg dw ?
regSiEnd dw ? 


.code

;**************************************************

putch macro symb            ;выводит символа на экран
    push ax
    push dx
    mov ah, 02h                      
    mov dl, symb
    int 21h 
    pop dx
    pop ax
endm  


emptyPage MACRO
    push ax 
    push bx                             ;
	push cx                             ;
	push dx 
    MOV AH, 7h   	  	;??????? 6. ???????? ???? ?????.
    xor al,al
    XOR CX, CX
    MOV DX, 184FH     	;?????? ?????? ???? ??????. ??????=24 (18h), ???????=79 (4fh)
    MOV BH, 7			;????? ?? ???????
    INT 10H  
    pop dx
    pop cx
    pop bx
    pop ax
ENDM

println MACRO info          ;
	push ax                 ;
	push dx                 ;
                            ;
	mov ah, 09h             ; Команда вывода строки на экран
	mov dx,offset info            ; Загрузка в dx смещения выводимого сообщения
	int 21h                 ; Вызов прервывания для выполнения вывода
   putch newLineSymbol
   putch returnsymbol
                            ;
	pop dx                  ;
	pop ax                  ;
ENDM 
         
setPosInFileTo MACRO symbolsInt, symbols;
	push ax                     ;
	push bx                     ;
	push cx                     ;
	push dx                     ; ????????? ???????? ?????????
    mov bx, sourceID                        ;
	mov ah, 42h                 ; ?????????? ? ah ??? 42h - ?-??? DOS ????????? ????????? ?????
	xor al ,al 			        ; ???????? al, ?.?. al=0 - ??? ??????????? ????????? ? ?????? ?????
	mov cx, symbolsInt          ; ???????? cx, 
	mov dx, symbols			    ; ???????? dx, ?.? ????????? ????????? ?? 0 ???????? ?? ?????? ????? (cx*2^16)+dx 
	int 21h                     ; ???????? ?????????? DOS ??? ?????????? ????????   
                                ;
	pop dx                      ; ??????????????? ???????? ????????? ? ??????? ?? ?????????
	pop cx                      ;
	pop bx                      ;
	pop ax                      ;
ENDM 
SETCURSOR MACRO R, C	;????????? ??????? ? ???????? ?????????  
    push ax
    push bx
    push dx
    MOV AH, 2
    XOR BH, BH			;???????? ????? ?????????????
    MOV DH, R			;????? ??????
    MOV DL, C			;????? ???????
    INT 10H           
    pop dx
    pop bx
    pop ax
ENDM        






openFiles proc
push cx
push dx
mov ah, 3Dh			        ; Функция 3Dh - открыть существующий файл
	mov al, 02h			        ; Режим открытия файла
	lea dx, CmdFileName         ; Загружаем в dx название исходного файла
	mov cx, 00h			        ; маска атрибутов файла
	int 21h                     ;
                                ;
	jb badOpenFile	        ; Если файл не открылся, то прыгаем в badOpenSource
                                ;
	mov sourceID, ax	        ; Загружаем в sourceId значение из ax, полученное при открытии файла
                                ;
	mov ax, 0			        ; Загружаем в ax 0, т.е. ошибок во время выполнения процедуры не произшло 
	jmp endOpenProc		        ; Прыгаем в endOpenProc и корректно выходим из процедуры
                                ;
badOpenFile:                  ;
	println cannotOpenFile        ; Выводим соответсвующее сообщение
	cmp ax, 02h                 ; Сравниваем ax с 02h
	jne errorFound              ; Если ax != 02h - файл найден, прыгаем в errorFound
                                ;
	println fileNotFound    ; Выводим сообщение о том, что файл не найден 
                                ;
	
                                ;       ;       ==//==                               ;
errorFound:                     ;
	mov ax, 1                   ; Загружаем в ax 1, т.е. произошла ошибка
endOpenProc:                    ;
	pop dx                      ;
	pop cx                      ; Восстанавливаем значения регистров и выходим из процедуры
	ret                         ;
ENDP                            ;

;**************************************************************************************************************************
closeFiles PROC                 ;
	push bx                     ;
	push cx                     ; Сохраняем значения регистров 
                                ;
	xor cx, cx                  ; Обнуляем cx
                                ;
	mov ah, 3Eh                 ; Загружаем в ah код 3Eh - код закрытия файла
	mov bx, sourceID            ; В bx загружаем ID файла, подлежащего закрытию
	int 21h                     ; Выпоняем прерывание для выполнения 
                                ;
	jnb goodCloseOfSource		; Если ошибок при закрытии не произошло, прыгаем в goodCloseOfSource
                                ;
	println errorClosingFile  ; Иначе выводим соответсвующее сообщение об ошибке       
	                            ;
	inc cx 			            ; now it is a counter of errors
                                ;
goodCloseOfSource:              ;               ;
	mov ax, cx 		            ; Записываем в ax значение из cx, если ошибок не произошло, то это будет 0, иначе 1 или 2, в зависимости от
                                ; количества незакрывшихся файлов
	pop cx                      ;
	pop bx                      ; Восстанавливаем значения регистров и выходим из процедуры
	ret                         ;
ENDP                            ;
;*************************************************************************        

;reads to buffer maxWordSize symbols
;?????????: ? ax ?????????? ????-?? ????????? ?? ????? ????????
readFromFile PROC                   ;
	push bx                         ;
	push cx                         ;
	push dx                         ; ????????? ???????? ?????????
    setPosInFileTo degree,posInFile                              ;
	mov ah, 3Fh                     ; ????????? ? ah ??? 3Fh - ??? ?-??? ?????? ?? ?????
	mov bx, sourceID                ; ? bx ????????? ID ?????, ?? ???????? ?????????? ?????????
	mov cx,  bufferSize          ; ? cx ????????? ???????????? ?????? ?????, ?.?. ????????? ???????????? ???-?? ???????? (maxWordSize ????????)
	lea dx, buffer                  ; ? dx ????????? ???????? ???????, ? ??????? ????? ????????? ?????? ?? ?????
	int 21h                         ; ???????? ?????????? ??? ?????????? ?-???
    cmp ax,0
	je endRead1                                ;
	jnb goodRead					; ???? ?????? ?? ????? ??????? ?? ????????? - ??????? ? goodRead 
	
                                    ;
	println errorReadFile     ; ????? ??????? ????????? ?? ?????? ?????? ?? ?????
	mov ax, 0                       ; ?????????? ? ax 0
     jmp endread2                               ;
goodRead:
    lea si,buffer
    mov bl,[si]
    cmp bl,eofSymbol
    je endRead1
    clc ;CF=0  
    add posinfile,ax 
    adc degree,0 
    jmp endRead2    
endRead1:
mov ax,0  
    mov flagLastStr,1
    jmp endRead2

   
                         ;   
        
endRead2:                       ;
	pop dx                          ; ??????????????? ???????? ?????????
	pop cx                          ;
	pop bx                          ;
	ret                             ;
ENDP   


getLenStrInfile proc 
    
  mov ax,firstOfStrPos
	mov posInFile,ax
	
	mov ax,firstDegree
	mov degree,ax
	
	mov countSizeStr,0  
loop_strlen:
	call readFromFile  
	cmp ax,0
	je  end_strlen  
	
	lea si,buffer
    mov al ,[si]
    cmp al ,newLineSymbol
    je end_strlen
    add countSizeStr,1
    jmp loop_strlen        
    
    
    end_strlen:
    
    
ret
endp

checkStringSymbols proc
	mov ax,firstOfStrPos
	mov posInFile,ax
	
	mov ax,firstDegree
	mov degree,ax
	
	
	mov flagEqu,0
loop_check_symbols:
	 call readFromFile
	 cmp ax,0
	 je check_end
	
	 mov di,offset buffer   
	 mov al,[di]
	 lea si,availableSymbols
loop_check_available:
	 mov ah,[si]
	 cmp ah,newLineSymbol
	 je next_check_symbols
	 cmp ah,al
	 je loop_check_symbols
	 inc si
	 jmp loop_check_available
	 
next_check_symbols:	 
	cmp al,equSymbol
	je up_flag_equ

next_check_symbols2:
	cmp al,newLineSymbol
	je check_up_flag
	jmp errorSymbols
	
	
up_flag_equ:
	mov ah,flagEqu
	cmp ah,1
	je errorSymbols
	mov flagEqu,1
	jmp loop_check_symbols
check_up_flag:
	mov ah,flagEqu
	cmp ah,1
	je goodSymbols
	jmp errorSymbols

check_end:
	mov al,flagEqu
	cmp al,1
	je goodSymbols1
	jmp errorSymbols 
goodSymbols1:

goodSymbols:
mov ax,0    

	jmp end_check_symbols
	errorSymbols:
	mov ax,1
end_check_symbols:
ret
endp     


checkFirstSymbolStr proc
    mov ax,firstOfStrPos
	mov posInFile,ax
	
	mov ax,firstDegree
	mov degree,ax
	
	call readFromFile
	cmp ax,0
	je errorFirstSymbol
	lea si,buffer
	mov al,[si]
	mov ah,addSymbol
	cmp al ,ah
	je errorFirstSymbol
	mov ah,mulSymbol
	cmp al ,ah
	je errorFirstSymbol
	mov ah,divSymbol
	cmp al ,ah
	je errorFirstSymbol   
	cmp al,nullSymbol
	je checkSecondSymbol 
	jmp goodFirstSymbol
	
	checkSecondSymbol:
	call readFromFile
	cmp ax,0
	je errorFirstSymbol
	lea si,buffer
	mov al,[si] 
	cmp al,equSymbol
	je goodFirstSymbol
	call isSign 
	cmp ax,0
	je goodFirstSymbol  
	jmp errorFirstSymbol 
	
	
	
	
	
	goodFirstSymbol: 
	mov ax,0
	jmp endCheckFirstSymbol
	errorFirstSymbol:  
	mov ax,1  
	endCheckFirstSymbol: 
	
    ;mov ax,firstOfStrPos
	;mov posInFile,ax
	
	;mov ax,firstDegree
	;mov degree,ax
    ret
endp     


checkLastSymbolStr proc  
    mov ax,firstOfStrPos
    mov bx,firstDegree 
    
    clc
    add ax, countSizeStr  
    adc bx,0
    
    clc
    sub ax,2 
    sbb bx,0
    
	mov posInFile,ax
	mov degree,bx 
	
	call readFromFile
	cmp ax,0 
	je errorLastSymbol
	lea si,buffer
	mov al,[si]
	mov ah,'+'
	cmp al ,ah
	je  errorLastSymbol
	mov ah,'-'
	cmp al ,ah
	je  errorLastSymbol
	mov ah,'*'
	cmp al ,ah
	je  errorLastSymbol
	mov ah,'/'
	cmp al ,ah
	je  errorLastSymbol
 goodLastSymbol:  
 mov ax,0
	jmp  endCheckLastSymbol 
       
    
 errorLastSymbol:  
 mov ax,1
 
 endCheckLastSymbol:
 ret   
endp     


;symbol in al
isSign proc
   push di
   push cx
   lea di,signs 
   mov cx,4
   loop_signs2:
	 mov ah,[di]
	 
	 cmp ah,al
	 je itIsSign
	 inc di
	 loop loop_signs2  
     jmp itIsNotSign
    itIsSign:  
    mov ax,0
    jmp endIsSign
    itIsNotSign:
    mov ax,1
    endIsSign:
    
    pop cx
   pop di 
   ret
endp 




checkDoubleSign proc
    mov ax,firstOfStrPos
	mov posInFile,ax
	
	mov ax,firstDegree
	mov degree,ax    
	
	mov cx, countSizeStr
	sub cx,2
loop_check_double_sign_1: 
    call readFromFile 
    lea si,buffer
    mov dl,[si]
    call readFromFile    
    lea si,buffer
    mov dh,[si]  
    
    mov ax,posInFile
    mov bx,Degree
    
    clc
    sub ax,1 
    sbb bx,0 
    mov posInFile,ax
    mov degree,bx 
     
    mov al ,dl
    call isSign
    cmp ax,1
    je   endLoopCheckDoubleSign  
    mov al,dh
    call isSign
    cmp ax,1
     je   endLoopCheckDoubleSign
     
     jmp errorDoubleSign

     endLoopCheckDoubleSign:
     
loop loop_check_double_sign_1     

mov ax,0 
jmp endCheckDoubleSign


errorDoubleSign:  

mov ax,1
    
endCheckDoubleSign:    
    
ret
endp      





checkDiv0 proc  
     mov ax,firstOfStrPos
	mov posInFile,ax
	
	mov ax,firstDegree
	mov degree,ax    
	
	mov cx, countSizeStr
	sub cx,2
loop_check_div_0: 
    call readFromFile 
    lea si,buffer
    mov dl,[si]  
    cmp dl,divSymbol
    jne end_loop_check_div_0
     call readFromFile 
    lea si,buffer
    mov dl,[si] 
     call readFromFile 
    lea si,buffer
    mov dh,[si] 
    mov ax,posInFile 
    mov bx,degree
    
    clc
    sub ax,2
    sbb bx,0 
    mov posInFile,ax
    mov degree,bx  
    cmp dl,nullSymbol
    jne end_loop_check_div_0
    
    mov al ,dh
    call isSign 
    cmp ax,0
    je  badDiv
    cmp dh,equSymbol
    je badDiv
      
    
    end_loop_check_div_0:
    
    loop   loop_check_div_0     
    
    
    goodDiv:  
    mov ax,0
    jmp end_check_div
    
    badDiv:  
    mov ax,1
    end_check_div:
    
    ret
    
endp
       


checkNull proc  
     mov ax,firstOfStrPos
	mov posInFile,ax
	
	mov ax,firstDegree
	mov degree,ax    
	
	mov cx, countSizeStr
	sub cx,2
loop_check_0: 
    call readFromFile 
    lea si,buffer
    mov al,[si]   
    call isSign
    cmp ax,0
    jne end_loop_check_0
    
     call readFromFile 
    lea si,buffer
    mov dl,[si] 
     call readFromFile 
    lea si,buffer
    mov dh,[si] 
    mov ax,posInFile 
    mov bx,degree
    
    clc
    sub ax,2
    sbb bx,0 
    mov posInFile,ax
    mov degree,bx  
    cmp dl,nullSymbol
    jne end_loop_check_0
     
     cmp dh,equSymbol
     je  end_loop_check_0
    mov al ,dh
    call isSign 
    cmp ax,0 
    
    
    je end_loop_check_0
    jmp bad0
    
   
      
    
    end_loop_check_0:
    
    loop   loop_check_0     
    
    
    good0:  
    mov ax,0
    jmp end_check_0
    
    bad0:  
    mov ax,1
    end_check_0:
    
    ret
    
endp
       


checkStrInFile proc  
    call getLenStrInfile
    
    call checkStringSymbols
	cmp ax,1
	je errorString   
	
	call checkFirstSymbolStr
	cmp ax,1
	je errorString    
	
	call checkLastSymbolStr
	cmp ax,1
	je errorString 
	    
	call checkDoubleSign 
	cmp ax,1
	je errorString  
	
	call checkDiv0
	cmp ax,1
	je errorString 
	
	call checkNull  
	cmp ax,1
	je errorString
	
	
	goodString:
	mov ax,0
     jmp end_check_string
    errorString: 
    mov ax,1 
   println errorFormatString    
   end_check_string:
	
ret
endp  

clearSignFlag macro 
    mov dx,0 
    mov flagAddSub,dl
    mov flagMulDiv,dl
    
     
  
  
  endm

polishNotation proc 
    mov ax,firstOfStrPos
	mov posInFile,ax
	
	mov ax,firstDegree
	mov degree,ax  
    mov cx,   countSizeStr
    sub cx,1
    mov countSizeStrCopy,cx  
    loop_for_polish_notation:
     call   readFromFile
     lea si,buffer
     mov al,[si]
    
     call isSign 
     cmp ax,0
     je  preparingSigns 
      lea si,buffer
     mov al,[si]
     putch al
     
     
    jmp end_loop_for_polish_notation
     
     
     preparingSigns:  
     mov al ,spaceSymbol
     putch al
      
      lea si,buffer
     mov al,[si] 
     mov ah,stackLen
      cmp ah,0
      je addToStack
     cmp al,addSymbol
     je addSubLabel
     cmp al,subSymbol
     je addSubLabel
     cmp al,mulSymbol
     je mulDivLabel
     cmp al,divSymbol
     je mulDivLabel
     
  
     
     
addSubLabel: 
     mov bl, flagAddSub
     cmp bl ,1
     je callOverlay
     clearSignFlag
    preparingOverlay pathAddSub,flagAddSub 
   mov ax,2
      jmp callOverlay
     

     

     
     
     
mulDivLabel: 

     mov bl, flagMulDiv
     cmp bl ,1
     je callOverlay
     clearSignFlag
    preparingOverlay pathMulDiv ,flagMulDiv 
    mov ax,3
     jmp callOverlay
     
     


     
     
   


callOverlay:  
     
      
    call dword ptr run_address  
    
     jmp verificationAction

    
 verificationAction: 
    
    
  cmp ax,2
  je emptyStack  
  
  
  cmp ax,3
  je mulDivDeleteInStack    
  
  
  mulDivDeleteInStack:
 
  loop_div_mul_delete: 
  mov al,stackLen
  cmp al,0
  je addTostack
  pop ax
  cmp al,addSymbol
  je end_loop_div_mul_delete 
   cmp al,subSymbol
  je end_loop_div_mul_delete 
  putch al
  dec stackLen
  jmp loop_div_mul_delete
  
  
  end_loop_div_mul_delete:
   push ax 
   jmp addToStack
  
  
  
  
  
  emptyStack:
  mov cl,stackLen
  xor ch,ch  
  mov stackLen,0
  loop_empty_stack:
  pop ax
  putch al
  loop loop_empty_stack 
  jmp addToStack
  
  
    
    
    
    addToStack:
     lea si,buffer
     mov al,[si]   
     xor ah,ah
     push ax
     add stackLen ,1
     
     
     
     end_loop_for_polish_notation:   
     mov cx, countSizeStrCopy
     dec cx   
     mov    countSizeStrCopy,cx
     cmp cx, 0
     jne   loop_for_polish_notation
     
     
     
    
                                   
                                   
      mov cl,stackLen
      xor ch,ch
      cmp cl,0
      ja   loop_empty_stack_2
      jmp end_polish_not
      
      loop_empty_stack_2:   
      pop ax
      putch al
      
      
      loop  loop_empty_stack_2 
      
      
    
    
    end_polish_not:
    putch newLineSymbol
    putch returnSymbol
    ret
endp
          
          
workingFile proc  
loop_check: 
   mov al, flagLastStr     
   cmp al,1
   je endWork
    call checkStrInFile   
    cmp ax,1
    je to_next_string   
     call polishNotation     
    
    
    
    jmp to_next_string
to_next_String:
mov stackLen,0
clearSignFlag
     mov ax,firstOfStrPos
    mov bx,firstDegree 
    
    clc
    add ax, countSizeStr  
    adc bx,0
    
    clc
    add ax,2 
    adc bx,0
    
    
	mov firstOfStrPos,ax
	mov firstDegree,bx 
	mov ax,0
	 mov ax,firstOfStrPos
	 
	mov countSizeStr,0
	mov flagEqu,0
	
	jmp loop_check
    
  
  
  endWork:  
ret
endp      


preparingOverlay MACRO path,flag   
    
	mov ax, es;PSP     
    mov bx, zseg ;zero segment
    sub bx, ax;new prog size
    mov ah, 4Ah
    int 21h
    ;jc error_download       
     
    ;allocate memory for overlay
    mov bx, 50h  
    mov ah, 48h        
    int 21h
    ;jc error_download   
    ;ax-segment address of allocated block  
    
    ;prepare for executing program 
    mov EPB, ax;EPB(bx)=segment address for overlay load  
    mov EPB+2, ax;for using in commands 
    mov overlay_seg, ax;save overlay segment 
    mov ax, ds;
    mov es, ax;ES:BX=EPB 
    
    mov dx, offset path 
        
    mov bx, offset EPB 
    mov ax, 4B03h 
    int 21h    
    mov flag,1
    
endm 

cmd_parse proc
    
    cld   
    
    mov ax,@data
    mov es,ax

    
    xor cx,cx
    mov cl,ds:[80h]
    mov cmdSize,cl
    cmp cl,0
    je error_cmd
    mov si,82h
    lea di,cmdText
    rep movsb           ;get cmd line 
    
    ;print_str cmd_line

    xor cx,cx
    xor ax,ax
    mov cl,cmdsize
    
    mov ax,@data
    mov es,ax
    mov ds,ax
    lea si,cmdText
    
    lea di,cmdFileName
    call get_cmd_word
    
    
    
    ret
error_cmd: 
    mov ax,@data
    mov es,ax
    mov ds,ax
    println badCMDArgumentsError
    jmp endMain     
            
    
cmd_parse endp

;GETTING WORD FROM CMD LINE
get_cmd_word proc
    push ax
    push cx
    push di 
    xor ax,ax
loop_getting:
    mov al,[si]
    

    cmp al,0Dh
    je end_getting

    
    mov [di],al
    
    inc si
    inc di
    
    loop loop_getting
end_getting:
    mov byte ptr [di],0
    inc si
            
    pop di
    pop cx
    pop ax
    ret
get_cmd_word endp

main:
	call cmd_parse
    mov	cx,@data                      
    mov	ds,cx 
    
    
       
	
	call openFiles          ; Вызываем процедуру, которая открывает оба файла для чтения/записи
	cmp ax, 0               ;
	jne endMain2				;  ==//==
	 
	 
	
	 call workingFile
	
	
	
	endMain2:			;  ==//==
                            ;
	call closeFiles         ; Завершив обработку информации, вызываем процедуру закрытия файлов
	cmp ax, 0               ;
	jne endMain				;  ==//==
	
	endMain:                    ;
	       ; Выводим сообщение о завершении работы программы
                            ;
	mov ah, 4Ch             ; Загружаем в ah код команды завершения работы
	int 21h                 ; Вызов прерывания DOS для ее исполнения
                            ;                                                                              
                            
                            
                            
   zseg segment
zseg ends 
end main

 
