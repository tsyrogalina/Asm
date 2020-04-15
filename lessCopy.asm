.model small

.stack 100h

.data    

posInFile dw 0       
degree dw 0

maxNumOfString equ 23   ;максимальное количество строк ,которое может быть выведено на экране


maxCMDSize equ 127
cmd_size              db  ?
cmd_text              db  maxCMDSize + 2 dup(0)
sourcePath            db  maxCMDSize + 2 dup(0) 
;sourcePath            db  "text.", 0
       
                      
sourceID              dw  0
                      
maxWordSize           equ 16
buffer                db  maxWordSize  dup(0)
count db 0      
firstStrPtr dw 0    ;указатель на первую строку экрана (0,1,2...)
 
userIn db ?   ;записыывается пользовательский ввод
                  
countStr db 0                      ;счетчик строк (то есть сколько строк на экране выведено)
                            
spaceSymbol           equ ' '
newLineSymbol         equ 0Dh
returnSymbol          equ 0Ah
tabulation            equ 9
endl                  equ 0
                      

badCMDArgsMessage     db  "Bad command-line arguments. Only 1 argument required: source path",'$'
badSourceText         db  "Cannot open source file", '$'
fileNotFoundText      db  "File not found", '$'
errorClosingSource    db  "Cannot close source file",'$' 
errorReadSourceText   db  "Error reading from source file", '$'



.code  
.386
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

println MACRO info          ;
	push ax                 ;
	push dx                 ;
                            ;
	mov ah, 09h             ; Команда вывода строки на экран
	lea dx, info            ; Загрузка в dx смещения выводимого сообщения
	int 21h                 ; Вызов прервывания для выполнения вывода
                            ;
	mov dl, 0Ah             ; Символ перехода на новую строку
	mov ah, 02h             ; Команда вывода символа
	int 21h                 ; Вызов прерывания
                            ;
	mov dl, 0Dh             ; Символ перехода в начало строки   
	mov ah, 02h             ;
	int 21h                 ;            ==//==
                            ;
	pop dx                  ;
	pop ax                  ;
ENDM

main:
	mov ax, @data           ; Загружаем данные
	mov es, ax              ;
    
	

	xor ch, ch              ; Обнуляем ch
	mov cl, ds:[80h]		; Смещеие для дальнейшей работы с командой строкой
	mov cmd_size, cl 		; В cmd_size загружаем длину командной строки
	mov si, 81h             ;
	lea di, cmd_text        ; Загружаем в di смещение текста переданного через командную строку
	rep movsb               ; Записать в ячейку адресом ES:DI байт из ячейки DS:SI
                            ;
	mov ds, ax              ; Загружаем в ds данные
                            ;
	
                            ;
	call parseCMD           ; Вызов процедуры парсинга командной строки
	cmp ax, 0               ;
	jne endMain				; Если ax != 0, т.е. при выполении процедуры произошла ошибка - переходим к конце программы, т.е. прыгаем в endMain
                            ;
	call openFiles          ; Вызываем процедуру, которая открывает оба файла для чтения/записи
	cmp ax, 0               ;
	jne endMain2				;  ==//==
                            ;
	call displayPage     ; Главная процедура, в которой содержится весь алгоритм обработки файла
	cmp ax, 0               ; 
	je endMain2		
	cmp ax,2 
	je endMain2
	
userInput:
mov degree,0
mov posinfile,0
    MOV AH, 0
    INT 16H
    MOV USERIN, AH 
    
    CMP USERIN, 48H        ;up
    jne next1    
    
up: 
    cmp firstStrPtr,0
    je userInput   
    
    sub firstStrPtr ,1
   
    ;cmp firstStrPtr,4096  ;2^16/16      
    mov ax,firstStrPtr
      
   
    xor dx,dx
    mov bx,4096
    div bx
    cmp ax,0 
    
    
   
    
    je nextup 
    add degree,ax   
    mov ax,dx
    jmp nextUp2
    

nextUp: 
     mov ax, firstStrPtr  
nextUp2:     
    xor dx,dx   
    mov bx,maxWordSize 
    mul bx
    mov posInFile,ax  
    ;sub posInFile ,1
    
    call displayPage
     jmp userInput
    
next1: 
    CMP USERIN, 50H         ;DOWN
	JNE NEXT2   
    
down:
   
     
     add firstStrPtr ,1 
    
     
     mov ax,firstStrPtr  
    
  xor dx,dx
    mov bx,4096
    div bx
    cmp ax,0 
    
    
   
    
    
   
    je nextDown
    add degree,ax  
     mov ax,dx
    jmp nextDown2

nextDown: 
 mov ax, firstStrPtr
 nextDown2: 
    xor dx,dx   
    mov bx,maxWordSize
    mul bx
    mov posInFile,ax 
   ; sub posInfile,1
    call displayPage  
    jmp userInput
	
next2:
    CMP USERIN, 49H         ;pageUp
	JNE NEXT3
pageUp:
    cmp firstStrPtr,0
    je userInput	 
    cmp firstStrPtr, maxNumOfString 
    jb toFirstPage
    sub firstStrPtr,maxNumOfString   
    mov ax,firstStrPtr
      
   
    xor dx,dx
    mov bx,4096
    div bx
    cmp ax,0 
    
    
   
    
    je nextpageup 
    add degree,ax   
    mov ax,dx
    jmp nextPageUp2
    

nextpageUp: 
     mov ax, firstStrPtr  
nextPageUp2:     
    xor dx,dx   
    mov bx,maxWordSize 
    mul bx
    mov posInFile,ax  
nextPageUP3:    
    
    call displayPage
    
     jmp userInput
toFirstPage: 
    mov firstStrPtr,0
    mov degree,0
    mov posInFile,0  
    jmp nextPageUP3
       
	
next3:	 
    CMP USERIN, 51H         ;pageDown
	JNE NEXT4     
PageDown:  
    add firstStrPtr,maxNumOfString 
      mov ax,firstStrPtr  
    
  xor dx,dx
    mov bx,4096
    div bx
    cmp ax,0 
    
    
   
    
    
   
    je nextPageDown
    add degree,ax  
     mov ax,dx
    jmp nextPageDown2

nextPageDown: 
 mov ax, firstStrPtr
 nextPageDown2: 
    xor dx,dx   
    mov bx,maxWordSize 
    mul bx
    mov posInFile,ax 
  
    call displayPage  
    jmp userInput
    	
	
next4:  
cmp userIn,10H
je endMain2   
    jmp userInput
   
    
endMain2:			;  ==//==
                            ;
	call closeFiles         ; Завершив обработку информации, вызываем процедуру закрытия файлов
	cmp ax, 0               ;
	jne endMain				;  ==//==
                            ;
endMain:                    ;
	       ; Выводим сообщение о завершении работы программы
                            ;
	mov ah, 4Ch             ; Загружаем в ah код команды завершения работы
	int 21h                 ; Вызов прерывания DOS для ее исполнения


cmpWordLenWith0 MACRO textline, is0Marker       ; Сравнение строки с нулем, 1 параметр - строка, 2 условие перехода при 0
	push si                                     ; Сохранение значение регистра si
	                                            ;
	lea si, textline                            ; Загружаем в si смещение строки, в которой измеряем длинну
	call    strlen                              ; Вызываем ф-цию нахождения длины строки textline, результат -> ax
	                                            ;
	pop si                                      ; Восстанавливаем значение si
	cmp ax, 0                                   ; Сравниваем найденную длинну с нулем
	je is0Marker                                ; Если длина равна нулю -> переходим по переданной метке is0Marker
ENDM                                            ;                               ;
                                                ; 
                                                                                        ;                               ;
                                                        
                                                ;
;**************************************************************************************************************************
parseCMD PROC                                   ;
	push bx                                     ;
	push cx                                     ;
	push dx                                     ; Сохраняем
                                                ;
	mov cl, cmd_size                            ; Загружаем в cl размер командой строки
	xor ch, ch                                  ; Обнуляем ch
                                                ;
	lea si, cmd_text                            ; В si загружаем смещение данных из командой строки
	                                            ;                                                          
	lea di, buffer                              ;(вызываемый экзешник) В di загружаем смещение буффера для обработки данных      
	call rewriteAsWord                          ;                                                           
                                                ;                                                           
	lea di, sourcePath                          ;(файл) Загружаем в di смещение sourcePath т.е. источника текста для обработки
	call rewriteAsWord                          ;
                                                ;
	cmpWordLenWith0 sourcePath, badCMDArgs      ; Если строка, содержащая название исзодног файла пуста, прыгаем в badCMDArgs  
	
                                                ; 
	lea di, buffer                              ; 
	call rewriteAsWord                          ;
                                                ;
	cmpWordLenWith0 buffer, argsIsGood          ; Если больше данных нет, т.е. кроме названия выходного и выходного файлов командная строка ничего не содержит
                                                ; то вызов программы корректный - прыгаем в argsIsGood
badCMDArgs:                                     ;
	println badCMDArgsMessage                   ; Выводим сообщение о неверных параметрах командной строки
	mov ax, 1                                   ; Записываем в ax код ошибки
                                                ;
	jmp endproc                                 ; Прыгаем в endproc и завершаем процедуру
                                                ;
argsIsGood:                                     ;
	mov ax, 0                                   ; Записываем в ax код успешного завершения процедуры
endproc:                                        ;
	pop dx                                      ;
	pop cx                                      ;
	pop bx                                      ; Восстанавливаем значения регистров и выходим из процедуры
	ret	                                        ;
ENDP
;*************************************************************************************************************************  
  
;*************************************************************************************************************************
;cx - Длина командной строки
;Результат - переписывает параметр из коммандной строки 
rewriteAsWord PROC              ;
	push ax                     ;
	push cx                     ;
	push di                     ; Сохраняеи регистры
	push bx
	 mov bx,0
		;
loopParseWord:                  ;
	mov al, ds:[si]             ; Загружаем в al текущий символ
	
	cmp al, spaceSymbol         ; -------------------
    je isStoppedSymbol          ;                                 ;
	cmp al, newLineSymbol       ;
	je isStoppedSymbol          ;         Если этот символ равен            ;
	cmp al, tabulation          ;   пробелу, табуляции, 0Ah, 0Dh или \0
	je isStoppedSymbol          ;     Значит возможно мы дошли до  конца слова
	cmp al, returnSymbol        ;
	je isStoppedSymbol          ;                                        ;
	cmp al, endl                ;
	je isStoppedSymbol          ;
                                ;
	mov es:[di], al             ; Если данный символ входит в слово, добавляем его в результирующую строку
	inc bx
nextLoopParseWord:                                ;
	inc di                      ; Увеличиваем di,si т.е. переходим на следующий символ
	inc si                      ;
                                ;
	loop loopParseWord          ; Пока не превышена максимальная длина слова, парсим

isStoppedSymbol:                ;
	mov al, endl          ;
	mov es:[di], al             ; Загружаем символ конца строки в результирующую строку
	inc si                      ; Увеличиваем si для перезода на следующий символ командной строки
    
	pop bx
	pop di                      ; восстанавливаем регистры
	pop cx                      ;
	pop ax                      ;
	ret                         ;
ENDP   
;**************************************************************************************************************************  
  
;*************************************************************************************************************************
;ds:si - смещение, в котором находится начало строки
;Результат - в ax помещается длина строки 
strlen PROC                     ;
	push bx                     ;
	push si                     ;  Сохраняем используемые далее регистры
	                            ;
	xor ax, ax                  ;  Зануляем ax
                                ;
    startCalc:                  ;
	    mov bl, ds:[si]         ;  Загружаем очередной символ строки из ds со смещением si
	    cmp bl, endl            ;  Сравниваем этот символ с символом конца строки
	    je endCalc              ;  Если это символ конца строки - прыгаем в endCalc и заканчиваем вычисления
                                ;
	    inc si                  ;  Увеличиваем si, т.е. переходим к следующему символу
	    inc ax                  ;  Увеличиваем ax, т.е. длину строки                                                     
	    jmp startCalc           ;  Продолжаем
	                            ;
    endCalc:                    ;
	pop si                      ;
	pop bx                      ;  Восстанавливаем значения
	ret                         ;
ENDP                            ;
;*************************************************************************************************************************

;**************************************************************************************************************************
;Результат в ax - 0 если все хорошо
openFiles PROC                  ;
	push bx                     ;
	push dx                     ; Сохраняем значения регистров          
	
	
	;;;;;;;;;
	push si                                     
	                                            
	lea si, sourcePath                          
	call    strlen               ;in ax-len               
	    
	xor si, si         
	mov si, ax 
	sub si, 1                   
	 
	 
	cmp sourcePath[si], '.' 
	je checkFormat_Error   
	mov cx, ax
	sub cx,2
loopDot:   
	
	sub si, 1
	
	cmp sourcePath[si], '.' 
	je checkFormat_OK  
	loop loopDot 
	        
	                
	checkFormat_Error: 
	pop si
	jmp badOpenSource       
	       
	checkFormat_OK:                                            ;
	pop si  
	
	;;;;;;;
                                ;
	mov ah, 3Dh			        ; Функция 3Dh - открыть существующий файл
	mov al, 02h			        ; Режим открытия файла
	lea dx, sourcePath          ; Загружаем в dx название исходного файла
	mov cx, 00h			        ; 
	int 21h                     ;
                                ;
	jb badOpenSource	        ; Если файл не открылся, то прыгаем в badOpenSource
                                ;
	mov sourceID, ax	        ; Загружаем в sourceId значение из ax, полученное при открытии файла
                                ;
	mov ax, 0			        ; Загружаем в ax 0, т.е. ошибок во время выполнения процедуры не произшло 
	jmp endOpenProc		        ; Прыгаем в endOpenProc и корректно выходим из процедуры
                                ;
badOpenSource:                  ;
	println badSourceText       ; Выводим соответсвующее сообщение
	cmp ax, 02h                 ; Сравниваем ax с 02h
	jne errorFound              ; Если ax != 02h - файл найден, прыгаем в errorFound
                                ;
	println fileNotFoundText    ; Выводим сообщение о том, что файл не найден 
                                ;
	jmp errorFound              ; Прыгаем в errorFound
                                ;       ;       ==//==                               ;
errorFound:                     ;
	mov ax, 1                   ; Загружаем в ax 1, т.е. произошла ошибка
endOpenProc:                    ;
	pop dx                      ;
	pop bx                      ; Восстанавливаем значения регистров и выходим из процедуры
	ret                         ;
ENDP                            ;
;******************************************************************************************************************************        
 
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
	println errorClosingSource  ; Иначе выводим соответсвующее сообщение об ошибке       
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
;******************************************************************************************************************************
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
;******************************************************************************************************************************       
setPosInFileTo MACRO symbolsInt, symbols;
	push ax                     ;
	push bx                     ;
	push cx                     ;
	push dx                     ; Сохраняем значения регистров
    mov bx, sourceID                        ;
	mov ah, 42h                 ; Записываем в ah код 42h - ф-ция DOS уставноки указателя файла
	xor al ,al 			        ; Обнуляем al, т.к. al=0 - код перемещения указателя в начало файла
	mov cx, symbolsInt          ; Обнуляем cx, 
	mov dx, symbols			    ; Обнуляем dx, т.е премещаем указатель на 0 символов от начала файла (cx*2^16)+dx 
	int 21h                     ; Вызываем прерывания DOS для исполнения кодманды   
                                ;
	pop dx                      ; Восстанавливаем значения регистров и выходим из процедуры
	pop cx                      ;
	pop bx                      ;
	pop ax                      ;
ENDM 
;******************************************************************************************************************************
outputSymbHex proc 
push cx
push ax

	mov cl,4
	mov ch,0
	lodsb
	mov bl,16
toHex:	
    
	 xor ah,ah
	div bl
	cmp ah,10
	je A
	cmp ah,11
	je B 
	cmp ah,12
	je C 
	cmp ah,13
	je D
	cmp ah,14
	je E
	cmp ah,15
	je F  
	add ah,'0'
	mov dl,ah
	xor dh,dh
	push dx  
	inc ch
nextToHex:
	
	
	cmp al,0
	jne toHex
	
	jmp writeHex
A:
	mov dl,'A'
	xor dh,dh
	push dx
	inc ch
	jmp nextToHex
B:
	mov dl,'B'
	xor dh,dh
	push dx
	inc ch
	jmp nextToHex
C:
	mov dl,'C'
	xor dh,dh
	push dx
	inc ch
	jmp nextToHex
D:
	mov dl,'D'
	xor dh,dh
	push dx 
	inc ch
	jmp nextToHex
E:
	mov dl,'E'
	xor dh,dh
	push dx
	inc ch
	jmp nextToHex
F:
	mov dl,'F'
	xor dh,dh
	push dx
	inc ch
	jmp nextToHex
writeHex:
	
	sub cl,ch
	mov dh,'0'
loopZero:
    
	putch dh 
	
	dec cl
	cmp cl,0
jne loopZero

loopNumber:
	pop dx
	putch dl 
	dec ch
	cmp ch ,0
	jne loopNumber
	
	pop ax
	pop cx
   ret
  endp


          
;******************************************************************************************************************************



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
;******************************************************************************************************************************    
displayPage PROC                     ;
	                         ;
	push bx                             ;
	push cx                             ;
	push dx                             ;
	push si                             ;
	push di                             ; Сохраняем значения регистров
    
                                     ;
	                   ; Загружаем в bx ID файла-источника 
	
	               ; Вызываем процедую смещения курсора в начало файла
                                        ;
    call readFromFile                   ; Вызов процедуры чтения из файла
	                                    ;
	cmp ax, 0                           ; Сравнивание ah с 0 для проверки на конец файла
	je finishProcessing3                 ; Если ah == 0, то буффер пуст и мы дошли до конца файла
      setCursor 0,0 
     emptyPage
    mov countStr,0
startLoopProcessing:                                        ;
	lea si, buffer                      ; Иначе загружаем в si 
	
	mov cx, ax					        ; В cx загружаем ax, т.е кол-во элементов в буффере (кол-во элементов считанных с файла)
startLoopProcessing2:	

      cmp countStr,maxNumOfString 
    je finishProcessing2    
      	
     add countstr,1
	
	
	
   	
	
	mov count,0
loopProcessing:                         ;
	call outputSymbHex
	add count,1
	cmp count,maxWordSize
	je goToNewLine
	mov al,' '
	putch al
	
	cmp count,8
	je twoSpace
	
nextProcessing:

	dec cx
	call checkEndBuff                   ; Вызываем процедуру проверки конца буффера, если буффер пуст - подгружаем данные из файла
	cmp ax, 2                           ; Если после вызова процедуры checkEndBuff в ax лежит 2, то заканчиваем обработку - прыгаем в ax
	je finishProcessing                 ; Прыгаем в finishProcessing
	cmp ax, 1                           ; ==//== в ax лежит 1, то буффер был пуст и были подгружены новые данные из файла
	je startLoopProcessing2    
	cmp ax,0
	je loopProcessing
	
twoSpace:
	mov al,' '
	putch al
	jmp nextProcessing
goToNewLine:
	
	
    jmp nextProcessing                                    ;
	                 ;
finishProcessing2:

mov ax,1   
jmp finishProcessing1

finishProcessing3:
    cmp userIn,51h
    je  finishProcessing4
   mov ax,2 
   sub firstStrPtr,1
   jmp finishProcessing1
finishProcessing4:
     mov ax,2 
   sub firstStrPtr, maxNumOfString
   jmp finishProcessing1                                 ;
finishProcessing:                       ;
   mov ax,0  
finishProcessing1:
                                        ;
	pop di                              ; Восстанавливаем значения 
	pop si                              ;
	pop dx                              ;
	pop cx                              ;
	pop bx                              ;
	                            ;
	ret                                 ;
ENDP                                    ;
;************************************************************************************************************************** 
;*****************************************************************


   
;*****************************************************************************************************************************   
;reads to buffer maxWordSize symbols
;Результат: в ax помещается колв-во считанных из файла символов
readFromFile PROC                   ;
	push bx                         ;
	push cx                         ;
	push dx                         ; Сохраняем значения регистров
    setPosInFileTo degree,posInFile                              ;
	mov ah, 3Fh                     ; Загружаем в ah код 3Fh - код ф-ции чтения из файла
	mov bx, sourceID                ; В bx загружаем ID файла, из которого собираемся считывать
	mov cx, maxWordSize             ; В cx загружаем максимальный размер слова, т.е. считываем максимальное кол-во символов (maxWordSize символов)
	lea dx, buffer                  ; В dx загружаем смещения буффера, в который будет считывать данные из файла
	int 21h                         ; Вызываем прерывание для выполнения ф-ции
                                    ;
	jnb goodRead					; Если ошибок во время счтения не произошло - прыгаем в goodRead
                                    ;
	println errorReadSourceText     ; Иначе выводим сообщение об ошибке чтения из файла
	mov ax, 0                       ; Записываем в ax 0
                                    ;
goodRead:      
    cmp posInFile,65520
    je checkRead
     jmp endRead1
checkRead:   
    cmp ax,maxWordSize
    jne endRead1
    add degree,1
    mov posInFile,0
    jmp endRead2
endRead1:                         ;   
    add posInFile,ax         
endRead2:                       ;
	pop dx                          ; Восстанавливаем значения регистров
	pop cx                          ;
	pop bx                          ;
	ret                             ;
ENDP                                ;
;*****************************************************************************************************************************
             
;*****************************************************************************************************************************             
                              ;
;******************************************************************************************************************************
               
;******************************************************************************************************************************               
;Результат: помещает в ax -
;	ax = 0 - буффер еще не пуст, т.е. еще есть данные для обработки
;	ax = 1 - буффер был обработан, данные записаны, и новый "кусок" файла был подгружен в буффер 
;	ax = 2 - буффер был обработан, данные записаны, была совершена попытка подгузки данных, но программа уже дошла до конца файла
checkEndBuff PROC               ;
	cmp cx, 0                   ; Сравниваем cx с нулем
	jne notEndOfBuffer          ; Если cx != 0, то буффер еще не полностью записан, прыгаем в notEndOfBuffer
                                ;
	
                                ;
skipWrite:  
    
                          ;
	call readFromFile           ; Считываем часть файла
	cmp ax, 0                   ; Сравниваем ax с нулем, если ax == 0, то произошла ошибка, соответсвенно переходим в endOfProcessing
	je endOfProcessing          ;
    lea si,buffer                            ;
	
	mov cx, ax					; Записываем в cx кол-во символов в буффере из ax
	
                                ;
	mov ax, 1                   ; Записываем в ax - конец буффера
	ret                         ;
                                ;
endOfProcessing:                ;
	mov ax, 2                   ; Конец обработки - помещаем в ax условный код 2
	ret                         ;
                                ;
notEndOfBuffer:                 ;
	mov ax, 0                   ; Если чтение еще не окончено - записываем условный код 0
	ret                         ;
ENDP                            ;
;*******************************************************************************************************************************

end main