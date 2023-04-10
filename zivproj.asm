IDEAL
MODEL small
STACK 100h
DATASEG
    BoardBMP    db  'imgs\Boardlol.bmp',0   ; board picture file location
    MainMenuBMP db  'imgs\mainmenu.bmp',0   ; main menu picture file location1
    TrainingBMP db  'imgs\Training.bmp',0   ; training picture file location
    
    UPBetterBMP db  'imgs\betterlu.bmp',0   ; "better luck next time" pop up picture file location
    UPBotWonBMP db  'imgs\botwon.bmp',0     ; "bot won" pop up picture file location
    UPPlyWonBMP db  'imgs\youwon.bmp',0     ; "you won" pop up picture file location
    UPTieBMP    db  'imgs\tie.bmp',0        ; "tie" pop up picture file location

    filehandle  dw  ? 
    Header      db  54 dup(0)
    Palette     db  256*4 dup (0)
    ScrLine     db  320 dup (0)

    X           dw  ?                       ; X location for drawing pixels
    Y3          dw  ?                       ; Y location for drawing pixels

    x1          dw  ?                       ; top left corner x location for drawing a rectangle with the draw_rectangle procedure
    y1          dw  ?                       ; top left corner y location for drawing a rectangle with the draw_rectangle procedure
    x2          dw  ?                       ; bottom right corner x location for drawing a rectangle with the draw_rectangle procedure
    y2          dw  ?                       ; top right corner y location for drawing a rectangle with the draw_rectangle
    color       db  ?                       ; color variable for drawing a rectangle with the draw_rectangle procedure

    R           db  249                     ; code number for red
    P           db  253                     ; code number for purple
    Y           db  251                     ; code number for yellow
    G           db  250                     ; code number for green
    C           db  254                     ; code number for cyan
    B           db  4                       ; code number for blue
    def         db  247                     ; code number for default color (grey)

    RF          db  ?                       ; flags to check if red was already pressed this turn
    PF          db  ?                       ; flags to check if purple was already pressed this turn
    YF          db  ?                       ; flags to check if yellow was already pressed this turn
    GF          db  ?                       ; flags to check if green was already pressed this turn
    CF          db  ?                       ; flags to check if cyan was already pressed this turn
    BF          db  ?                       ; flags to check if blue was already pressed this turn

    x_bot_block dw  1                       ; X location for the bot block
    y_bot_block dw  142                     ; Y location for the bot block

    btemp       db  ?                       ; temporary bulls variable so it doesn't ruin the current bulls variable in the draw_score_... procedures
    ctemp       db  ?                       ; temporary cows variable so it doesn't ruin the current cows variable in the draw_score_... procedures

    bulls       db  ?                       ; variable to store the returned number of bulls from the play_result procedure
    cows        db  ?                       ; variable to store the returned number of cows from the play_result procedure

    combLeft    dw  360                     ; number of possible combinations left in the population
    population  dw  360 dup(?)              ; an array of 360 words that include all possible combinations for Bulls and Cows   (6!/(6-4)!
    first       dw  ?                       ; loop counter for the first digit of a combination
    second      dw  ?                       ; loop counter for the second digit of a combination
    third       dw  ?                       ; loop counter for the third digit of a combination
    fourth      dw  ?                       ; loop counter for the fourth digit of a combination

    comb1       equ [bp + 4]                ; a combination to check the score with the second combination in the play_result procedure, other thing that need the pushed variable
    comb2       equ [bp + 6]                ; the second combination

    counter     dw  ?                       ; counter for the x location of the first player's guess blocks
    ycounter    dw  22                      ; counter for the y location of the player's guess blocks
    botcode     dw  ?                       ; the code the player is trying to match
    code        dw  ?                       ; the code the bot is trying to match
    lastGuess   dw  ?                       ; the last guess made by the bot
    plastGuess  dw  ?                       ; the last guess made by the player
    mark        dw  8000h                   ; a binary code to mark combinations are not possible guessed already
    lineDown    db  10,13,'$'               ; a string for a line break in output
    clear_scrn  db  48 dup(10,13),'$'       ; a string for clearing the screen at the end of the program
    wscore      db  ?                       ; a character for the draw_score procedure
    easteregg   db  0                       ; ?????????

CODESEG

proc OPEN_FILE
    ; Open file
    mov ah, 3Dh
    xor al, al
    mov dx, si
    int 21h
    jc openerror
    mov [filehandle], ax
    ret
    openerror:
    mov dl, 'E'
    mov ah, 2h
    int 21h
    ret
    endp OPEN_FILE

proc READ_HEADER
    ; Read BMP file header, 54 bytes
    mov ah,3fh
    mov bx, [filehandle]
    mov cx,54
    mov dx,offset Header
    int 21h
    ret
    endp READ_HEADER

proc READ_PALETTE
    ; Read BMP file color palette, 256 colors * 4 bytes (400h)
    mov ah,3fh
    mov cx,400h
    mov dx,offset Palette
    int 21h
    ret
    endp READ_PALETTE

proc COPY_PAL
    ; Copy the colors palette to the video memory
    ; The number of the first color should be sent to port 3C8h
    ; The palette is sent to port 3C9h
    mov si,offset Palette
    mov cx,256
    mov dx,3C8h
    mov al,0
    ; Copy starting color to port 3C8h
    out dx,al
    ; Copy palette itself to port 3C9h
    inc dx
    PalLoop:
    ; Note: Colors in a BMP file are saved as BGR values rather than RGB.
    mov al,[si+2] ; Get red value.
    shr al,2 ; Max. is 255, but video palette maximal
    ; value is 63. Therefore dividing by 4.
    out dx,al ; Send it.
    mov al,[si+1] ; Get green value.
    shr al,2
    out dx,al ; Send it.
    mov al,[si] ; Get blue value.
    shr al,2
    out dx,al ; Send it.
    add si,4 ; Point to next color.
    ; (There is a null chr. after every color.)
    loop PalLoop
    ret
    endp COPY_PAL

proc COPY_BMP
    ; BMP graphics are saved upside-down.
    ; Read the graphic line by line (200 lines in VGA format),
    ; displaying the lines from bottom to top.
    mov ax, 0A000h
    mov es, ax
    mov cx,200
    PrintBMPLoop:
    push cx
    ; di = cx*320, point to the correct screen line
    mov di,cx
    shl cx,6
    shl di,8
    add di,cx
    ; Read one line
    mov ah,3fh
    mov cx,320
    mov dx,offset ScrLine
    int 21h
    ; Copy one line into video memory
    cld ; Clear direction flag, for movsb
    mov cx,320
    mov si,offset ScrLine
    rep movsb ; Copy line to the screen
    ;rep movsb is same as the following code:
    ;mov es:di, ds:si
    ;inc si
    ;inc di
    ;dec cx
    ;loop until cx=0
    pop cx
    loop PrintBMPLoop
    ret
    endp COPY_BMP

proc COPY_POP_UP_BMP 
    ; BMP graphics are saved upside-down.
    ; Read the graphic line by line (200 lines in VGA format),
    ; displaying the lines from bottom to top.
    mov ax, 0A000h
    add ax, 1200 ; 60*320/16
    mov es, ax
    mov cx,85
    PrintBMPLoop1:
    push cx
    ; di = cx*320, point to the correct screen line
    mov di,cx
    shl cx,6
    shl di,8
    add di,cx
    ; Read one line
    mov ah,3fh
    mov cx,152
    mov dx,offset ScrLine
    int 21h
    ; Copy one line into video memory
    cld ; Clear direction flag, for movsb
    mov cx,150
    mov si, offset ScrLine
    add di, 85
    rep movsb ; Copy line to the screen
    ;rep movsb is same as the following code:
    ;mov es:di, ds:si
    ;inc si
    ;inc di
    ;dec cx
    ;loop until cx=0
    pop cx
    loop PrintBMPLoop1
    ret
    endp COPY_POP_UP_BMP

proc CREATE_POPULATION ; This procedure creates an array called "population" that contains all possible combinations of 4 digits between 0 and 5 without repeating.
    push    ax bx cx dx si                      ; Save the value of the register on the stack for it to be restored at the end of the procedure.
    mov     si,             offset population   ; Set SI to the memory address of the first element of the "population" array so the array can be read easily.
    mov     [first], 0                          ; Set the value of "first" to 0 so it goes through all possible combinations.
    cmp     [word ptr si],  ?                   ; Check if the population array is empty
    je      firstLoop                           ; If the population array is fill it with the combinations
    mov     cx, 360                     
    overrideLoop:                               ; Else override the not possible current population array with the combination without the mark 
    and     [word ptr si],  0fffh
    add     si,             2
    loop    overrideloop 
    jmp     endLoop
    firstLoop:                                  ; Loop through all possible values of the first digit.
    mov     [second],       0           
    secondLoop:                                 ; Loop through all possible values of the second digit. 
    mov     ax,             [second]
    cmp     ax,             [first]             ; Compare the value of "second" to "first" to make sure they aren't the same.
    jne     continue            
    jmp     nosave                              ; if they are the same, jump to "nosave" so there wont be any repetition in numbers.
    continue:           
    mov     [third],        0       
    thirdLoop:                                  ; Loop through all possible values of the third digit.
    mov     ax,             [third]
    cmp     ax,             [first]             ; Compare the value of "third" to "first" to make sure they aren't the same.
    jne     continue1           
    jmp     nosave2                             ; if they are the same, jump to "nosave2" so there wont be any repetition in numbers.
    continue1:       
    cmp     ax,             [second]            ; Compare the value of "third" to "second" to make sure they aren't the same.
    jne     continue2           
    jmp     nosave2                             ; if they are the same, jump to "nosave2" so there wont be any repetition in numbers.
    continue2:      
    mov     [fourth],       0          
    fourthLoop:                                 ; Loop through all possible values of the fourth digit.
    mov     ax,             [fourth]
    cmp     ax,             [first]             ; Compare the value of "fourth" to "first".
    jne     continue3           
    jmp     nosave1                             ; if they are the same, jump to "nosave1" so there wont be any repetition in numbers.
    continue3:       
    cmp     ax,             [second]            ; Compare the value of "fourth" to "second".
    je      nosave1                             ; If "fourth" and "second" are equal, jump to "nosave1" so there wont be any repetition in numbers.
    cmp     ax,             [third]             ; Compare the value of "fourth" to "third".
    je      nosave1                             ; If "fourth" and "third" are equal, jump to "nosave1" so there wont be any repetition in numbers.

    mov     bx,             [first]             ; Combine the values of "first", "second", "third", and "fourth" into a single word the represents a combination (0000444333222111b).
    mov     ax,             [second]
    shl     ax,             3                   ; shift 3 times to get to the next digit.
    add     bx,             ax
    mov     ax,             [third]
    shl     ax,             6                   ; shift 3 more times to get to the next digit.
    add     bx,             ax
    mov     ax,             [fourth]
    shl     ax,             9                   ; shift 3 more times to get to the next digit.
    add     bx,             ax
    mov     [si],           bx                  ; add the new combination in the "population" array.
    add     si,             2                   ; Increment the pointer to the next element in the array.
    nosave1:
    inc     [fourth]                            ; Increment the value of "fourth" to move to the next possible value.
    cmp     [fourth],       6                   ; If all possible values of "fourth" have been tried, exit the loop.
    je      continue4
    jmp     fourthloop
    continue4:
    nosave2:
    inc     [third]                             ; Increment the value of "third" to move to the next possible value.
    cmp     [third],        6                   ; If all possible values of "third" have been tried, exit the loop.
    je      continue5
    jmp     thirdloop
    continue5:
    nosave:
    inc     [second]                            ; Increment the value of "second" to move to the next possible value.
    cmp     [second],       6                   ; If all possible values of "second" have been tried, exit the loop.
    je      continue6
    jmp     secondLoop                          ; Otherwise, jump back to the beginning of the second loop to try the next value of "second".
    continue6:
    inc     [first]                             ; Increment the value of "first" to move to the next possible value.
    cmp     [first],        6                   ; If all possible values of "first" have been tried, exit the loop.
    je      endLoop
    jmp     firstLoop                           ; Otherwise, jump back to the beginning of the first loop to try the next value of "first".
    endLoop:
    pop     si dx cx bx ax                      ; Restore the value of the registers from the stack.
    ret 
    endp    CREATE_POPULATION

proc PLAY_RESULT ; This procedure gets two combinations and returns the score between the two. for example 0123 3210 : 0 bulls 4 cows ... 
    push    bp
    mov     bp,         sp
    push    ax bx cx dx si      ; Save the value of the register on the stack for it to be restored at the end of the procedure.
    mov     [cows],     0       ; Initialize the value of cows (the amount of numbers they have in common not in the right place).
    mov     [bulls],    0       ; Initialize the value of bulls. (the amount of numbers they have in common not in the right place).
    mov     cl,         0       ; Amount of shifts needed to get to the next digit in the combination for comb1.
    comb1loop:
    mov     ax,         comb1
    shr     ax,         cl      ; Shift the numbers of times needed to get the the next digit of the combination.
    and     ax,         0111b   ; Get the 3 bits representing the digit in the combination.
    mov     ch,         0       ; Amount of shifts needed to get to the next digit in the combination for comb2.
    comb2loop:
    mov     bx,         comb2
    mov     dl,         cl      ; Switch cl with ch (in a way that saves both) because you can only shift with cl.
    mov     cl,         ch
    shr     bx,         cl      ; Shift the numbers of times needed to get the the next digit of the combination.
    mov     cl,         dl
    and     bx,         0111b   ; Get the 3 bits representing the digit in the combination.

    cmp     al,         bl      ; Compare the digits from both combination to check if they are the same.
    je      ADDBULLCOW          ; If so there shouldn't be any others so no need to continue the loop.
    add     ch,         3       ; Else add 3 shifts so that the next time the loop runs it will skip the last bits of the last digit.
    cmp     ch,         12      ; Check if it already passed all the digits.
    jne     comb2loop 
    je      pass1               ; if passed all don't rerun the loop.
    AddBullCow:
    cmp     cl,         ch      ; Check if the digit is also in the same position as the other.
    jne     AddCow              ; If not in the same position increment the cows variable.
    inc     [bulls]             ; If they are also in the same position increment the bulls variable.
    jmp     pass1
    AddCow:
    inc     [cows]
    pass1:
    add     cl,         3       ; Add 3 shifts so the next time the loop runs it will skip the last bits of the last digit.
    cmp     cl,         12      ; Check if it already passed all the digits.
    jne     comb1loop 
    pop     si dx cx bx ax bp   ; Restore the value of registers from the stack.
    ret     4
    endp PLAY_RESULT

proc KEEP_POSSIBLE_COMB ; This procedure marks all the impossible combinations in the population array. 
    ; [i for i in population if bulls,cows == play_result(guess, i)]
    mov     dh,             [bulls] 
    shl     [cows],         3 
    add     dh,             [cows]              ; this creates a byte that represents both the cows and bulls easily so it would be easy to compare them to the other combinations results
    mov     si,             offset population 
    mov     [counter],      0 
    populationLoop:                             ; Start looping through the population array.
    cmp     [word ptr si],  1000000000000000b   ; Compare the value stored at the memory location pointed to by SI with 1000000000000000b to check if it has been marked.
    ja      pass                                ; if it marked no need to take change it
    push    [si]                                ; if its not check if it gets the same score as the actual code with the last guess the bot made
    push    [lastguess] 
    call    PLAY_RESULT                         ; Call the PLAY_RESULT function. to check if its a possible combination
    mov     dl,             [bulls] 
    shl     [cows],         3 
    add     dl,             [cows]              ; this creates a byte that represents both the cows and bulls easily so it would be easy to compare it to the bulls and cows of the guess
    cmp     dl,             dh                  ; compare between the bulls and cows of the guess and the bulls and cows of the combination
    je      pass                                ; If they are equal it is a possible combination
    or      [word ptr si],  1000000000000000b   ; Else mark it as a possible combination.
    dec     [combleft] 
    pass: 
    add     si,             2                   ; Move the memory pointer to the next element.
    inc     [counter]
    cmp     [counter],      360                 ; check if it had already passed the whole array
    jne     populationLoop 
    ret 
    endp KEEP_POSSIBLE_COMB

proc GUESS ; using a population of all possible combinations, with the most significant bit set to mark them as not possible.
    mov     si,             offset population 
    mov     ah,             0
    int     1Ah                                 ; Get the current time and use it to seed the random number generator
    mov     ax,             dx
    xor     dx,             dx
    mov     cx,             [combleft]          ; Calculate the index of the guess to be generated
    div     cx
    cmp     dl,             0                   ; If the random number generated was 0 switch it with one so the code doesn't break
    jne     findit
    mov     dl,             1
    findit:                                     ; Enter a loop to find the next valid guess
    cmp     [word ptr si],  1000000000000000b   ; If the current guess has already been eliminated, move on to the next guess
    jb      found
    next:
    add     si,             2
    jmp     findit
    findit1:                                    ; If we have found a valid guess but its not the valid index,
    add     si,             2                   ; wrap around to the beginning of the loop and continue searching
    jmp     findit
    found:
    dec     dl                                  ; If we have found a valid guess, decrement dl
    cmp     dl,             0                   ; and check if we have generated enough guesses
    ja      findit1 
    mov     ax,             [word ptr si] 
    mov     [lastguess],    ax                  ; If we have generated enough guesses, store the current guess in lastguess and return
    ret
    endp GUESS

proc PRINT_CODE ; This procedure prints the combination stored in the stack [bp+4].
    push    bp
    mov     bp, sp
    push    ax cx dx    ; Save the value of the registers on the stack.
    mov     cl,     0   ; Numbers of shifts needed to get to the next digit in the combination.
    PRINT_CODE_LOOP:
        mov     dx,     comb1   ; Set dx as the combination so it doesn't mess up the combination.
        shr     dx,     cl      ; Shift the numbers of times needed to get the the next digit of the combination.
        and     dx,     111b    ; Get the 3 bits representing the digit in the combination.
        add     dx,     '1'     ; Convert the digit to an ASCII character.
        mov     ah,     2 
        int     21h 
        add     cl,     3       ; add 3 shifts so the next time the loop runs it will skip the last bits of the last digit.
        cmp     cl,     12      ; check if it already passed all the digits.
        je      jenprint        ; if not rerun else print linedown.
        jmp     PRINT_CODE_LOOP

    jenprint:
    mov     dx,     offset lineDown ; print('\n')
    mov     ah,     9 
    int     21h 
    pop     dx cx ax ; Restore the value of the register from the stack.
    pop     bp
    ret     2
    endp PRINT_CODE

proc DRAW_BOT_GUESS ; This procedure draws the guess the bot made
    push    ax bx cx dx si di
    mov     cl,     0               ; number of shifts needed to get to the next digit in the combination
    biggie: 
    push    cx                      ; save the counter because it is used as something else in the loop
    mov     dx,     [lastguess] 
    shr     dx,     cl  
    and     dx,     111b            ; get the next digit in the combination by shifting until the digit then getting only the relevant bits
    ; gets the next digit in the combination and checks what number it is to see what color to paint
    cmp     dl,     0               ; if its 0 then paint red
    je      red1    
    cmp     dl,     1               ; if its 1 then paint purple
    je      purple1 
    cmp     dl,     2               ; if its 2 then paint yellow
    je      yellow1 
    cmp     dl,     3               ; if its 3 then paint green
    je      green1  
    cmp     dl,     4               ; if its 4 then paint cyan
    je      cyan1   
    jne     blue1                   ; else paint blue
    red1:   
    mov     al,     [R]             ; turns the color to red
    jmp     h   
    purple1:    
    mov     al,     [P]             ; turns the color to purple
    jmp     h   
    yellow1:    
    mov     al,     [Y]             ; turns the color to yellow
    jmp     h   
    green1: 
    mov     al,     [G]             ; turns the color to green
    jmp     h   
    cyan1:  
    mov     al,     [C]             ; turns the color to cyan
    jmp     h   
    blue1:  
    mov     al,     [B]             ; turns the color to blue
    jmp     h
    h:
    mov     [color],al
    
    mov     bx,     [x_bot_block] 
    mov     [x1],   bx              ; puts the x1 locations for the draw_rectangle procedure
    add     bx,     31
    mov     [x2],   bx              ; puts the x2 locations for the draw_rectangle procedure

    mov     bx,     [y_bot_block]
    mov     [y1],   bx              ; puts the y1 locations for the draw_rectangle procedure
    add     bx,     19
    mov     [y2],   bx              ; puts the y1 locations for the draw_rectangle procedure

    call    DRAW_RECTANGLE
    pop     cx
    add     cl,     3               ; adds 3 to the shifts
    add     [x_bot_block],32        ; adds 32 so the next loop it will paint the next block
    cmp     cl,     12              ; check if all the combination was painted
    je      endthis1
    jmp     biggie
    endthis1:
    pop     di si dx cx bx ax
    ret
    endp DRAW_BOT_GUESS

proc DRAW_SCORE ; This procedure draws the score based on the bulls and cows variables
    push  ax bx cx dx si di
    ; Save the values of the bulls and cows variables, as they will be modifying them
    mov     bl,         [bulls]         
    mov     [btemp],    bl
    mov     bl,         [cows]          
    mov     [ctemp],    bl
    ; Add the values of the bulls and cows variables and save it on the stack
    add     bl,         [btemp]         
    push    bx

    ; Draw the first block
    ; Check if there are any bulls (right index && right color) to be drawn
    cmp     [btemp],    0
    jne     black
    mov     [color],    255
    dec     [ctemp]
    jmp     i
    black:
    mov     [color],    0
    dec     [btemp]
    i:
    cmp     [wscore],   'b'             ; Checks if it shall draw a rectangle for the player, bot or trainer
    jne     n1
    mov     [x1],       129             ; Set the x1 location for the draw_rectangle procedure (for the bot's guess)
    mov     [x2],       144             ; Set the x2 location for the draw_rectangle procedure (for the bot's guess)
    mov     ax,         [y_bot_block]   ; Set the y location for the draw_rectangle procedure (for the bot's guess)
    jmp     block2
    n1:
    cmp     [wscore],   'p'             
    jne     n3
    mov     [x1],       289             ; Set the x1 location for the draw_rectangle procedure (for the user's guess)
    mov     [x2],       304             ; Set the x2 location for the draw_rectangle procedure (for the user's guess)
    mov     ax,         [ycounter]      ; Set the y location for the draw_rectangle procedure (for the user's guess)
    jmp     block2
    n3:
    mov     [x1],       211             ; Set the x1 location for the draw_rectangle procedure (for the trainer)
    mov     [x2],       226             ; Set the x2 location for the draw_rectangle procedure (for the trainer)
    mov     ax,         [ycounter]      ; Set the y location for the draw_rectangle procedure (for the trainer)
    block2:
    mov     [y1],       ax              ; Set the y1 location for the draw_rectangle procedure (for everyone)
    add     ax,         9               ; Set the y2 location for the draw_rectangle procedure (for everyone)
    mov     [y2],       ax              
    call    DRAW_RECTANGLE              ; Draw the rectangle using the previously set x and y coordinates

    ; Draw the second block
    ; Check if there are any bulls (right index && right color) to be drawn
    cmp     [btemp],    0
    jne     black1
    mov     [color],    255
    dec     [ctemp]
    jmp     i1
    black1:
    mov     [color],    0
    dec     [btemp]
    i1:

    ; Set the x coordinates for the second block (which is 16 pixels to the right of the first block)
    add     [x1],       16              
    add     [x2],       16              
    call    DRAW_RECTANGLE

    ; Checks if there are more points to draw
    pop     bx
    cmp     bl,         2
    jne     ma
    jmp     byeproc
    ma:
    push    bx
    ; Draw the third block
    ; Check if there are any bulls (right index && right color) to be drawn
    cmp     [btemp],    0
    jne     black2
    mov     [color],    255
    dec     [ctemp]
    jmp     i2
    black2:
    mov     [color],    0
    dec     [btemp]
    i2:
    cmp     [wscore],   'b'             ; Checks if it shall draw a rectangle for the player, bot or trainer
    jne     n4
    mov     [x1],       129             ; Set the x1 location for the draw_rectangle procedure (for the bot's guess)
    mov     [x2],       144             ; Set the x2 location for the draw_rectangle procedure (for the bot's guess)
    mov     ax,         [y_bot_block]   ; Set the y location for the draw_rectangle procedure (for the bot's guess)
    jmp     block4
    n4:
    cmp     [wscore],   'p'
    jne     n5
    mov     [x1],       289             ; Set the x1 location for the draw_rectangle procedure (for the user's guess)
    mov     [x2],       304             ; Set the x2 location for the draw_rectangle procedure (for the user's guess)
    mov     ax,         [ycounter]      ; Set the y location for the draw_rectangle procedure (for the user's guess)
    jmp     block4
    n5:
    mov     [x1],       211             ; Set the x1 location for the draw_rectangle procedure (for the trainer's guess)
    mov     [x2],       226             ; Set the x2 location for the draw_rectangle procedure (for the trainer's guess)
    mov     ax,         [ycounter]      ; Set the y location for the draw_rectangle procedure (for the trainer's guess)
    block4:
    add     ax,         10              
    mov     [y1],       ax              ; Set the y1 location for the draw_rectangle procedure (for everyone)
    add     ax,         9               ; Set the y2 location for the draw_rectangle procedure (for everyone)
    mov     [y2],       ax                       
    call    DRAW_RECTANGLE              ; Draw the rectangle using the previously set x and y coordinates;

    ; Checks if there are more points to draw
    pop     bx
    cmp     bl,         4
    jne     byeproc
    ; Draw the fourth block
    ; Check if there are any bulls (right index && right color) to be drawn
    cmp     [btemp],    0
    jne     black3
    mov     [color],    255
    dec     [ctemp]
    jmp     i3
    black3:
    mov     [color],    0
    dec     [btemp]
    i3:
    ; Set the x coordinates for the second block (which is 16 pixels to the right of the first block)
    add     [x1],       16
    add     [x2],       16
    call    DRAW_RECTANGLE              ;
    byeproc:
    cmp     [wscore],   'b'
    jne     platra
    mov     [x_bot_block],1             ; changes the x to the default x location
    sub     [y_bot_block],20            ; changes the y to the next block y location
    jmp     lproc2
    platra:
    add     [ycounter], 20              ; changes the y to the next block y location
    lproc2:
    pop     di si dx cx bx ax
    ret 
    endp DRAW_SCORE

proc DRAW_RECTANGLE ; This procedure draws a rectangle from x1 to x2 and y1 to y2 in the color of the color variable
    push    ax bx cx dx si di   ; Save the values of these registers on the stack
    mov     ax,         2       ; hide the cursor so it won't be drawn
    int     33h
    mov     ax,         [x1] 
    mov     [x],        ax      ; Move the x coordinate of the top left corner of the rectangle into the x variable
    mov     cx,         [x2] 
    sub     cx,         [x1]    ; run the loop the width of the given rectangle
    deltaXloop:                 ; Loop over each x coordinate of the rectangle
    push    cx 
    mov     ax,         [y1]    ; Move the y coordinate of the top left corner of the rectangle into the y3 variable
    mov     [y3],       ax 
    mov     cx,         [y2] 
    sub     cx,         [y1]    ; run the loop the height of the given rectangle
    deltaYloop:                 ; Loop over each y coordinate of the rectangle
    ; draw a new pixel
    push    cx    
    mov     bh,         0           
    mov     cx,         [x] 
    mov     dx,         [y3]
    mov     al,         [color] 
    mov     ah,         0ch 
    int     10h                 
    inc     [y3]                ; Increment the y coordinate for the next repetition of the loop
    pop     cx 
    loop    deltaYloop 
    inc     [x]                 ; Increment the x coordinate for the next repetition of the loop
    pop     cx 
    loop    deltaXloop          ; Repeat the loop until all x-coordinates have been drawn

    mov     ax,         1       ; Show the cursor
    int     33h
    pop     ax bx cx dx si di   ; Restore the values of the registers from the stack
    ret
    endp DRAW_RECTANGLE

start:
    mov ax, @data
    mov ds, ax
    ; reset all the nont const variable
    mov [easteregg]     ,   0
    mov [btemp]         ,   ?               
    mov [ctemp]         ,   ?               
    mov [bulls]         ,   ?               
    mov [cows]          ,   ?               
    mov [combLeft]      ,   360                  
    mov [first]         ,   ?               
    mov [second]        ,   ?               
    mov [third]         ,   ?               
    mov [fourth]        ,   ?               
    mov [counter]       ,   ?               
    mov [ycounter]      ,   22              
    mov [botcode]       ,   ?
    mov [code]          ,   ?               
    mov [lastGuess]     ,   ?               
    mov [plastGuess]    ,   ?
    mov [RF]            ,   0       ; set red flag to 0 
    mov [PF]            ,   0       ; set purple flag to 0
    mov [YF]            ,   0       ; set yellow flag to 0
    mov [GF]            ,   0       ; set green flag to 0
    mov [CF]            ,   0       ; set cyan flag to 0
    mov [BF]            ,   0       ; set blue flag to 0
    mov [mark]          ,   8000h
    mov [ycounter]      ,   22

    ; Graphic mode
    mov ax, 13h
    int 10h
    ; Open the main menu bitmap
    mov si, offset mainmenubmp
    call OPEN_FILE
    call READ_HEADER
    call READ_PALETTE
    call COPY_PAL
    call COPY_BMP
    
    uno2:
            push cx
            mov cx, 0fh
            _1sec12:
                push cx
                mov cx, 0ffffh
                _2sec12:
                loop _2sec12
                pop cx
            loop _1sec12
            pop cx

    ; reset the mouse
    mov ax, 0
    int 33h
    ; make mouse visible
    mov ax, 1
    int 33h
    waitforchoice:
        mov ax, 3
        int 33h
        shr cx,1
        ; chek if the user pressed left click on one of the three buttons
        cmp bl, 1
        jne waitforkey
        
        cmp cx, 110
        jb waitforkey
        
        cmp cx, 210
        ja waitforkey
        
        cmp dx,85
        jb waitforkey
        
        cmp dx, 187
        ja waitforkey
        ; if he pressed on one of the three check which button and go to the right code segement (PlayGameVsBot\Train\Exit)
        cmp dx, 114
        jb PLAYGAMEVSBOT

        cmp dx,158
        jb con
        jmp exit
        con:
        
        cmp dx, 122
        jb waitforkey

        cmp dx, 151
        ja waitforkey

        jmp Train
        waitforkey:
         mov ah,1
        int 16h ; BIOS input 
        jz waitforchoice ; if there isn't any input jmp back
        ;read key
        in al,60h
        cmp al, 1 ; if esc exit
        jne notEsc
        jmp exit
        notEsc:
        cmp al, 2 ; if 1 training
        jne not1
        jmp Train
        not1:
        cmp al, 3 ; if 2 play vs bot
        je PLAYGAMEVSBOT
        jmp waitforchoice
PlayGameVsBot:
    mov [y_bot_block], 142
    mov [x_bot_block], 1
    mov ax, 2
    int 33h
    mov si, offset boardbmp
    call OPEN_FILE
    call READ_HEADER
    call READ_PALETTE
    call COPY_PAL
    call COPY_BMP
    
    ; Initialize count and generate population of all possible combinations
    call CREATE_POPULATION
    
    call guess ; Generate a random code for the player to guess
    mov ax, [lastguess]
    mov [botcode], ax ; Set the "botcode" variable to the genrated code

    mov [counter], 1 ; Set the "counter" variable to the first pixel in the x axis
    ; Gets the code from the player via BIOS input
    mov ax, 0
    int 33h
    mov ax, 1
    int 33h
    waitfordata:
    cmp [counter], 129 ; Checks if all the boxes were filled
    jne e ; if not continue
    jmp EndSelect ; else continue with the code 
    e:
        mov ax, 3
        int 33h
        shr cx,1
        cmp bl, 1
        jne check_keyboard
            cmp dx, 178
            jb check_keyboard
            cmp cx, 53
            ja _nr
            jmp red
            _nr:
            cmp cx, 108
            ja _np
            jmp purple
            _np:
            cmp cx, 160
            ja _ny
            jmp yellow
            _ny:
            cmp cx, 213
            ja _ng
            jmp green
            _ng:
            cmp cx, 267
            ja _nc
            jmp cyan
            _nc:
            jmp blue
        check_keyboard:
    mov ah,1
    int 16h ; BIOS input 
    jz waitfordata ; if there isn't any input jmp back
    ;read key
    in al,60h
    cmp al, 0Eh ; if Backspace remove block
    jne notrmv
    jmp delete
    notrmv:
    cmp al, 2 ; Checks if 1 (RED) was pressed
    je RED ; if 1 (RED) was pressed jmp to the segement of the code that takes care of the Red input
	cmp al, 3 ; Checks if 2 (purple) was pressed
    je purple ; if 2 (PURPLE) was pressed jmp to the segement of the code that takes care of the Purple input
    cmp al , 4 ; Checks if 3 (YELLOW) was pressed
    je Yellow ; if 3 (Yellow) was pressed jmp to the segement of the code that takes care of the Yellow input
    cmp al, 5 ; Checks if 4 (GREEN) was pressed
    jne d 
    jmp Green ; if 4 (GREEN) was pressed jmp to the segement of the code that takes care of the Green input
    d:
    cmp al, 6 ; Checks if 5 (CYAN) was pressed
    jne _b1
    jmp cyan ; if 5 (CYAN) was pressed jmp to the segement of the code that takes care of the Cyan input
    _b1:
    cmp al, 7 ; Checks if 6 (BLUE) was pressed
    jne waitfordata ; if its not 1 - 6 jmp back to get new input
    jmp Blue ; if 6 (BLUE) was pressed jmp to the segement of the code that takes care of the Blue input
    
    Red: 
        cmp [RF], 1 ; checks if it was pressed before
        jne ppap1
         jmp waitfordata ; if so it returns to search for a new color
        ppap1:
        mov [RF], 1 ; marks it as pressed
        mov al, [R] 
        mov [color], al ; turns the "color" variable into the red color constant
        shl [code], 3 
        add [code], 0 ; adds red to the "code" variable
        jmp DRAWRECTANGLE ; jmps to draw the new color in the "code" variable
    Purple: 
        cmp [PF], 1 ; checks if it was pressed before
        jne ppa
        jmp waitfordata ; if so it returns to search for a new color
        ppa:
        mov [PF], 1 ; marks it as pressed
        mov al, [P] ; turns the "color" variable into the purple color constant
        mov [color], al 
        shl [code], 3 
        add [code], 1 ; adds purple to the "code" variable
        jmp DRAWRECTANGLE ; jmps to draw the new color in the "code" variable
    Yellow: 
        cmp [YF], 1 ; checks if it was pressed before
        jne f
        jmp waitfordata ; if so it returns to search for a new color
        f:
        mov [YF], 1 ; marks it as pressed
        mov al, [Y]
        mov [color], al ; turns the "color" variable into the yellow color constant
        shl [code], 3
        add [code], 2 ; adds yellow to the "code" variable
        jmp DRAWRECTANGLE ; jmps to draw the new color in the "code" variable
    Green: 
        cmp [GF], 1 ; checks if it was pressed before
        jne a
        jmp waitfordata ; if so it returns to search for a new color
        a:
        mov [GF], 1 ; marks it as pressed
        mov al, [G]
        mov [color], al ; turns the "color" variable into the green color constant
        shl [code], 3
        add [code], 3 ; adds green to the "code" variable
        jmp DRAWRECTANGLE ; jmps to draw the new color in the "code" variable
    Cyan: 
        cmp [CF], 1 ; checks if it was pressed before
        jne _g1
        jmp waitfordata ; if so it returns to search for a new color
        _g1:
        mov [CF], 1 ; marks it as pressed
        mov al, [C]        
        mov [color], al ; turns the "color" variable into the cyan color constant
        shl [code], 3
        add [code], 4 ; adds cyan to the "code" variable
        jmp DRAWRECTANGLE ; jmps to draw the new color in the "code" variable
    Blue: 
        cmp [BF], 1 ; checks if it was pressed before
        jne continue0
        jmp waitfordata ; if so it returns to search for a new color
        continue0:
        mov [BF], 1 ; marks it as pressed
        mov al, [B] 
        mov [color], al ; turns the "color" variable into the blue color constant
        shl [code], 3
        add [code], 5 ; adds blue to the "code" variable
        jmp DRAWRECTANGLE
    goback:
        jmp waitfordata
    delete:
        cmp [counter], 1; checks if there is a box to delete
        je goback
            mov al, [def]
            mov [color], al ; sets the color to the default color (grey)

            sub [counter],32 ; goes back 32 pixel to the previous block

            mov bx, [code] 
            and bx, 111b ; gets the previous color
            shr [code],3 ; remove the previous color from the code
            cmp bx, 0 ; check what color it is so it turns the colors flag back to 0
            jne LFN1
                mov [RF], 0
            jmp done
            LFN1:
            cmp bx, 1
            jne LFN2
                mov [PF], 0
            jmp done
            LFN2:
            cmp bx, 2
            jne LFN3
                mov [YF], 0
            jmp done
            LFN3:
            cmp bx, 3
            jne LFN4
                mov [GF], 0
            jmp done
            LFN4:
            cmp bx, 4
            jne LFN5
                mov [CF], 0
            jmp done
            LFN5:
                mov [BF], 0
            done:
            
    DrawRectangle: ; draws the block based on the color pressed in the next block
        mov bx, [counter]
        mov [x1], bx
        mov [y1], 162
        mov [x2], bx
        add [x2], 31
        mov [y2], 181
        call DRAW_RECTANGLE

        cmp [color], 247 ; checks if the current action is bksp
        je dont
        add [counter],32 ; increase counter
        jmp waitfordata ; jump back
        dont: ; if it is backspace to increase the counter and wait for bksp up
            mov ah,1
            int 16h 
            jz dont
            in al,60h 
            cmp al, 8Eh
            jne dont
        jmp waitfordata ;jump back

    EndSelect:
    ;reverses the code because you get the code reversed
    mov ax,[code]
    and ax, 111b
    mov bx, ax ; puts the first digit of the 'code' variable into the three left bits of the bx register
    shl bx, 3 ; moves the whole bx register 3 bits to make place to the next digit

    mov ax, [code]
    shr ax, 3
    and ax, 111b
    add bx, ax ; puts the second digit of the 'code' variable into the three left bits of the bx register
    shl bx, 3 ; moves the whole bx register 3 bits to make place to the next digit

    mov ax, [code]
    shr ax, 6
    and ax, 111b
    add bx, ax ; puts the third digit of the 'code' variable into the three left bits of the bx register
    shl bx, 3 ; moves the whole bx register 3 bits to make place to the next digit
    
    mov ax, [code]
    shr ax, 9
    and ax, 111b
    add bx, ax ; puts the fourth digits of the 'code' variable into the three left bits of the bx register

    mov [code], bx ; and put the bx back into the code variable

    call guess ; gets the first guess of the bot
    call draw_bot_guess ; draws the first guess of the bot
        ; Enter the game loop
    
    GameLoop:
        cmp [easteregg], 3
        jae eastereggfound
        mov [easteregg], 0
        eastereggfound:
        ; Check the score of the last guess made
        push [lastguess]
        push [code]
        call play_result ; checks the result of the bot's guess
        mov [wscore],'b'
        call draw_score ; draws the score of the bot's guess
        ; If the code has been correctly guessed, end the program
        cmp [bulls], 4 ; checks if the bot won
        jne _p1 ; if not continue
            mov [mark], 2 ; if he won then you change the mark variable as a flag
        _p1:
        
        call KEEP_POSSIBLE_COMB ; Update the possible combinations left based on the score of the latest guess made
        
        ; player's turn
        ; set constants
        mov [counter], 161 ; the first pixel on the x axis
        mov [RF], 0 ; set red flag to 0 
        mov [PF], 0 ; set purple flag to 0
        mov [YF], 0; set yellow flag to 0
        mov [GF], 0 ; set green flag to 0
        mov [CF], 0 ; set cyan flag to 0
        mov [BF], 0 ; set blue flag to 0

        ; bot go brr so i stop him for about half a second
        uno:
            push cx
            mov cx, 0fh
            _1sec1:
                push cx
                mov cx, 0ffffh
                _2sec1:
                loop _2sec1
                pop cx
            loop _1sec1
            pop cx
        waitfordata1:
            cmp [counter], 289 ; Checks if all the boxes were filled
            jne e1 ; if not continue
            jmp EndSelect1 ; else don't continue with the code 
            e1:
            
        mov ax, 3
        int 33h
        shr cx,1
        cmp bl, 1
        ; checks if he pressed one of the colors 
        ; if so what color
        jne didntfindeasteregg
            cmp dx, 178
            jb check_keyboard1
            cmp cx, 53
            ja _nr1
            jmp red2 ; if red draw the new box red
            _nr1:
            cmp cx, 108
            ja _np1
            jmp purple2 ; if purple draw the next box purple
            _np1:
            cmp cx, 160
            ja _ny1
            jmp yellow2 ; if yellow draw the next box yellow
            _ny1:
            cmp cx, 213
            ja _ng1
            jmp green2 ; if green draw the next box green
            _ng1:
            cmp cx, 267
            ja _nc1
            jmp cyan2 ; if cyan draw the next box cyan
            _nc1:
            jmp blue2 ; else paint it blue
        check_keyboard1:
            ; checks if easteregg
            cmp dx, 20
            ja didntfindeasteregg
            cmp cx, 288
            jb didntfindeasteregg

            cmp [easteregg], 2
            jb addtoegg
            ja didntfindeasteregg
            inc [easteregg]
            push [botcode]
            call print_code
            jmp waitfordata1
            addtoegg:
                inc [easteregg]
                loopyegg:
                mov ax, 3
                int 33h
                cmp bl,0
                jne loopyegg
                jmp waitfordata1
                
            didntfindeasteregg:
            ; if there isn't any mouse input check keyboard
            mov ah,1
            int 16h ; BIOS input 
            jnz con12
            jmp waitfordata1 ; if there isn't any input jmp back
            CON12:
            ;read key
            in al,60h
            cmp al, 0Eh ; Checks if Bksp (delete) was pressed
            jne notrmv2
            jmp del1 ; if Bksp was pressed jmp to the segment of the code that takes care of the Bksp input
            notrmv2:
            cmp al, 2 ; Checks if 1 (RED) was pressed
            je RED2 ; if 1 (RED) was pressed jmp to the segement of the code that takes care of the Red input
            cmp al, 3 ; Checks if 2 (purple) was pressed
            je purple2; if 2 (PURPLE) was pressed jmp to the segement of the code that takes care of the Purple input
            cmp al , 4 ; Checks if 3 (YELLOW) was pressed
            je Yellow2; if 3 (Yellow) was pressed jmp to the segement of the code that takes care of the Yellow input
            cmp al, 5 ; Checks if 4 (GREEN) was pressed
            jne d2 
            jmp Green2 ; if 4 (GREEN) was pressed jmp to the segement of the code that takes care of the Green input
            d2:
            cmp al, 6 ; Checks if 5 (CYAN) was pressed
            jne b2
            jmp cyan2 ; if 5 (CYAN) was pressed jmp to the segement of the code that takes care of the Cyan input
            b2:
            cmp al, 7 ; Checks if 6 (BLUE) was pressed
            jne waitfordata1_
            jmp Blue2 ; if 6 (BLUE) was pressed jmp to the segement of the code that takes care of the Blue input
            waitfordata1_:
            jmp waitfordata1
            
            Red2: ; adds the new color to the code and turns the color variable to the red code number so it paints the block red if red wasnt pressed before
                cmp [RF], 1 ; checks if it was pressed before
                je waitfordata1_ ; if so it returns to search for a new color
                mov [RF], 1 ; marks it as pressed
                mov al, [R]
                mov [color], al ; turns the "color" variable into the red color constant
                shl [plastguess], 3
                add [plastguess], 0 ; adds red to the "code" variable
                jmp DRAWRECTANGLE1
            Purple2: ; adds the new color to the code and turns the color variable to the purple code number so it paints the block purple if purple wasnt pressed before
                cmp [PF], 1 ; checks if it was pressed before
                jne pap
                jmp waitfordata1 ; if so it returns to search for a new color
                pap:
                mov [PF], 1 ; marks it as pressed
                mov al, [P]
                mov [color], al ; turns the "color" variable into the red color constant
                shl [plastguess], 3
                add [plastguess], 1 ; adds purple to the "code" variable
                jmp DRAWRECTANGLE1
            Yellow2: ; adds the new color to the code and turns the color variable to the yellow code number so it paints the block yellow if yellow wasnt pressed before
                cmp [YF], 1 ; checks if it was pressed before
                jne f2
                jmp waitfordata1 ; if so it returns to search for a new color
                f2:
                mov [YF], 1 ; marks it as pressed
                mov al, [Y]
                mov [color], al ; turns the "color" variable into the red color constant
                shl [plastguess], 3
                add [plastguess], 2 ; adds yellow to the "code" variable
                jmp DRAWRECTANGLE1
            Green2: ; adds the new color to the code and turns the color variable to the green code number so it paints the block green if green wasnt pressed before
                cmp [GF], 1 ; checks if it was pressed before
                jne a1
                jmp waitfordata1 ; if so it returns to search for a new color
                a1:
                mov [GF], 1 ; marks it as pressed
                mov al, [G]
                mov [color], al ; turns the "color" variable into the red color constant
                shl [plastguess], 3
                add [plastguess], 3 ; adds green to the "code" variable
                jmp DRAWRECTANGLE1
            Cyan2: ; adds the new color to the code and turns the color variable to the cyan code number so it paints the block cyan if cyan wasnt pressed before
                cmp [CF], 1 ; checks if it was pressed before
                jne g1
                jmp waitfordata1 ; if so it returns to search for a new color
                g1:
                mov al, [C]        
                mov [color], al ; turns the "color" variable into the red color constant
                mov [CF], 1 ; marks it as pressed
                shl [plastguess], 3
                add [plastguess], 4 ; adds cyan to the "code" variable
                jmp DRAWRECTANGLE1
            Blue2: ; adds the new color to the code and turns the color variable to the blue code number so it paints the block blue if blue wasnt pressed before
                cmp [BF], 1 ; checks if it was pressed before
                jne continue01
                jmp waitfordata1 ; if so it returns to search for a new color
                continue01:
                mov [BF], 1 ; marks it as pressed
                mov al, [B]
                shl [plastguess], 3
                add [plastguess], 5 ; adds blue to the "code" variable
                mov [color], al ; turns the "color" variable into the red color constant
                jmp DRAWRECTANGLE1
            goback2:
            jmp waitfordata1
            del1:
                cmp [counter], 161 ; checks if it's the first so it doesn't delete a non existing box
                je goback2
            mov al, [def] ; sets the color to the default color (grey)
            mov [color], al

            sub [counter],32 ; subs 32 from counter so it paints the previous block and not the current

            mov bx, [plastguess]
            and bx, 111b ; checks which color is going to be deleted so it turns the flag false
            shr [plastguess],3            
            cmp bx, 0 ; if the color is red(0)
            jne LFN12
                mov [RF], 0 ; set the red flag to false(0)
            jmp done2
            LFN12:
            cmp bx, 1 ; if the color is purple(1)
            jne LFN22
                mov [PF], 0 ; set the purple flag to false(0)
            jmp done2
            LFN22:
            cmp bx, 2 ; if the color is yellow(2)
            jne LFN32
                mov [YF], 0 ; set the yellow flag to false(0)
            jmp done2
            LFN32:
            cmp bx, 3 ; if the color is green(3)
            jne LFN42
                mov [GF], 0 ; set the green flag to false(0)
            jmp done2
            LFN42:
            cmp bx, 4 ; if the color is cyan(4)
            jne LFN52
                mov [CF], 0 ; set the cyan flag to false(0)
            jmp done2
            LFN52:
                ; else the color is blue(5)
                mov [BF], 0 ; set the blue flag to false(0)
            done2:
            
            DrawRectangle1: ; draws the block based on the color pressed in the next block
                mov bx, [counter]
                mov [x1], bx
                mov ax, [ycounter]
                mov [y1], ax
                add bx, 31
                mov [x2], bx
                add ax, 19
                mov [y2], ax

                call DRAW_RECTANGLE

                cmp [color], 247 ; checks if it was delete
                je dont3
                add [counter],32 ; if not add 32 so it goes to the next block
                jmp waitfordata1 ; jmp back to the beginning of the loop
                dont3: ; else dont add 32 so it stays on the current block and wait for bksp to go up so it doesnt delete more blocks
                mov ah,1
                int 16h ; BIOS input 
                jz dont3
                in al,60h 
                cmp al, 8Eh
                jne dont3
                jmp waitfordata1 ; jmp back to the beginning of the loop

            EndSelect1:
            ; reverses the code because you get the code reversed
            mov ax,[plastguess]
            and ax, 111b
            mov bx, ax
            shl bx, 3

            mov ax, [plastguess]
            shr ax, 3
            and ax, 111b
            add bx, ax
            shl bx, 3

            mov ax, [plastguess]
            shr ax, 6
            and ax, 111b
            add bx, ax
            shl bx, 3
            
            mov ax, [plastguess]
            shr ax, 9
            and ax, 111b
            add bx, ax

            mov [plastguess], bx
        ; Make a new guess and draws it on the screen
        push [plastguess] 
        push [botcode]
        call play_result; checks the result of the player's guess 
        mov [wscore],'p'
        call draw_score ; draws the score of the guess of the player's guess
        cmp [bulls], 4 ; checks if the player won
        je byebye ; if so jmp to the end menu / who won the game
        cmp [mark], 2 ; checks if the bot won
        je byebye ; if so jmp to the end menu / who won the game
        call GUESS ; if no one won then the bots generate a new guess
        call draw_bot_guess ; draws the new bot's guess
        ; Loop back to the top of the game loop
        jmp GAMELOOP

    ; If the program is ending, print the bye string and exit
    byebye:
    mov [x_bot_block], 161
    jmp endthis
Train:
    mov ax, 2
    int 33h
    mov si, offset trainingbmp ; load the traing board bitmap
    call OPEN_FILE
    call READ_HEADER
    call READ_PALETTE
    call COPY_PAL
    call COPY_BMP

     
    ; generate population of all possible combinations
    call CREATE_POPULATION
    ; show mouse so there wont be the paint the cursor bug
    mov ax, 0
    int 33h
    mov ax, 1
    int 33h
    
    call guess ; Generate a random code for the player to guess
    mov ax, [lastguess]
    mov [botcode], ax ; Set the "botcode" variable to the genrated code
    mov [easteregg],0
    mov [ycounter], 22
    mov [counter], 83 ; Set the "counter" variable to the first pixel in the x axis
    ; Gets the guessed code from the user
    _waitfordata:
        cmp [counter], 211 ; Checks if all the boxes were filled
        jne _e ; if not continue
        jmp _ndSelect ; else continue with the code 
        _e:
        mov ax, 3
        int 33h
        shr cx,1
        cmp bl, 1
        ; checks if he pressed one of the colors 
        ; if so what color
        jne _didntfindeasteregg
            cmp dx, 178
            jb _check_keyboard
            cmp cx, 53
            ja nr
            jmp _red ; if red draw the new box red
            nr:
            cmp cx, 108
            ja np
            jmp _purple ; if purple draw the next box purple
            np:
            cmp cx, 160
            ja ny
            jmp _yellow ; if yellow draw the next box yellow
            ny:
            cmp cx, 213
            ja ng
            jmp _green ; if green draw the next box green
            ng:
            cmp cx, 267
            ja nc
            jmp _cyan ; if cyan draw the next box cyan
            nc:
            jmp _blue ; else paint it blue
        _check_keyboard:
            ; checks if easteregg
            cmp dx, 20
            ja _didntfindeasteregg
            cmp cx, 210
            jb _didntfindeasteregg
            cmp cx, 241
            ja _didntfindeasteregg
            cmp [easteregg], 2
            jb _addtoegg
            ja _didntfindeasteregg
            inc [easteregg]
            push [botcode]
            call print_code
            jmp _waitfordata
            _addtoegg:
                inc [easteregg]
                _loopyegg:
                mov ax, 3
                int 33h
                cmp bl,0
                jne _loopyegg
                jmp _waitfordata
                
            _didntfindeasteregg:
        ; if there isn't any mouse input check keyboard
        mov ah,1
        int 16h ; BIOS input 
        jnz dont1 
        jmp _waitfordata ; if there isn't any input jmp back
        dont1:
        ;read key
        in al,60h
        cmp al, 0Eh ; Checks if Bksp (delete) was pressed
        jne notrmv3
        jmp del2 ; if Bksp was pressed jmp to the segment of the code that takes care of the Bksp input
        notrmv3:
        cmp al, 2 ; Checks if 1 (RED) was pressed
        je _RED ; if 1 (RED) was pressed jmp to the segement of the code that takes care of the Red input
        cmp al, 3 ; Checks if 2 (purple) was pressed
        je _purple ; if 2 (PURPLE) was pressed jmp to the segement of the code that takes care of the Purple input
        cmp al , 4 ; Checks if 3 (YELLOW) was pressed
        je _Yellow ; if 3 (Yellow) was pressed jmp to the segement of the code that takes care of the Yellow input
        cmp al, 5 ; Checks if 4 (GREEN) was pressed
        jne _d 
        jmp _Green ; if 4 (GREEN) was pressed jmp to the segement of the code that takes care of the Green input
        _d:
        cmp al, 6 ; Checks if 5 (CYAN) was pressed
        jne _b
        jmp _cyan ; if 5 (CYAN) was pressed jmp to the segement of the code that takes care of the Cyan input
        _b:
        cmp al, 7 ; Checks if 6 (BLUE) was pressed
        jne waitforit ; if its not 1 - 6 jmp back to get new input
        jmp _Blue ; if 6 (BLUE) was pressed jmp to the segement of the code that takes care of the Blue input
        waitforit:
        jmp _waitfordata
        _Red: 
            cmp [RF], 1 ; checks if it was pressed before
            je waitforit ; if so it returns to search for a new color
            mov [RF], 1 ; marks it as pressed
            mov al, [R] 
            mov [color], al ; turns the "color" variable into the red color constant
            shl [code], 3 
            add [code], 0 ; adds red to the "code" variable
            jmp _DRAWRECTANGLE ; jmps to draw the new color in the "code" variable
        _Purple: 
            cmp [PF], 1 ; checks if it was pressed before
            jne ppap
            jmp _waitfordata ; if so it returns to search for a new color
            ppap:
            mov [PF], 1 ; marks it as pressed
            mov al, [P] ; turns the "color" variable into the purple color constant
            mov [color], al 
            shl [code], 3 
            add [code], 1 ; adds purple to the "code" variable
            jmp _DRAWRECTANGLE ; jmps to draw the new color in the "code" variable
        _Yellow: 
            cmp [YF], 1 ; checks if it was pressed before
            jne _f
            jmp _waitfordata ; if so it returns to search for a new color
            _f:
            mov [YF], 1 ; marks it as pressed
            mov al, [Y]
            mov [color], al ; turns the "color" variable into the yellow color constant
            shl [code], 3
            add [code], 2 ; adds yellow to the "code" variable
            jmp _DRAWRECTANGLE ; jmps to draw the new color in the "code" variable
        _Green: 
            cmp [GF], 1 ; checks if it was pressed before
            jne _a
            jmp _waitfordata ; if so it returns to search for a new color
            _a:
            mov [GF], 1 ; marks it as pressed
            mov al, [G]
            mov [color], al ; turns the "color" variable into the green color constant
            shl [code], 3
            add [code], 3 ; adds green to the "code" variable
            jmp _DRAWRECTANGLE ; jmps to draw the new color in the "code" variable
        _Cyan: 
            cmp [CF], 1 ; checks if it was pressed before
            jne _g
            jmp _waitfordata ; if so it returns to search for a new color
            _g:
            mov [CF], 1 ; marks it as pressed
            mov al, [C]        
            mov [color], al ; turns the "color" variable into the cyan color constant
            shl [code], 3
            add [code], 4 ; adds cyan to the "code" variable
            jmp _DRAWRECTANGLE ; jmps to draw the new color in the "code" variable
        _Blue: 
            cmp [BF], 1 ; checks if it was pressed before
            jne _continue0
            jmp _waitfordata ; if so it returns to search for a new color
            _continue0:
            mov [BF], 1 ; marks it as pressed
            mov al, [B] 
            mov [color], al ; turns the "color" variable into the blue color constant
            shl [code], 3
            add [code], 5 ; adds blue to the "code" variable
            jmp _drawrectangle
        _goback:
            jmp _waitfordata
        del2:
            cmp [counter], 83 ; checks if it's the first so it doesn't delete a non existing box
            je _goback
            mov al, [def] ; sets the color to the default color (grey)
            mov [color], al
            sub [counter], 32 ; subs 32 from counter so it paints the previous block and not the current
            mov bx, [code]
            and bx, 111b ; checks which color is going to be deleted so it turns the flag false
            shr [code], 3
            cmp bx, 0 ; if the color is red(0)
            jne _LFN1
                mov [RF], 0 ; set the red flag to false(0)
            jmp _done
            _LFN1:
            cmp bx, 1 ; if the color is purple(1)
            jne _LFN2
                mov [PF], 0 ; set the purple flag to false(0)
            jmp _done
            _LFN2:
            cmp bx, 2 ; if the color is yellow(2)
            jne _LFN3
                mov [YF], 0 ; set the yellow flag to false(0)
            jmp _done
            _LFN3:
            cmp bx, 3 ; if the color is green(3)
            jne _LFN4
                mov [GF], 0 ; set the green flag to false(0)
            jmp _done
            _LFN4:
            cmp bx, 4 ; if the color is cyan(4)
            jne _LFN5
                mov [CF], 0 ; set the cyan flag to false(0)
            jmp _done
            _LFN5: 
                ; else the color is blue(5)
                mov [BF], 0 ; set the blue flag to false(0)
            _done:
            
        _DrawRectangle: ; draws the block based on the color pressed in the next block
            mov bx, [counter]
            mov [x1], bx
            mov ax, [ycounter]
            mov [y1], ax
            mov [x2], bx
            add [x2], 31
            mov [y2], ax
            add [y2], 19
            
            call DRAW_RECTANGLE
            cmp [color], 247 ; checks if it was delete
            je dont4
                add [counter],32 ; if not add 32 so it goes to the next block
                jmp _waitfordata ; jmp back to the beginning of the loop
            dont4: ; else dont add 32 so it stays on the current block and wait for bksp to go up so it doesnt delete more blocks
            mov ah,1
            int 16h ; BIOS input 
            jz dont4
            in al,60h 
            cmp al, 8Eh
            jne dont4
            jmp _waitfordata ; jmp back to the beginning of the loop
        _ndselect:
        
                ;reverses the code because you get the code reversed
                mov ax,[code]
                and ax, 111b
                mov bx, ax
                shl bx, 3

                mov ax, [code]
                shr ax, 3
                and ax, 111b
                add bx, ax
                shl bx, 3

                mov ax, [code]
                shr ax, 6
                and ax, 111b
                add bx, ax
                shl bx, 3
                
                mov ax, [code]
                shr ax, 9
                and ax, 111b
                add bx, ax

                mov [code], bx
            push [code] 
            push [botcode]
            call play_result; checks the result of the player's guess 
            mov [wscore], 't'
            call draw_score ; draws the score of the guess of the player's guess
            ; set all flags to 0
            mov [RF], 0
            mov [PF], 0 
            mov [YF], 0
            mov [GF], 0
            mov [CF], 0
            mov [BF], 0 
            mov [counter], 83 ; reset the x to the default location
            _1uno:
                push cx
                mov cx, 0fh
                _1_1sec1:
                    push cx
                    mov cx, 0ffffh
                    _1_2sec1:
                    loop _1_2sec1
                    pop cx
                loop _1_1sec1
                pop cx

            cmp [bulls], 4 ; checks if the player won
                je continue_train
            cmp [ycounter], 180 ; checks if the user finished all his guesses
                ja continue_train
            jmp _waitfordata ; else jmp to the beginning of the loop
        continue_train:

    mov [x_bot_block], 83
    

endthis:
    mov cl, 0 ; number of shifts needed to get to the next digit in the combination
    _biggie1:
    push cx
    mov dx, [botcode]
    shr dx, cl
    and dx, 111b
    ; gets the next digit in the combination and checks what number it is to see what color to paint

    cmp dl, 0 ; if its 0 then paint red
    je _red11
    cmp dl, 1 ; if its 1 then paint purple
    je _purple11
    cmp dl, 2 ; if its 2 then paint yellow
    je _yellow11
    cmp dl, 3 ; if its 3 then paint green
    je _green11
    cmp dl, 4 ; if its 4 then paint cyan
    je _cyan11 
    jne _blue11 ; else paint blue
    
    _red11:
    mov al, [R] ; turns the color to red
    jmp _h1
    _purple11:
    mov al, [P] ; turns the color to purple
    jmp _h1
    _yellow11:
    mov al, [Y] ; turns the color to yellow
    jmp _h1
    _green11:
    mov al, [G] ; turns the color to green
    jmp _h1
    _cyan11:
    mov al, [C] ; turns the color to cyan
    jmp _h1
    _blue11:
    mov al, [B] ; turns the color to blue
    _h1:
    mov [color], al
    
    mov bx, [x_bot_block] 
    mov [x1], bx            ;moves the correct x1,x2 locations for the draw_rectangle procedure
    add bx, 31              
    mov [x2], bx

    mov bx, 2
    mov [y1], bx            ;moves the correct y1,y2 locations for the draw_rectangle procedure
    add bx, 19
    mov [y2], bx
    

    call DRAW_RECTANGLE

    pop cx
    add cl, 3 ; adds 3 to the shifts
    add [x_bot_block], 32 ; adds 32 so the next loop it will paint the next block
    cmp cl, 12 ; check if all the combination was painted
    je endthis2
    jmp _biggie1
    endthis2:
    cmp [bulls], 4  ; checks if the user won
    je YOUWON
    cmp [mark],8000h ; checks if the bot won
    jne BOTWON
    mov ax, 2
    int 33h
    mov si, offset UPBETTERBMP ; if no one won than show the better luck next time menu
    call OPEN_FILE
    call READ_HEADER
    call READ_PALETTE
    call COPY_PAL
    call COPY_POP_UP_BMP
    jmp _waitfordata2   ; and wait to see if the user returns to main menu or quits
    YOUWON:
        cmp [mark], 8000h   ; checks if there was a tie
        jne jtie            
        mov ax, 2
        int 33h
        mov si, offset UPPLYWONBMP ; if the user won then show the player won menu
        call OPEN_FILE
        call READ_HEADER
        call READ_PALETTE
        call COPY_PAL
        call COPY_POP_UP_BMP
        jmp _waitfordata2           ; and wait to see if the user returns to main menu or quits
    BOTWON:
        mov ax, 0
        int 33h
        mov si, offset upbotwonbmp  ; if the bot won then show the bot won menu
        call OPEN_FILE
        call READ_HEADER
        call READ_PALETTE
        call COPY_PAL
        call COPY_POP_UP_BMP
        jmp _waitfordata2           ; and wait to see if the user returns to main menu or quits
    jTIE:
        mov ax, 2
        int 33h
        mov si, offset uptiebmp ; if there was a tie then show the tie menu
        call OPEN_FILE
        call READ_HEADER
        call READ_PALETTE
        call COPY_PAL
        call COPY_POP_UP_BMP
        ; and wait to see if the user returns to main menu or quits
    _waitfordata2:
    mov ax, 1
    int 33h
    wfd2: ; check if he pressed left click on one of the buttons
    mov ax, 3
    int 33h
    shr cx,1
    cmp bl, 1
    jne check_keyboard2
    cmp cx, 127 
    jb check_keyboard2
    cmp cx, 225
    ja check_keyboard2
    cmp dx, 90
    jb check_keyboard2
    cmp dx,141
    ja check_keyboard2
    cmp dx, 114
    jb restart
    cmp dx, 117
    ja exit
    check_keyboard2: ; if he didnt press on any button check if he press space of esc to see if he wants to quit of go back to menu
        mov ah,1
        int 16h ; BIOS input 
        jz wfd2
        ;read key
        in al,60h
        cmp al, 1 
        jne __
            jmp exit
        __:
        cmp al, 39h
            jne wfd2
    restart:
    jmp start
    exit:
    mov ax, 2
    int 33h
    mov ah, 9
    mov dx, offset clear_scrn
    int 21h
    mov ax, 4c00h
    int 21h
END start