
;GameBoy Specific Routines -
; Lasted edit 7-Mar-99
;
;  The reason these routines are in a separate file
; is because they are specific to GameBoy. For development
; purposes, I used a CPM.INC file to test this basic
; interpreter on the Z80MU CP/M Emulator for dos.
;
; Copyright (c], 1996,1997,1998,1999 Jeff Frohwein
;
; V1.22 - Swapped command/button positions on intro screen.
;         Suggested by David Nathan.
;       - Fixed bug where SOUND < 256 duration would
;         play continuously. Reported by John Montalbano.
;       - Fixed error messages for DIM that were not
;         correct. Reported by Oliver Wittchow.
;       - Modified 'getbuts' routine to not affect
;         P14/P15 serial port.
;       - Entry of "2g.1" caused software lockup.
;         Reported by Marc Rawer.
; V2.00 - Added initial support for GameBoy Colo(u)r.
;       - Added SLOW & FAST to switch GBC CPU speeds.
;         [Defaults to SLOW on power up.]
; V2.10 - Added serial output support for MBC pins.

INCLUDE "src/hardware.inc"

BRKBTN  EQU $c0      ;front panel break button(s]
BSBTN   EQU $80      ;backspace button
CAPSBTN EQU $40      ;caps lock button
SELBTN  EQU $20      ;select button
ENTBTN  EQU $10      ;enter button

BS      EQU 8        ;backspace
SERVOTL EQU 211      ;Servo table length
TXIDLE  EQU $80      ;Serial idle tx character

;* External Keyboard Hardware Equates *

CAPSROW EQU $40
CAPSCOL EQU $0fe
SHFTROW EQU $10
SHFTCOL EQU $0fe

linkAdr EQU $bfff
stack   EQU $fffe   ;Put stack pointer to top of high ram
systype EQU $fe     ;Put the system type at $fffe
MBCOut  EQU $4000

SECTION "Low RAM Assignments", WRAM0[$c000]
windulx:: DB
winduly:: DB
windlrx:: DB
windlry:: DB
curx:: DB
cury:: DB
bright:: DB
keypadx:: DB
keypady:: DB
capsLock:: DB         ;holds caps lock state for onscreen menu  0=lc,1=uc]
extCapsLock:: DB      ;holds caps lock state for external keyboard
extShift:: DB         ;holds shift state for external keyboard
errflg:: DB           ;reports an error to calling routines
regaf:: DW
regbc:: DW
regde:: DW
reghl:: DW
tjmp:: DS 3           ;used to do a 'soft' jump
color:: DB
_d:: DW
i1:: DB
i2:: DB
sx:: DB
sy:: DB
x1:: DB
x2:: DB
xc:: DB
y1:: DB
y2:: DB
yc:: DB
vmode:: DB
rxchar:: DB             ; serial receive storage
txchar:: DB             ; serial transmit storage
linkUp:: DB             ; Serial link connection flag
linkUC:: DB             ; Serial link Up Count
linkDC:: DB             ; Serial link Down Count
servoTable:: DS SERVOTL ; Storage for servo routine
servoVals:: DS 8        ; Storage for servo values
servoTemps:: DS 8       ; Storage for servo routine
MBCPortState:: DB       ; Storage for MBC output port state
SerBaudRate:: DB        ; Holds serial output baud rate
SerPolarity:: DB        ; Holds serial output polarity

SECTION "High RAM Assignments", HRAM
; NOTE: IF YOU ACCESS THESE VARIABLES YOU MUST
; USE THE 'LDH' COMMAND.

divByTwo:: DB    ; Divide Interrupt clock by 2 for servo routine
tenMSC:: DW      ; Decremented every 10ms by interrupt timer routine
servoAddr:: DW   ; Storage for servo port memory address

NEG: MACRO
  cpl
  inc a
ENDM
;Macro for Z80 equivalent of EX (SP],HL

EX_SP_HL: MACRO
  push de
  di
  add sp,2
  pop de
  push hl
  ld l,e
  ld h,d
  add sp,-2
  ei
  pop de
ENDM

;Macro to wait until we can write to screen memory

HV_WAIT: MACRO
  ldh a,[rSTAT]
  and 2
  jr nz,@-4
ENDM

ld_long: MACRO
  IF STRLWR("\1") == "a"
    ; ld a, [$ff40]
    db $FA
    dw \2
  ELSE
    IF STRLWR("\2") == "a"
      ; ld [$ff40], a
      db $EA
      dw \1
    ENDC
  ENDC
ENDM

;#DEFINE  RGBSet(R,G,B) DW (((B>>3)<<10)+((G>>3)<<5)+(R>>3)]

;EX AF,BC C1F1C5F5 4 NOP 1

;        .addinstr EXCH AF,BC 010203040506 6 NOP 1

; This table MUST be located
; at org 0 !
SECTION "Pixel Mask", ROM0[$0]

PixelMask:
        DB $80,$40,$20,$10,$08,$04,$02,$01
;        exch af,bc

SECTION "Color", ROM0[$10]

;        RGBSet(255,0,255]

SECTION "Timer", ROM0[$50]             ; Timer Overflow Interrupt
        jp      Overflow

SECTION "Serial", ROM0[$58]             ; Serial Transfer Completion
        push    af

        xor     a
        ld      [linkDC],a      ; Reset link down count

        ldh     a,[1]
        cp      TXIDLE          ; Was an idle received?
        jr      z,stc1          ; yes, don't save it

        ld      [rxchar],a      ; Save received char

stc1:   call    LinkUpDetect

        ld      a,[txchar]      ; Start transfer again - Send txchar
        ldh     [1],a
        ld      a, SCF_TRANSFER_START | SCF_CLOCK_EXTERNAL
        ldh     [2],a

        ld      a,TXIDLE        ; Clear txchar
        ld      [txchar],a

        pop     af
        reti

; If we get 5 idle chars in a row
; then bring serial link up.

LinkUpDetect:
        cp      TXIDLE          ; Is it an idle char?
        jr      z,lud1          ; yes

        xor     a
        ld      [linkUC],a      ;clear link up count
        ret

lud1:   ld      a,[linkUC]
        inc     a
        ld      [linkUC],a
        cp      5               ; 5 consecutive idles?
        ret     nz              ; not yet

        ld      a,1
        ld      [linkUp],a      ; Bring up serial link
        ret

; If we get 10 timer interrupts and Serial Completion
; doesn't occur once then bring serial link down.

LinkDownDetect:
        ld      a,[linkDC]      ; Count number of timer interupts.
        inc     a               ; Serial Complete will zero this
        ld      [linkDC],a      ; number if external clock present.
        cp      10              ; 10 consecutive interrupts?
        ret     nz              ; not yet

        xor     a
        ld      [linkUp],a      ; Bring down serial link
        ret

SerialTransmit:
        push    af
        call    getLink
        dec     a               ; Is game link port set for Terminal mode?
        jr      nz,SerTx2       ; no, so don't attempt to send anything out
SerTx1:
        ld      a,[linkUp]
        or      a               ; Is serial link up?
        jr      z,SerTx2        ; no, don't try to send out char

        ld      a,[txchar]
        cp      TXIDLE          ; Is Tx buffer full?
        jr      nz, SerTx1      ; yes, wait

        pop     af
        ld      [txchar],a      ; Send char
        ret
SerTx2:
        pop     af
        ret

SECTION "Header", ROM0[$100]
;*** Beginning of rom execution point ***

        nop
        jp      begin

;Nintendo scrolling logo
;(Code won't work on a real GameBoy]
;(if next three lines are altered.]

DB $CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C,$00,$0D
DB $00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6,$DD,$DD,$D9,$99
DB $BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC,$99,$9F,$BB,$B9,$33,$3E

;Rom Header Info
pup:
;      12345678901xxxxx
DB "GBBasic2.10"             ; Cart name   11bytes
DB "JEFF"
DB $80                       ; Set GBC mode
DB 0,0,0                     ; Not used
DB 3                         ; Cart type   ROM+MBC1+RAM+Battery
DB 0                         ; ROM Size    32k
DB 2                         ; RAM Size     8k
DB $ff,$7e                   ; Maker ID    $7eff=Jeff Frohwein
DB 0                         ; Version     =0
DB $e2                       ; Complement check (important]
DW $c40e                     ; Checksum (not important]

SECTION "Entry Point", ROM0[$0150]
;Pointer to important address pointers
        DW   MemoryPointers

begin:
        ldh      [systype],a    ; Save the system type
        di
                  ; The stack initializes to $FFFE

        ld      a,1
        ld      [$6000],a ; Enable 4/32 mode on MBC1

;ser:
;        call    SerialOut
;
;        ld      a,0
;ser2:   dec     a
;        jr nz,ser2
;        jp ser

; Turn all servos off

        ld      bc,$0800  ;b = 8 : c = 0
servini:
        push    bc
        call    UpdateServoTable
        pop     bc
        dec     b
        jr      nz,servini

        xor    a         ;a = 0
        ldh    [rIF],a   ;clear pending interrupts

        call    getLink

        dec     a        ;Is link port set for external terminal?
        ld      a,4+8
        jr      z,intset ;yes, enable serial done interrupt
        ld      a,4
intset:
        ldh    [rIE],a  ;enable timer interrupt

; Set timer for 10mS Interrupt for Servo &
; Delay Refresh
;    Timer clock is 4096 Hertz.
;    To generate a 99.9Hz (10mS) interrupt
;    we have to divide the clock by 41.
;      4096 / 41 = 99.9Hz

        ld     a,256-41
        ldh    [rTIMA],a     ;set timer overflow count
        ld     a,256-41
        ldh    [rTMA],a     ;set timer refill count

        ld     a,TACF_START
        ldh    [rTAC],a     ;start timer

        xor    a               ; Misc standard init things..
        ldh    [rSCY],a         ; Screen scroll Y=0
        ldh    [rSCX],a         ; Screen scroll X=0
        ld     [vmode],a       ; set text mode as video mode
        ld     [capsLock],a    ; set caps lock to lowercase
        ld     [linkUp],a      ; serial link down
        ld     [linkUC],a      ; serial link up count
        ld     [linkDC],a      ; serial link down count
        ld     [MBCPortState],a
        ld     [SerPolarity],a ; Default polarity = idle low

        inc    a
        ld     [SerBaudRate],a ; Default baud rate = 2400

        ld      a,$80
        ldh     [divByTwo],a   ; Turn servo service off

        ld      a,TXIDLE         ; Clear serial rxchar & txchar
        ld      [rxchar],a
        ld      [txchar],a

        ei                     ; turn on interrupts

        call    getLink
        dec     a              ;Is link port set for external terminal?
        jr      nz,intset2     ;no

        ld      a,TXIDLE       ; initiate continuous serial transfer
        ldh     [rSB],a
        ld      a,SCF_TRANSFER_START | SCF_CLOCK_EXTERNAL          ; use external clock
        ldh     [rSC],a
intset2:

        call   def_color       ; set default color

        call   waitvbl         ; Must be in VBL before turning the screen off.

        ld     a, LCDCF_BG8000 | LCDCF_BGON     
                               ; LCD Controller = Off (No picture on screen]
                               ; WindowBank = $9800 (Not used]
                               ; Window = OFF
                               ; BG Chr = $8000
                               ; BG Bank= $9800
                               ; OBJ    = 8x8
                               ; OBJ    = Off
                               ; BG     = On
        ldh    [rLCDC],a

        call   SetAllCGB_BGP   ; Set GBC background palette

        call   nor_col         ; Normal palette
        call   move_char       ; Move the charset to $8000
        call   move_text       ; Move the text to $9800

        ld     a,LCDCF_ON | LCDCF_BG8000 | LCDCF_BGON ; LCD Controller = On
        ldh    [rLCDC],a

        ld      b,1     ;set text window for whole screen
        ld      c,1
        ld      d,20
        ld      e,18
        call    window

;pause until Start pressed

        ld      hl,400
pdx:    call    getbuts;TODO: convert to format expected in hardware.inc
        and     $80             ;start pressed?
        jr      nz,pdx1         ;yes

        ld      de,1
        call    dely1

        dec     hl
        ld      a,h
        or      l               ;has 10 second delay run out?
        jr      nz,pdx          ;no, loop until it does

pdx1:   call    cls             ;clear the screen

        jp      start

; *** RS232 Serial Timing Fine Tuning Table ***
SECTION "Serial Timing Table", ROM0[$200]
        nop
        nop
        nop
        ret

Rs232Timing:
        jp      hl

; ***
; *** Baud Rate Data Table ***
; 1xcourse,1xfine,2xcourse,2xfine

BaudRateTable:
        DB   212,0,212,0     ;  1200 [874= 1200/NA]
        DB   103,1,212,0     ;  2400 [437= 2400/874= 2400]
        DB   48,0,103,1      ;  4800 [218= 4809/437= 4799]
        DB   21,1,48,0       ;  9600 [109= 9620/218= 9619]
        DB   12,1,30,0       ; 14400 [ 73=14364/146=14364]
        DB   8,3,21,1        ; 19200 [ 55=19065/109=19240]
        DB   1,3,8,3         ; 38400 [ 27=38814/ 55=38130]
        DB   1,3,3,2         ; 57600 [   NA    / 36=58254]
; timing = 27 + (course-1)*4 + 3-fine.

; "Sinit"
;    Sinit BAUDRATE,DATA
;
; BAUDRATE
;  0 = 1200 (Only available in SPEED 0 mode]
;  1 = 2400
;  2 = 4800
;  3 = 9600
;  4 = 14400
;  5 = 19200
;  6 = 38400
;  7 = 57600 (Only available in SPEED 1 mode]
; DATA
;  bit 0 - 0 = Serial idle is low, 1 = Serial idle is high

SerInit:
        call    exprb   ;Get baud rate
        call    pfix

        push    de

        ld      b,","
        call    eatc

        call    exprb   ;Get data
        call    pfix

        ld      a,e
        ld      [MBCPortState],a
        ld      [MBCOut],a
        ld      [SerPolarity],a

        pop     de
        ld      a,e
        ld      [SerBaudRate],a
        ret

; "Sout"
;    Sout DATA

SerOut:
        call    exprb   ;Get data
        call    pfix

        push    de

        ld      a,[MBCPortState]
        or      1
        ld      e,a
        and     $fe
        ld      d,a     ; Setup for Idle = low

        ld      a,[SerPolarity]
        and     1        ; Idle is low?
        jr      z,serlow ; yes

        ld      a,d
        ld      d,e
        ld      e,a      ; Swap b & c
serlow:
        ld      a,[SerBaudRate]
        add     a,a      ; x2
        add     a,a      ; x4
        ld      l,a
        ld      h,0

        ldh     a,[systype]
        cp      $11             ; Is this a GBC?
        jr      nz,serngbc      ; no

        ldh     a,[rKEY1]
        and     KEY1F_DBLSPEED  ; Is GBC in double speed mode?
        jr      z,serngbc       ; no

        inc     l
        inc     l
serngbc:
        ld      bc,BaudRateTable
        add     hl,bc
        ld      a,[hli]         ; get course timing
        ld      c,a
        ld      a,[hl]          ; get fine timing

        pop     hl              ; get data
        ld      b,l             ; b = data
        ld      l,a             ; l = fine timing

        di
        call    SerialOut
        ei
        ret

; b = data out
; c = course timing
; d = bit 1
; e = bit 0
; h = --
; l = fine timing

; *** Output a serial byte ***
; 8 bits, 1 stop bit, lsb first

SerialOut:
;        ld      l,3
;        ld      b,55h
;        ld      c,1
;        ld      d,1
;        ld      e,0
;Set low for start bit
        ld      a,e
        ld      [MBCOut],a
        ld      h,2
        nop
        ld      a,c
SerialLoop0:
        dec     a
        jr      nz,SerialLoop0
        call    Rs232Timing
;----
        rr      b           ; get next serial bit
        jr      c,SerialSet1
        ld      a,e
        ld      [MBCOut],a
        jr      SerialBit1
SerialSet1:
        ld      a,d
        ld      [MBCOut],a
        nop
        nop
SerialBit1:
        ld      a,c
SerialLoop1:
        dec     a
        jr      nz,SerialLoop1
        call    Rs232Timing
;----
        rr      b           ; get next serial bit
        jr      c,SerialSet2
        ld      a,e
        ld      [MBCOut],a
        jr      SerialBit2
SerialSet2:
        ld      a,d
        ld      [MBCOut],a
        nop
        nop
SerialBit2:
        ld      a,c
SerialLoop2:
        dec     a
        jr      nz,SerialLoop2
        call    Rs232Timing
;----
        rr      b           ; get next serial bit
        jr      c,SerialSet3
        ld      a,e
        ld      [MBCOut],a
        jr      SerialBit3
SerialSet3:
        ld      a,d
        ld      [MBCOut],a
        nop
        nop
SerialBit3:
        ld      a,c
SerialLoop3:
        dec     a
        jr      nz,SerialLoop3
        call    Rs232Timing
;----
        rr      b           ; get next serial bit
        jr      c,SerialSet4
        ld      a,e
        ld      [MBCOut],a
        jr      SerialBit4
SerialSet4:
        ld      a,d
        ld      [MBCOut],a
        nop
        nop
SerialBit4:
        ld      a,c
SerialLoop4:
        dec     a
        jr      nz,SerialLoop4
        call    Rs232Timing
;----
        rr      b           ; get next serial bit
        jr      c,SerialSet5
        ld      a,e
        ld      [MBCOut],a
        jr      SerialBit5
SerialSet5:
        ld      a,d
        ld      [MBCOut],a
        nop
        nop
SerialBit5:
        ld      a,c
SerialLoop5:
        dec     a
        jr      nz,SerialLoop5
        call    Rs232Timing
;----
        rr      b           ; get next serial bit
        jr      c,SerialSet6
        ld      a,e
        ld      [MBCOut],a
        jr      SerialBit6
SerialSet6:
        ld      a,d
        ld      [MBCOut],a
        nop
        nop
SerialBit6:
        ld      a,c
SerialLoop6:
        dec     a
        jr      nz,SerialLoop6
        call    Rs232Timing
;----
        rr      b           ; get next serial bit
        jr      c,SerialSet7
        ld      a,e
        ld      [MBCOut],a
        jr      SerialBit7
SerialSet7:
        ld      a,d
        ld      [MBCOut],a
        nop
        nop
SerialBit7:
        ld      a,c
SerialLoop7:
        dec     a
        jr      nz,SerialLoop7
        call    Rs232Timing
;----
        rr      b           ; get next serial bit
        jr      c,SerialSet8
        ld      a,e
        ld      [MBCOut],a
        jr      SerialBit8
SerialSet8:
        ld      a,d
        ld      [MBCOut],a
        nop
        nop
SerialBit8:
        ld      a,c
SerialLoop8:
        dec     a
        jr      nz,SerialLoop8
        call    Rs232Timing
;---- STOP BIT
        scf
        nop
        jr      c,SerialSet9
SerialSet9:
        ld      a,d
        ld      [MBCOut],a
        nop
        nop
SerialBit9:
        ld      a,c
SerialLoop9:
        dec     a
        jr      nz,SerialLoop9
        call    Rs232Timing
;----
        ret

;
;    interpreter driver
;

iloop:    call    istat         ;interpret current statement
          call    joe         ;test for junk on end
        jr    c,iloopx     ;stop if end of program
        call  getbuts      ;break button pressed?
        and   BRKBTN
        cp    BRKBTN
        jr    nz,iloop     ;no
iloopb:
        call  text_mode
        jp    stop1         ;execute end statement
iloopx:
        call  text_mode
        jp    bend

;Update servo table to reflect the current
;values of the servo settings
;Entry:
; B = Servo Number (1-8]
; C = Servo Value (0-SERVOTL-1]

UpdateServoTable:
        ld      hl,servoVals    ;Record servo value
        ld      d,0
        ld      e,b
        add     hl,de
        dec     hl
        ld      [hl],c

        ld      a,$80           ;B = bitmask for servo
UpSrv1: rlc     a
        dec     b
        jr      nz,UpSrv1
        ld      b,a

        ld      hl,servoTable
        ld      d,SERVOTL
        inc     c               ;Increment value to make
                                ;zero test easier
UpSrv2: ld      a,[hl]
        or      b               ;Set bit B
        dec     c               ;Is servo value zero?
        jr      nz,UpSrv3       ;No
        inc     c
        xor     b               ;Reset bit B
UpSrv3: ldi     [hl],a

        dec     d               ;Have we done whole table?
        jr      nz,UpSrv2       ;not yet
        ret

;Service Timer Overflow Interrupt
; (Occurs ~100 times a second]
Overflow:
        push    af
        push    hl

        ldh     a,[tenMSC]     ;Decrement 10ms Counter
        ld      l,a
        ldh     a,[tenMSC+1]
        ld      h,a
        dec     hl
        ld      a,l
        ldh     [tenMSC],a
        ld      a,h
        ldh     [tenMSC+1],a

        call    LinkDownDetect  ;Take down serial link if no external clock

        ldh     a,[divByTwo]
        xor     1               ;toggle lsb
        ldh     [divByTwo],a
        jr      nz,oflow13      ;This is the wrong toggle state OR
                                ;other bits are set disabling servos.
; Occurs ~50 times a second (20ms]

        push    de
        push    bc

; Load reg C with Initial value for servo port

        ld      b,8
        ld      hl,servoVals
oflow1:
        ld      a,[hl]
        inc     hl
        cp      1               ;Set carry if a=0 (servo should be off]
        ccf                     ;invert carry flag
        rr      c
        dec     b               ;are we done?
        jr      nz,oflow1       ;not yet

; Output reg C to servo port

        ldh     a,[servoAddr]
        ld      e,a
        ldh     a,[servoAddr+1]
        ld      d,a             ;de = Servo port address

        ld      a,c             ;Send a high pulse to
        ld      [de],a          ;servos that should be on

        ld      b,180           ; 1.9uS   - 179 * 3.8uS = 693uS
oflow11:
        dec     b               ;4  |    16 cpu cycles
        jr      nz,oflow11      ;12 |  1/(4.194MHz/16) = 3.8uS

        ld      hl,servoTable   ; 3.8uS
        ld      b,SERVOTL       ; 1.9uS
oflow12:
        ld      a,[hli]         ;8  --+
        ld      [de],a          ;8    |--- 32 cpu cycles
        dec     b               ;4    |   1/(4.194MHz/32) = 7.6uS
        jr      nz,oflow12      ;12 --+

        pop     bc              ; 2.9uS

        xor     a               ; 0.95uS  Make sure all high pulses
        ld      [de],a          ; 1.9uS   for the servos are off

        pop     de
oflow13:
        pop     hl
        pop     af
        reti

abc:
        ld      a,1
        ld      [linkUp],a

        ret

; * Default drawing color *
;    called by run

def_color:
        ld      a,4
        ld      [color],a
        ret

; "Link"
; Entry: 0 = No Link port use
;        1 = Terminal Interface
;        2 = External keyboard
setLink:
        ld      a,CART_SRAM_ENABLE
        ld      [rRAMG],a           ;enable sram

        call    exprb   ;get link port state
        call    pfix
        ld      a,e
        ld      [linkAdr],a

        xor     a;CART_SRAM_DISABLE
        ld      [rRAMG],a
        ret

; Get game link port setting
getLink:
        ld      a,CART_SRAM_ENABLE
        ld      [rRAMG],a           ;enable sram

        ld      a,[linkAdr]

        push    af
        xor     a;CART_SRAM_DISABLE
        ld      [rRAMG],a
        pop     af

        ret

;
; "Color"
; Set drawing color

set_color:
        call    exprb   ;get color
        call    pfix
        ld      a,e
        ld      [color],a
        ret

;
; "Servo"
;    Controls the position of up to 8 memory-mapped
;    servos. SERVO 0,x defines the memory address of
;    the servos output port. SERVO x,y outputs a high
;    pulse to servo x (1-8) for 693+(y*7.63) microseconds
;    giving a pulse range of .7 to 2.3 ms. This pulse
;    is sent out every 20 ms.

servo:
        call    exprb   ;Get servo number
        call    pfix

        ld      b,e
        push    bc

        ld      b,","
        call    eatc

        call    exprb   ;Get servo value
        call    pfix

        pop     bc
        ld      a,b
        or      a               ;Is this a servo initialize?
        jr      z,servo1        ;yes

        ld      c,e
        di                       ;Prevent servos from using bad servo data
        call    UpdateServoTable ;by disabling interrupts during servo
        ei                       ;table update.
        ret

servo1:
        di                      ;Prevent interrupts from occuring
        ld      a,e             ;during servo port address change.
        ldh     [servoAddr],a
        ld      a,d
        ldh     [servoAddr+1],a

        or      e               ;Does address = 0 ?

        jr      z,servo2        ;yes, disable servo pulses

        ld      a,$80           ;enable servo updates
servo2:
        xor     $80
        ldh     [divByTwo],a
        ei
        ret

; "Speed"
;   Speed 0 = Single speed
;   Speed 1 = Double speed

SetSpeed:
        call    exprb   ;Get speed setting
        call    pfix
        ld      a,e
        or      a
        jr      nz,SetDoubleSpeed

; "Slow"
; Set slow CPU mode on the GBC.

SetSingleSpeed:
        ldh     a,[rKEY1]
        and     KEY1F_DBLSPEED  ; Is GBC in double speed mode?
        ret     z               ; no, already in single speed


SetSpeed2:
        di

        ldh     a,[rIE]
        push    af

        xor     a               ; a = 0
        ldh     [rIE],a         ;disable interrupts
        ldh     [rIF],a

        ld      a,P1F_GET_NONE
        ldh     [rP1],a

        ld      a,KEY1F_PREPARE
        ldh     [rKEY1],a

        stop

        pop     af
        ldh     [rIE],a

        ret

; "Fast"
; Set fast CPU mode on the GBC.

SetDoubleSpeed:
        ldh     a,[rKEY1]
        and     KEY1F_DBLSPEED  ; Is GBC in double speed mode?
        ret     nz              ; yes, exit
        jr      SetSpeed2

;
; "Point"
; Put a pixel to (b,c) with (color]
;  optimized by Jens Christian Restemeier

draw_point:
        call    exprb   ;get x coordinate
        call    pfix
        ld      b,e
        push    bc

        ld      b,","
        call    eatc

        call    exprb   ;get y coordinate
        call    pfix

        pop     bc
        ld      c,e

point:
        ld      a,c             ; hl = 8000h + y*2 + (x/8)*256
        rlc     a
        ld      l,a
        ld      a,b
        srl     a
        srl     a
        srl     a
        add     a,$80
        ld      h,a
        ld      a,b             ; b = Bitmask[b & 7]
        and     7
        ld      e,a
        ld      d,PixelMask/256
        ld      a,[de]
        ld      b,a
        ld      d,a
        cpl
        ld      c,a
        ld      e,a

        ld      a,[color]
        or      a               ;color 0?
        jr      nz,point1       ;no

        ld      c,$ff
        ld      e,$ff
        jr      point8

point1:
        dec     a               ;color 1?
        jr      nz,point2

        ld      b,0
        ld      e,$ff
        jr      point8

point2:
        dec     a               ;color 2?
        jr      nz,point3

        ld      c,$ff
        ld      d,0
        jr      point8

point3:
        dec     a               ;color 3?
        jr      nz,point4

        ld      b,0
        ld      d,0
        jr      point8

;must be a xor color

point4:  di
         HV_WAIT

         ld      a,[hl]
         xor     b
         ld      [hli],a
         ei

         di
         HV_WAIT

         ld      a,[hl]
         xor     b
         ld      [hl],a
         ei
         ret


point8:  di
         HV_WAIT

         ld      a,[hl]
         or      b
         and     c
         ld      [hli],a
         ei

point9:  di
         HV_WAIT

         ld      a,[hl]
         or      d
         and     e
         ld      [hl],a
         ei
         ret

; Draw a line from (X1],[Y1) to (X2],[Y2) with (color]
;  optimized by Jens Christian Restemeier

draw_line:
        call    exprb   ;get x1 coordinate
        call    pfix
        ld      a,e
        ld      [x1],a

        ld      b,","
        call    eatc

        call    exprb   ;get y1 coordinate
        call    pfix
        ld      a,e
        ld      [y1],a

        ld      b,","
        call    eatc

        call    exprb   ;get x2 coordinate
        call    pfix
        ld      a,e
        ld      [x2],a

        ld      b,","
        call    eatc

        call    exprb   ;get y2 coordinate
        call    pfix
        ld      a,e
        ld      [y2],a

drw_lin:
        xor     a
        ld      [_d],a
        ld      [_d+1],a

        ld      a,[x2]
        ld      b,a
        ld      a,[x1]
        ld      [xc],a
        sub     b
        ld      b,0
        or      a
        jr      z,l_px
        ld      b,$ff
        bit     7,a
        jr      z,l_px
        NEG
        ld      b,1
l_px:        ld        d,a        ; dx sichern
        sla     a
        ld      [i2],a    ; i2=dx*2

        ld      a,b
        ld      [sx],a
        ld      a,[y2]
        ld      b,a
        ld      a,[y1]
        ld      [yc],a
        sub     b
        ld      b,0
        jr      z,l_py
        ld      b,$ff
        bit     7,a
        jr      z,l_py
        NEG
        ld      b,1
l_py:        ld        e,a             ; dy sichern
        sla     a
        ld      [i1],a    ; i1=dy*2
        ld      a,b
        ld      [sy],a
        ld      a,d
        cp      e               ; dx<dy ?
        jr      c,dloop2

; dx>dy
dloop:  ld      a,[xc]    ; if ((x1==x2)&&(y1==y2)) return;
        ld      b,a
        ld      a,[yc]
        ld      c,a
        call    point

        ld      a,[xc]
        ld      b,a
        ld      a,[x2]
        cp      b
        jr      nz,dnext

        ld      a,[yc]
        ld      b,a
        ld      a,[y2]
        cp      b
        jr      nz,dnext
        ret

dnext:  ld  a,[_d]
        ld  e,a
        ld  a,[_d+1]
        ld  d,a
        bit 7,d
        jr  nz,xxp

        ld  a,[sy]    ; yc+=sy
        ld  b,a
        ld  a,[yc]
        add a,b
        ld  [yc],a

        ld  a,[i2]    ; d-=i2
        ld  b,a
        ld  a,e
        sub b
        ld  e,a
        jr  nc,jnc1
        dec d
jnc1:
xxp:        ld    a,[sx]        ; xc+=sx
        ld  b,a
        ld  a,[xc]
        add a,b
        ld  [xc],a

        ld  a,[i1]    ; d+=i1
        ld  b,a
        ld  a,e
        add a,b
        ld  [_d],a
        jr  nc,jnc2
        inc d
jnc2:        ld    a,d
        ld  [_d+1],a
        jr  dloop

; dy>dx
dloop2: ld      a,[xc]    ; if ((x1==x2)&&(y1==y2)) return;
        ld      b,a
        ld      a,[yc]
        ld      c,a
        call    point
        ld      a,[xc]
        ld      b,a
        ld      a,[x2]
        cp      b
        jr      nz,dnext2
        ld      a,[yc]
        ld      b,a
        ld      a,[y2]
        cp  b
        jr  nz,dnext2
        ret

dnext2: ld  a,[_d]
        ld  e,a
        ld  a,[_d+1]
        ld  d,a
        bit 7,d
        jr  nz,yyp
        ld  a,[sx]    ; xc+=sy
        ld  b,a
        ld  a,[xc]
        add a,b
        ld  [xc],a

        ld  a,[i1]    ; d-=i2
        ld  b,a
        ld  a,e
        sub b
        ld  e,a
        jr  nc,jnc12
        dec d
jnc12:

yyp:        ld    a,[sy]        ; yc+=sy
        ld  b,a
        ld  a,[yc]
        add a,b
        ld  [yc],a

        ld  a,[i2]    ; d+=i1
        ld  b,a
        ld  a,e
        add a,b
        ld  [_d],a
        jr  nc,jnc22
        inc d
jnc22:  ld  a,d
        ld  [_d+1],a
        jr  dloop2




; Clear screen

clsgr:  ld      hl,_VRAM
        ld      d,0
        ld      e,16            ; x2=4096
clsgr2: xor     a               ; A = 0
        ld      [hli],a
        dec     d
        jr      nz,clsgr2
        dec     e
        jr      nz,clsgr2
        ret

; Switch to text mode
; If already there than do nothing.

text_mode:
        ld      a,[vmode]
        dec     a               ;are we in graphics mode?
        jr      z,txtmode       ;yes, set to text mode

        ret

; Activate Auto load & run
; of a program on reset or powerup.

auto:   ld      a,CART_SRAM_ENABLE  ;Enable ram bank
        ld      [0],a

        ld      a,[$a006]      ;enable auto run mode
        or      $80
        ld      [$a006],a

        xor     a               ;Disable ram bank
        ld      [0],a
        ret

; Initialize screen

screen:
        call    exprb           ;get video mode
        call    pfix

        ld      a,e

txtmode:
        ld      [vmode],a       ;save video mode

        or      a               ;is it text mode?
        jr      nz,screen1      ;no
clstxt:
        call   waitvbl         ; Must be in VBL before turning the screen off.

        ld     a, LCDCF_BG8000 | LCDCF_BGON
                               ; LCD Controller = Off (No picture on screen]
                               ; WindowBank = $9800 (Not used]
                               ; Window = OFF
                               ; BG Chr = $8000
                               ; BG Bank= $9800
                               ; OBJ    = 8x8
                               ; OBJ    = Off
                               ; BG     = On
        ldh    [rLCDC],a

        call   nor_col         ; Normal palette
        call   move_char       ; Move the charset to $8000
        call   move_noth       ; Move the text to $9800

        ld     a, LCDCF_ON | LCDCF_BG8000 | LCDCF_BGON     ; LCD Controller = On
        ldh    [rLCDC],a

        call    initkpd

        ld      bc,11
        call    locate

        jp      highlight

screen1:
        dec     a               ;is it graphics mode?
        jp      nz,e1           ;no, syntax error

        call    waitvbl         ; wait for vertical blank before
                                ; turning screen off

        ld      a, LCDCF_BG8000 | LCDCF_BGON
                                ; LCD Controller = Off (No picture on screen]
                                ; WindowBank = $9800 (Not used]
                                ; Window = OFF
                                ; BG Chr = $8000
                                ; BG Bank= $9800
                                ; OBJ    = 8x8
                                ; OBJ    = Off
                                ; BG     = On
        ldh     [rLCDC],a

        call    clsgr           ;clear graphics screen

initscr:
        ld      hl,_SCRN0        ; First clear the screen
        ld      e,4
cloop1: ld      d,0
cloop2: di
        HV_WAIT
        ld      a,$ff          ; This char ($ff) isn't used for GFX.
        ld      [hli],a
        ei

        dec     d
        jr      nz,cloop2
        dec     e
        jr      nz,cloop1
        ld      b,0
        ld      hl,$9822        ; Now draw the 16*15 matrix
        ld      e,$f
loop1:  ld      d,$10
loop2:  di
        HV_WAIT
        ld      a,b
        swap    a               ; The screen must be rotated by 90�.
        ld      [hli],a
        ei

        inc     b
        dec     d
        jr      nz,loop2
        push    de
        ld      d,0
        ld      e,$10
        add     hl,de
        pop     de
        dec     e
        jr      nz,loop1

        ld      a,LCDCF_ON | LCDCF_BG8000 | LCDCF_BGON ; LCD Controller = On
        ldh     [rLCDC],a

        ret

; "Delay"
delay:
        call    exprb   ;get value
        call    pfix

dely1:  ld      a,e             ; load up interrupt counter
        ldh     [tenMSC],a
        ld      a,d
        ldh     [tenMSC+1],a

dely2:  ld      c,13
        call    getbuts         ; Break button pressed?
        and     BRKBTN
        cp      BRKBTN
        jr      z,dely4         ; Yes, exit

        ldh     a,[tenMSC]
        ld      b,a
        ldh     a,[tenMSC+1]
        or      b               ; is interrupt counter done?
        jr      nz,dely2        ; no
dely4:  ret

;* Get a Key from Input device & keep most register values *

inchar:
        push  bc
        push  de
        push  hl
        call  getchns
        pop hl
        pop de
        pop bc
        ld      b,a
        ret

;* Get a Key from Input device *

getchns:
getch0:
        call    getbuts
        ld      b,a
        and     SELBTN          ;is select pressed?
        jp      nz,padchar          ;yes, return what keypad's pointing to

        ld      a,b
        and     BSBTN           ;is backspace pressed?
        ld      a,BS
        jp      nz,keyup  ;yes, backspace

        ld      a,b
        and     ENTBTN          ;is enter pressed?
        ld      a,cr
        jp      nz,keyup  ;yes

        ld      a,b
        and     $f
        add     a,$f8    ;is down pressed?
        jr      nc,getch3 ;no

        ld      a,[keypady]
        cp      3               ;Are we already at right column?
        jr      z,getch00       ;yes

        inc     a
        ld      [keypady],a
        jr      getchup

getch00:
        ld      a,1
        ld      [keypady],a
        jr      getchup

getch3:
        add     a,4             ;is up pressed?
        jr      nc,getch5       ;no

        ld      a,[keypady]
        cp      1               ;Are we already at top row?
        jr      z,getch31       ;yes

        dec     a
        ld      [keypady],a

        jr      getchup
getch31:
        ld      a,3
        ld      [keypady],a
        jr      getchup

getch5:
        add     a,2             ;is left pressed?
        jr      nc,getch7       ;no

        ld      a,[keypadx]
        cp      1               ;Are we already at left column?
        jr      z,getch6        ;yes

        dec     a
        ld     [keypadx],a

        jr      getchup
getch6:
        ld      a,20
        jr      getch9

getch7:
        add     a,1             ;is right pressed?
        jr      nc,getch10      ;no

        ld      a,[keypadx]
        cp      20              ;Are we already at right column?
        jr      z,getch8        ;yes

        inc     a
        ld      [keypadx],a
        jr      getchup

getch8: ld      a,1
getch9: ld     [keypadx],a
        jr      getchup

getch10:
        ld      a,b
        and     CAPSBTN         ;is caps lock pressed?
        jp      z,extkey        ;no

        ld      a,[capsLock]    ;Toggle caps lock
        xor     1
        ld      [capsLock],a
getchup:
        call    DrawMenu
        call    keyup
        jp      getch0

padchar:
        call    keyup
        jr      xy2char

;* Convert B,C keypad character to actual value *

xy2char:
        ld      de,menuLC
        ld      a,[capsLock]
        or      a               ;Is lowercase selected?
        jr      z,padchar0      ;yes
        ld      de,menuUC

padchar0:
    ld    c,1
padchar1:
    ld    b,1
padchar2:
    ld    a,[keypadx]
    cp    b
    jr    nz,padchar9
    ld    a,[keypady]
    cp    c
    jr    nz,padchar9
    ld    a,[de]
    cp    10        ;was space pressed?
    jr    nz,padchar8    ;no
    ld    a," "
padchar8:
    ret
padchar9:
    inc    de
    inc    b
    ld    a,21
    cp    b
    jr    nz,padchar2
    inc    c
    jr    padchar1

; * Wait until all buttons have been
;   released and debounced *
keyup:
    push    af
keyup1:
    ld    b,255
keyup2:
    push    bc
    call    getbuts
    pop    bc
    or    a        ;have all buttons been released?
    jr    nz,keyup1    ;not yet

    dec    b
    jr    nz,keyup2
    pop    af
    ret

; x = keypad(x]
;  return value of keypad to user

akeypad:
        call    pfix            ;pop argument
        ld      a,e
        or      d               ;does arg=0?
        jr      z,akeyp0        ;yes

        call    getbuts
        and     e               ;is select bit(s) zero?
        jp      z,acal2         ;yes
        ld      a,1
        jp      acal2

akeyp0: call    getbuts
        jp      acal2

; * Get Keypad Button Status *
;  The following bits are set if pressed.
;   $80 - Start   $8 - Down
;   $40 - Select  $4 - Up
;   $20 - B       $2 - Left
;   $10 - A       $1 - Right

getbuts:
    ld    a,$20
    ld_long rP1, a    ;turn on P15

    ld_long a,rP1    ;delay
    ld_long a,rP1
    cpl
    and    $f
    swap    a
    ld    b,a
    ld    a,$30
    ld_long    rP1,a
    ld    a,$10
    ld_long    rP1,a    ;turn on P14
    ld_long    a,rP1    ;delay
    ld_long    a,rP1
    ld_long    a,rP1
    ld_long    a,rP1
    ld_long    a,rP1
    ld_long    a,rP1
    cpl
    and    $f
    or    b
    swap    a
    ld  b,a
    ld  a,$30
    ld_long  rP1,a
    ld  a,b
    ret

;Delay required for external
;keyboard circuits.

sdelay: push    af
        ld      a,128
del0:   dec     a
        call    del1
        call    del1
        jp      nz,del0
        pop     af
del1:   ret

; * external keyboard routine *

extkey:
        call    getLink
        or      a               ;Is link port not used?
        jp      z,getch0        ;yes, don't do anything

        cp      2               ;Is link port set for external keyboard?
        jr      z,extky0        ;yes

        ld      a,[linkUp]
        or      a               ;Is serial link up?
        jp      z,getch0        ;no, ignore rxchar

        ld      a,[rxchar]
        cp      TXIDLE          ;Is an external key ready?
        jp      z,getch0        ;no

        push    af
        ld      a,TXIDLE
        ld      [rxchar],a
        pop     af
        ret

;Check external keyboard for a key

extky0:
        call    keychk          ;key pressed?
        jr      nz,extky1       ;yes

        jp      getch0

extky1:
        push    hl
extky2:
        call    keychk          ;key still pressed?
        jr      nz,extky2       ;yes, wait until released

        pop     hl

        ld      a,[hl]
        ret

;check for external keyboard key pressed
;Return:
; Z flag reset if key found
; HL points to key if found

keychk:
        xor     a               ;Default SHIFT & CAPS LOCK to off
        ld      [extShift],a
        ld      [extCapsLock],a

        ld      hl,0
        ld      de,8
        ld      b,$7f
        ld      c,8
keych1:
        ld      a,b
        rlc     a
        ld      b,a

        ldh     [1],a           ;Setup column to scan on
        ld      a,$81           ;external keyboard
        ldh     [2],a

        call    sdelay

        ld      a,b             ;Read column on
        ldh     [1],a           ;external keyboard
        ld      a,$81
        ldh     [2],a

        call    sdelay

        ldh     a,[1]
        call    remove_shift    ;remove shift keys from scan
        cp      $ff            ;key pressed?
        jp      nz,keych2       ;yes

        add     hl,de

        dec     c
        jr      nz,keych1
        ret

keych2: rrc     a
        jr      nc,keych3
        inc     hl
        jr      keych2

keych3:

; Select character lookup table based on
; shift state.

        ld      a,[extShift]    ;Is shift pressed?
        or      a
        ld      bc,keytLC
        ld      de,keytUC
        jr      z,keych4        ;no
        push    bc              ;exchange bc & de
        ld      b,d
        ld      c,e
        pop     de
keych4:
        ld      a,[extCapsLock] ;Is caps lock on?
        or      a
        jr      z,keych6        ;no

        push    hl
        add     hl,bc
        ld      a,[hl]
        pop     hl

        cp      "A"             ;Is it an alpha?
        jr      c,keych6        ;no
        cp      "z"+1           ;Is it an alpha?
        jr      nc,keych6       ;no
        cp      "a"             ;Is it an alpha?
        jr      nc,keych5       ;yes
        cp      "Z"+1           ;Is it an alpha?
        jr      nc,keych6       ;no

keych5: push    bc              ;exchange bc & de
        ld      b,d
        ld      c,e
        pop     de

keych6: add     hl,bc

        ld      a,1
        or      a               ;reset z flag to indicate key found
        ret

; Remove shift key from detection as a character
; from external keyboard processing.

remove_shift:
        push    bc
        ld      c,a
        and     CAPSROW         ;Is the CAPS LOCK row pressed?
        jr      nz,remsh1       ;no

        ld      a,b
        cp      CAPSCOL         ;Is the CAPS LOCK column pressed?
        jr      nz,remsh1       ;no

        ld      a,1             ;Set External caps Lock true
        ld      [extCapsLock],a

        ld      a,c
        xor     CAPSROW         ;Remove CAPS LOCK from detection
        ld      c,a
remsh1:
        ld      a,c
        and     SHFTROW         ;Is the SHIFT row pressed?
        jr      nz,remsh2       ;no

        ld      a,b
        cp      SHFTCOL         ;Is the SHIFT column pressed?
        jr      nz,remsh2       ;no

        ld      a,1             ;Set External Shift true
        ld      [extShift],a

        ld      a,c
        xor     SHFTROW         ;Remove SHIFT from detection
        ld      c,a

remsh2: ld      a,c
        pop     bc
        ret

keytLC: DB   "zvbx c  "
        DB   "afgshd  "
        DB   "qrtwye  "
        DB   "145263  "
        DB   "\\n.,m/` "
        DB   13,"';lkj  "
        DB   "][poiu",8,0
        DB   "=-0987",9,0

keytUC: DB   "ZVBX C  "
        DB   "AFGSHD  "
        DB   "QRTWYE  "
        DB   "!$%@^#  "
        DB   "|N><M?~ "
        DB   13,34,":LKJ  "
        DB   "}\{POIU",8,0
        DB   "+_)(*&",9,0

; *  Initialize on screen keypad *

initkpd:
        ld      b,1     ;set window for all but menu
        ld      c,1
        ld      d,20
        ld      e,15
        call    window

        ld      a,1
        ld      [keypadx],a
        ld      [keypady],a

        call    DrawMenu

        jp      highlight

;        ld      b,1             ;set window for all but menu
;        ld      c,1
;        ld      d,20
;        ld      e,15
;        jp      window

; * Draw Menu *

DrawMenu:
        ld      a,[curx]
        ld      b,a
        ld      a,[cury]
        ld      c,a
        push    bc

        ld      bc,$110         ;b=1, c=16
        call    locate

        ld      hl,menuLC
        ld      a,[capsLock]
        or      a               ;Is lowercase selected?
        jr      z,drawm0        ;yes
        ld      hl,menuUC
drawm0:
        ld      c,1
drawm1: ld      b,1
drawm2: ldi     a,[hl]
        or      a
        jr      z,drawm4

        push    af
        call    lowlight
        ld      a,[keypadx]
        cp      b
        jr      nz,drawm3
        ld      a,[keypady]
        cp      c
        jr      nz,drawm3

        ld      d,b
        ld      e,c

        call    highlight
drawm3:
        pop     af
        call    charout
        inc     b
        ld      a,21
        cp      b
        jr      nz,drawm2
        inc     c
        jr      drawm1
drawm4:
;        call    lowlight
;        ld      a," "
;        call    charoutnu

        call    highlight
        pop     bc
        push    bc
        call    xy2char
        pop     bc
        ld      a,b
        ld      [curx],a
        ld      a,c
        ld      [cury],a
        jp      charou2

; Onscreen menu in lowercase

menuLC: DB    "1230?abcdefghi.<>()$"
        DB    "456=+jklmnopqr:;#%/*"
        DB    "789",10,"-stuvwxyz",34,"&!@"
        DB    $27,",^"
        DB    0

; Onscreen menu in uppercase

menuUC: DB    "1230?ABCDEFGHI.<>()$"
        DB    "456=+JKLMNOPQR:;#%/*"
        DB    "789",10,"-STUVWXYZ",34,"&!@"
        DB    $27,",^"
        DB    0

waitvbl:                        ; Wait for VBL
        ldh      a,[rLCDC]
        add      a,a
        ret      nc

notyet:
        ldh     a,[rLY]         ; $ff44=LCDC Y-Pos
        cp      $90             ; $90 and bigger = in VBL
        jr      nz,notyet       ; Loop until it $90
        ret

white:                          ; All colors to transparent
        ld      a,0
        ldh     [rBGP],a
        ret

black:                          ; All colors to black
        ld      a,$ff
        ldh     [rBGP],a
        ret

nor_col:                        ; Sets the colors to normal palette
        ld      a,%11100100     ; color 3=11 (Black]
                                ; color 2=10 (Dark grey]
                                ; color 1=01 (Light grey]
                                ; color 0=00 (Transparent]
        ldh     [rBGP],a
        ret

;: * Initialize the Character Set *

move_char:
        ld      hl,_VRAM8000
        ld      bc,charset
        ld      d,0             ; Like move 1024 bytes man
        ld      e,4             ; x2=1024

lp1:
        ld      a,[bc]
        ld      [hli],a
        ld      [hli],a
        inc     bc
        dec     d
        jr      nz,lp1
        dec     e
        jr      nz,lp1

        ld      hl,_VRAM8800        ; Fill second set with same
        ld      bc,charset      ; at half brightness.
        ld      d,0             ; Move 1024 bytes
        ld      e,4

lp2:
        ld      a,[bc]
        ld      [hli],a
        ld      [hl],0
        inc     hl
        inc     bc
        dec     d
        jr      nz,lp2
        dec     e
        jr      nz,lp2
        ret

move_noth:
        ld      hl,_SCRN0
        ld      d,0
        ld      e,4
move_n:
        ld      a," "
        ld      [hli],a
        dec     d
        jr      nz,move_n
        dec     e
        jr      nz,move_n
        ret

move_text:
        ld      a,1
        ldh     [rVBK],a    ; Select GBC attribute memory

        ld      hl,_VRAM9000
        ld      d,0        ; 0 = 256
wloop0:
        di
        HV_WAIT
        xor     a
        ld      [hli],a
        ld      [hli],a
        ld      [hli],a
        ld      [hli],a
        ei

        dec     d
        jr      nz,wloop0


        xor     a
        ldh     [rVBK],a    ; Select GBC tile map memory

        ld      bc,the_text
mve:
        ld      hl,_SCRN0
        ld      d,0
        ld      e,4       ; 256*4=1024=32x32=One whole GB Screen

wloop1:
        di
        HV_WAIT
        ld      a,[bc]
        ld      [hli],a
        ei

        inc     bc
        dec     d
        jr      nz,wloop1
        dec     e
        jr      nz,wloop1

        ret

; * Print string pointed to by BC
;   and terminated with a 0 *

prntstr:
        ld      a,[bc]
        or      a
        ret     z
        call    charout
        inc     bc
        jr      prntstr

; * Print a Space *

;space:
;        ld      a," "
;        jr      charout


charout:
        push    bc
        ld      b,a
        call    outch
        pop     bc
        ret

; * Print a Character & protect all registers *

outch:
        push    af
        push    bc
        push    de
        push    hl
        ld      a,b
        call    charoutns
outskip:
        pop     hl
        pop     de
        pop     bc
        pop     af
        ret

; * Print a Character and update cursor position *

charoutns:
        push    af

        ld      a,[vmode]
        dec     a               ;are we in graphics mode?
        jp      z,chnuvx        ;yes, ignore print

        pop     af

        cp      BS              ;is it backspace?
        jp      z,charout5      ;yes

        cp      cr              ;is it enter?
        jr      nz,charou1      ;no

        ld      a," "
        call    charoutnu       ;get rid of cursor
        call    charout2        ;move cursor down a line
        jr      charou2         ;display cursor

charou1:
        call    charoutnu

        ld      a,[windlrx]
        cp      19              ;Are we in far right column?
        jr      nz,charou3      ;no
        ld      a,[windlry]
        cp      17              ;Are absolute bottom row?
        jr      nz,charou3      ;no

;This routine is only used for drawing the on screen
;menu's most bottom right character. Normal scroll
;and wrap-around features won't work in this case.

        ld      a,1             ;Put the cursor somewhere so
        ld      [curx],a        ;that erasing cursor later works okay
        ld      [cury],a
        ret                     ;Don't advance cursor since we are

charou3:
        call    curupdate
charou2:
        ld      a,"_"
        jp      charoutnu

; Update Cursor position
curupdate:
        ld      a,[windulx]
        ld      b,a
        ld      a,[windlrx]
        sub     b
        inc     a
        ld      b,a   ;b = window width
        ld      a,[curx]
        cp      b   ;is cursor at end of line?
        jr      z,charout2  ;yes

charout1:
        inc     a
        ld      [curx],a
        ret

charout2:
        ld      a,[winduly]
        ld      b,a
        ld      a,[windlry]
        sub     b
        inc     a
        ld      b,a          ;b = window width
        ld      a,[cury]
        cp      b            ;is cursor on last line?
        jr      z,charout4   ;yes

        ld      a,[cury]
        inc     a
        ld      [cury],a

charout3:
        xor     a
        jr      charout1

charout4:
        call    scroll
        jr      charout3

charout5:
        ld      a,[curx]
        cp      1            ;are we at beginning?
        ret     z            ;yes, do nothing

        push    af
        ld      a," "
        call    charoutnu
        pop     af
        dec     a
        ld     [curx],a
        ld     a,"_"
        jp     charoutnu

;* HighLight *

highlight:
 ld    a,1
 ld    [bright],a
 ret

;* LowLight *

lowlight:
 xor    a
 ld    [bright],a
 ret

; * Print a Character *

charoutnu:
 push    af

 ld     a,[vmode]
 dec    a               ;are we in graphics mode?
 jr     z,chnuvx        ;yes, ignore print

 ld     a,[windulx]
 ld     d,a
 ld     a,[curx]
 dec    a
 add    a,d
 ld     d,a

 ld     a,[winduly]
 ld     e,a
 ld     a,[cury]
 dec    a
 add    a,e
 ld     e,a

 ld     l,e
 ld     h,0
 add    hl,hl
 add    hl,hl
 add    hl,hl
 add    hl,hl
 add    hl,hl
 ld     c,d
 ld     b,0
 add    hl,bc
 ld     bc,_SCRN0
 add    hl,bc

 ld     a,[bright]  ;bright if 1
 ld     b,a
 pop    af
 and    $7f
 dec    b
 jr     z,chnubr

 add    a,128
chnubr:
 ld     b,a

chnuloop:
 di
 HV_WAIT
 ld     [hl],b
 ei
 ret

chnuvx:
 pop    af
 ret

;move:
;        ldi     a,[hl]
;        ld      [de],a
;        inc     de

; * LDIR - Just like z80 command *

ldir:
 di
 HV_WAIT
 ldi    a,[hl]
 ld     [de],a
 ei

 inc    de
 dec    bc
 ld     a,b
 or     c
 jr     nz,ldir
 ret

; * Position Cursor *
;  New cursor position is (b,c).
;  This position is relative to the window.
;  Possible Values are (1,1) - (20,18]

locate:

 push   bc
 ld     a," "
 call   charoutnu ;get rid of cursor
 pop    bc

 ld     a,b
 ld     [curx],a
 ld     a,c
 ld     [cury],a
 ret

; * Clear Screen *

cls:
 ld     a,[vmode]
 dec    a               ;are we in graphics mode?
 jp     z,clsgr         ;yes

 jp     clstxt

; * Scroll Window up *
;  Upper left coordinate of window is (windulx,winduly).
;  Lower right coordinate of window is (windlrx,windlry).
;  Possible Values are (0,0) - (19,17]

scroll:
 ld     a,[winduly]
 ld     l,a
 ld     h,0
 add    hl,hl
 add    hl,hl
 add    hl,hl
 add    hl,hl
 add    hl,hl
 ld     a,[windulx]
 ld     c,a
 ld     b,0
 add    hl,bc
 ld     bc,_SCRN0
 add    hl,bc
 push   hl      ;put UL address on stack
 ld     bc,32
 add    hl,bc   ;hl=next line
 pop    bc

 ld     a,[winduly]
 ld     e,a
 ld     a,[windlry]
 sub    e
 ld     e,a     ;e = window heigth in characters

scroll1:
 push   bc

 ld     a,[windulx]
 ld     d,a
 ld     a,[windlrx]
 sub    d
 inc    a
 ld     d,a     ;d = window width in characters

scroll2:
 di
 HV_WAIT
 ldi    a,[hl]
 ld     [bc],a
 ei

 inc    bc
 dec    d
 jr     nz,scroll2

 pop    hl

 ld     bc,32
 add    hl,bc
 push   hl
 ld     bc,32
 add    hl,bc   ;hl=next line
 pop    bc

 dec    e
 jr     nz,scroll1

; Fill new line with all spaces

 ld     a,[windulx]
 ld     d,a
 ld     a,[windlrx]
 sub    d
 inc    a
 ld     d,a     ;d = window width in characters

scroll4:
 di
 HV_WAIT
 ld     a,32
 ld     [bc],a
 ei

 inc    bc
 dec    d
 jr     nz,scroll4

 ret

;Set Cursor Window
;BC = X,Y upper left of window
;DE = X,Y lower right of window

window:
  ld      a,b
  dec     a
  ld      [windulx],a
  ld      a,c
  dec     a
  ld      [winduly],a
  ld      a,d
  dec     a
  ld      [windlrx],a
  ld      a,e
  dec     a
  ld      [windlry],a
    ret

hvwait1:
        ldh     a,[rLY]
        cp      144
        jr      c,hvwait1
        cp      153
        jr      nc,hvwait1
        ret

; *** Set first background palette ***

SetAllCGB_BGP:
        ld      hl,BGPaletteTable

        ld      a,$80
        ldh     [rBCPS],a

        ld      bc,$0869        ; b = 8, c = rBCPD
        di
SetBGP2:
        ldh     a,[rLCDC]
        and     2
        jr      nz,SetBGP2

        ld      a,[hli]
        ld      [c],a
        ei

        dec     b
        jr      nz,SetBGP2

        ret

;  This RGBSet Macro uses RGB values from 0 to 255.
; Even though this is a greater color range than the
; GBC supports it allows you to directly read color
; values from your favorite paint program using the
; eye dropper tool and drop them directly into your
; program.


RGBSet: MACRO;\1 = r, \2 = g, \3 = b   
  DW ((\3 >> 3) << 10) + ((\2 >> 3) << 5) + (\1 >> 3)
ENDM

; ** Example colo(u)r palettes **

BGPaletteTable:
        RGBSet 255,255,255     ; White         - Palette #1
        RGBSet 127,127,127     ; Dark Grey
        RGBSet 127,127,127     ; Dark Grey
        RGBSet 0,0,0           ; Black

charset:
 DB 0,0,0,$1f,16,16,16,16            ; upper left of box
 DB 0,0,0,$ff,0,0,0,0            ; upper middle of box
 DB 0,0,0,$f8,8,8,8,8            ; upper right of box
 DB 16,16,16,16,16,16,16,16            ; left middle of box
 DB 8,8,8,8,8,8,8,8                ; right middle of box
 DB $10,$10,$10,$10,$1f,0,0,0        ; lower left of box
 DB 0,0,0,0,$ff,0,0,0            ; lower middle of box
 DB 8,8,8,8,$f8,0,0,0            ; lower right of box
 DB $ff,$ff,$e7,$c3,$c3,$e7,$ff,$ff
 DB 0,$3c,$66,$42,$42,$66,$3c,0
 DB $7e,$42,$42,$42,$42,$42,$42,$7e
 DB $f,7,$f,$7d,$cc,$cc,$cc,$78
 DB $3c,$66,$66,$66,$3c,$18,$7e,$18
 DB $3f,$33,$3f,$30,$30,$70,$f0,$e0
 DB $7f,$63,$7f,$63,$63,$67,$e6,$c0
 DB $99,$5a,$3c,$0e7,$0e7,$3c,$5a,$99
 DB $80,$e0,$f8,$fe,$f8,$e0,$80,0
 DB 2,$0e,$3e,$fe,$3e,$0e,2,0
 DB $18,$3c,$7e,$18,$18,$7e,$3c,$18
 DB $66,$66,$66,$66,$66,0,$66,0
 DB $7f,$db,$db,$7b,$1b,$1b,$1b,0
 DB $3e,$63,$38,$6c,$6c,$38,$cc,$78
 DB 0,0,0,0,$7e,$7e,$7e,0
 DB $18,$3c,$7e,$18,$7e,$3c,$18,$ff
 DB $18,$3c,$7e,$18,$18,$18,$18,0
 DB $18,$18,$18,$18,$7e,$3c,$18,0
 DB 0,$18,$0c,$fe,$0c,$18,0,0
 DB 0,$30,$60,$fe,$60,$30,0,0
 DB 0,0,$c0,$c0,$c0,$fe,0,0
 DB 0,$24,$66,$ff,$66,$24,0,0
 DB 0,$18,$3c,$7e,$ff,$ff,0,0
 DB 0,$ff,$ff,$7e,$3c,$18,0,0
 DB 0,0,0,0,0,0,0,0
 DB $30,$78,$78,$78,$30,0,$30,0              ;!
 DB $6c,$6c,$6c,0,0,0,0,0                    ;"
 DB $6c,$6c,$fe,$6c,$fe,$6c,$6c,0          ;#
 DB $30,$7c,$c0,$78,$0c,$f8,$30,0          ;$
 DB 0,$c6,$cc,$18,$30,$66,$c6,0           ;%
 DB $38,$6c,$38,$76,$dc,$cc,$76,0          ;&
 DB $60,$60,$c0,0,0,0,0,0                   ;'
 DB $18,$30,$60,$60,$60,$30,$18,0            ;(
 DB $60,$30,$18,$18,$18,$30,$60,0            ;]
 DB 0,$66,$3c,$ff,$3c,$66,0,0               ;*
 DB 0,$30,$30,$fc,$30,$30,0,0               ;+
 DB 0,0,0,0,0,$30,$30,$60                    ;,
 DB 0,0,0,$fc,0,0,0,0                       ;-
 DB 0,0,0,0,0,$30,$30,0                      ;.
 DB 6,$0c,$18,$30,$60,$c0,$80,0             ;/
 DB $7c,$c6,$ce,$de,$f6,$e6,$7c,0       ;0
 DB $30,$70,$30,$30,$30,$30,$fc,0           ;1
 DB $78,$cc,$0c,$38,$60,$cc,$fc,0         ;2
 DB $78,$cc,$0c,$38,$0c,$cc,$78,0          ;3
 DB $1c,$3c,$6c,$cc,$fe,$0c,$1e,0          ;4
 DB $fc,$c0,$f8,$0c,$0c,$cc,$78,0        ;5
 DB $38,$60,$c0,$f8,$cc,$cc,$78,0        ;6
 DB $fc,$cc,$0c,$18,$30,$30,$30,0          ;7
 DB $78,$cc,$cc,$78,$cc,$cc,$78,0        ;8
 DB $78,$cc,$cc,$7c,$0c,$18,$70,0          ;9
 DB 0,$30,$30,0,0,$30,$30,0                  ;:
 DB 0,$30,$30,0,0,$30,$30,$60                ;;
 DB $18,$30,$60,$c0,$60,$30,$18,0           ;<
 DB 0,0,$fc,0,0,$fc,0,0                    ;=
 DB $60,$30,$18,$0c,$18,$30,$60,0            ;>
 DB $78,$cc,$0c,$18,$30,0,$30,0             ;?
 DB $7c,$c6,$de,$de,$de,$c0,$78,0       ;@
 DB $30,$78,$cc,$cc,$fc,$cc,$cc,0       ;A
 DB $fc,$66,$66,$7c,$66,$66,$fc,0          ;B
 DB $3c,$66,$c0,$c0,$c0,$66,$3c,0         ;C
 DB $f8,$6c,$66,$66,$66,$6c,$f8,0          ;D
 DB $7e,$60,$60,$78,$60,$60,$7e,0            ;E
 DB $7e,$60,$60,$78,$60,$60,$60,0            ;F
 DB $3c,$66,$c0,$c0,$ce,$66,$3e,0         ;G
 DB $cc,$cc,$cc,$fc,$cc,$cc,$cc,0     ;H
 DB $78,$30,$30,$30,$30,$30,$78,0            ;I
 DB $1e,$0c,$0c,$0c,$cc,$cc,$78,0          ;J
 DB $e6,$66,$6c,$78,$6c,$66,$e6,0          ;K
 DB $60,$60,$60,$60,$60,$60,$7e,0            ;L
 DB $c6,$ee,$fe,$fe,$d6,$c6,$c6,0     ;M
 DB $c6,$e6,$f6,$de,$ce,$c6,$c6,0     ;N
 DB $38,$6c,$c6,$c6,$c6,$6c,$38,0         ;O
 DB $fc,$66,$66,$7c,$60,$60,$f0,0          ;P
 DB $78,$cc,$cc,$cc,$dc,$78,$1c,0        ;Q
 DB $fc,$66,$66,$7c,$6c,$66,$e6,0          ;R
 DB $78,$cc,$e0,$70,$1c,$cc,$78,0         ;S
 DB $fc,$30,$30,$30,$30,$30,$30,0           ;T
 DB $cc,$cc,$cc,$cc,$cc,$cc,$fc,0     ;U
 DB $cc,$cc,$cc,$cc,$cc,$78,$30,0       ;V
 DB $c6,$c6,$c6,$d6,$fe,$ee,$c6,0     ;W
 DB $c6,$c6,$6c,$38,$38,$6c,$c6,0         ;X
 DB $cc,$cc,$cc,$78,$30,$30,$78,0         ;Y
 DB $fe,6,$0c,$18,$30,$60,$fe,0            ;Z
 DB $78,$60,$60,$60,$60,$60,$78,0            ;[
 DB $c0,$60,$30,$18,$0c,6,2,0               ;\
 DB $78,$18,$18,$18,$18,$18,$78,0            ;]
 DB $10,$38,$6c,$c6,0,0,0,0                 ;^
 DB 0,0,0,0,0,0,0,$ff                       ;_
 DB $30,$30,$18,0,0,0,0,0                    ;`
 DB 0,0,$78,$0c,$7c,$cc,$76,0               ;a
 DB $e0,$60,$60,$7c,$66,$66,$dc,0          ;b
 DB 0,0,$78,$cc,$c0,$cc,$78,0             ;c
 DB $1c,$0c,$0c,$7c,$cc,$cc,$76,0          ;d
 DB 0,0,$78,$cc,$fc,$c0,$78,0             ;e
 DB $38,$6c,$60,$f0,$60,$60,$f0,0          ;f
 DB 0,0,$76,$cc,$cc,$7c,$0c,$f8           ;g
 DB $e0,$60,$6c,$76,$66,$66,$e6,0          ;h
 DB $30,0,$70,$30,$30,$30,$78,0              ;i
 DB $0c,0,$0c,$0c,$0c,$cc,$cc,$78          ;j
 DB $e0,$60,$66,$6c,$78,$6c,$e6,0          ;k
 DB $70,$30,$30,$30,$30,$30,$78,0            ;l
 DB 0,0,$cc,$fe,$fe,$d6,$c6,0           ;m
 DB 0,0,$f8,$cc,$cc,$cc,$cc,0           ;n
 DB 0,0,$78,$cc,$cc,$cc,$78,0             ;o
 DB 0,0,$dc,$66,$66,$7c,$60,$f0            ;p
 DB 0,0,$76,$cc,$cc,$7c,$0c,$1e            ;q
 DB 0,0,$dc,$76,$66,$60,$f0,0              ;r
 DB 0,0,$7c,$c0,$78,$0c,$f8,0              ;s
 DB $10,$30,$7c,$30,$30,$34,$18,0            ;t
 DB 0,0,$cc,$cc,$cc,$cc,$76,0            ;u
 DB 0,0,$cc,$cc,$cc,$78,$30,0             ;v
 DB 0,0,$c6,$d6,$fe,$fe,$6c,0            ;w
 DB 0,0,$c6,$6c,$38,$6c,$c6,0              ;x
 DB 0,0,$cc,$cc,$cc,$7c,$0c,$f8          ;y
 DB 0,0,$fc,$98,$30,$64,$fc,0              ;z
 DB $1c,$30,$30,$e0,$30,$30,$1c,0           ;{
 DB $18,$18,$18,0,$18,$18,$18,0              ;|
 DB $e0,$30,$30,$1c,$30,$30,$e0,0          ;}
 DB $76,$dc,0,0,0,0,0,0                     ;~
 DB 0,$10,$38,$6c,$c6,$c6,$fe,0

down:
DB "                    "
DB "............"
the_text:
DB 32,32,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,32,32
DB "            "
DB 32,32,3
DB "GB Basic V2.10"
DB 4,32,32
DB "            "
DB 32,32,5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,7,32,32
DB "            "
DB "         by         "
DB "............"
DB "   Jeff Frohwein    "
DB "............"
DB "                    "
DB "............"
DB "http://www.devrs.com"
DB "............"
DB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
DB "............"
DB " Button Assignments:"
DB "............"
DB "                    "
DB "............"
DB "    Enter = A       "
DB "............"
DB "   Select = B       "
DB "............"
DB "Caps Lock = SELECT  "
DB "............"
DB "Backspace = START   "
DB "............"
DB "    Break = SEL&STRT"
DB "............"
DB "                    "
DB "............"
DB "   [Press START!]   "
DB "............"
DB "                    "
DB "............"
