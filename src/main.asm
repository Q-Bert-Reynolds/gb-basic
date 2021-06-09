;********************************
;*     Floating-Point Basic     *
;*             for              *
;*        Z80 or GameBoy        *
;********************************

;December 9, 1997 - last edit from Jeff Frohwein
;    June 8, 2021 - first edit from Nolan Baker

percision EQU   6            ;This is the floating percision in digits.
                              ;It should be an even number because the
                              ;floating point routines can't handle odd.
                              ;Increasing it's size increases percision
                              ;but is slower & requires more ram usage.

fpsiz   EQU     (percision/2)+2        ;Size in bytes of a fp number
digit   EQU     percision/2            ;fpsiz-2
fpnib   EQU     percision
stesiz  EQU     2+fpsiz      ;symbol table element size
cr      EQU     13           ;carriage return
null    EQU     0            ;null character value
lf      EQU     10           ;line feed
esc     EQU     3            ;escape char
eof     EQU     1            ;end of file
bell    EQU     7            ;bell character
linlen  EQU     80           ;# of chrs in legal input line
opbase  EQU     "("
ftype   EQU     1            ;control stack for entry type
forsz   EQU     fpsiz*2+2+2+1;'for' control stack entry size
gtype   EQU     2            ;control stack gosub entry type
etype   EQU     0            ;control stack underflow type
uminu   EQU     $31          ;unary minus
term    EQU     $22          ; 'prnt' terminator character

subit   EQU     2            ;speed up button bit for list
sdbit   EQU     3            ;slow down button bit for list

linent  EQU     $e           ;line number token

GBB_RDY EQU     1            ;Input ready command for ext terminal

INCLUDE "src/gb.inc"

SECTION "GB Basic RAM", WRAM0
;floating point ram
hold1:: DS digit+1
hold2:: DS digit+1
hold3:: DS digit+1
hold4:: DS digit+1
hold5:: DS digit+1
hold6:: DS digit+1
hold7:: DS digit+1
hold8:: DS digit+1
nu1:: DB
erri:: DB                   ;error flag
nu2:: DB
buf:: DS digit              ;working buffer
sign:: DB                   ;sign bit
exp:: DB                    ;exponent
rctrl:: DB                  ;rounding control flag 1=msd
rdigi:: DB                  ;rounding digit
signd: MACRO
  hold1+digit
ENDM
expd: MACRO
  hold1+digit+1
ENDM
;
;    system ram
;
phead:: DB
reltyp:: DB
nullct:: DB
argf:: DB
dirf:: DB
txa:: DW
cstksz EQU      100
astksz EQU      fpsiz*linlen/2
cstkl:: DS cstksz
astkl:: DS astksz
rtxa:: DW
cstka:: DW
sink:: DS fpsiz-1
fpsink:: DS fpsiz
ftemp:: DS fpsiz
ftem1:: DS fpsiz
ftem2:: DS fpsiz
frand:: DB
ibcnt:: DB
ibln:: DW
ibuf:: DS linlen
cnsbuf:: DS 6             ;storage for 'cns' output
astka:: DW
adds:: DW
addt:: DW
bcadd:: DW
opst:: DB
opstr:: DB
ecnt:: DB
fsign:: DB
bcs:: DS digit+2
abufsiz EQU digit*2+2
abuf:: DS abufsiz
xsign:: DB
expo:: DB
fes:: DB
infes:: DB
maxl:: DW
insa:: DW
callRegC:: DB               ;Storage for C reg for USR
callRegB:: DB               ;Storage for B reg for USR
callRegE:: DB               ;Storage for E reg for USR
callRegD:: DB               ;Storage for D reg for USR
miscW1:: DW                 ;temp storage for SAVE,LOAD,LIST, & PFIX

;* Important memory pointers *
MemoryPointers::
bofa:: DW                   ;start of file addr
eofa:: DW                   ;end of file addr
mata:: DW                   ;free memory for upward growing matrixs
stb:: DW                    ;first byte of downward growing variables
memtop:: DW                 ;last assigned memory location
memfree:: DB

;Basic Statements Storage Format
;  byte - Length of line (includes this length]
;  word - Line number
;   tokens & data
;  byte - CR

;Save to backup ram format
;  byte "B" - Basic
;  byte "F" - File
;  byte "0" - Format 0
;  word crc
;  word length
;   byte data

;        .org     100h
;
;    startup basic system
;

;#include "gb.inc"
SECTION "Main", ROM0
start::
        ld      sp,stack

        ld      hl,memfree
        ld      a,l
        ld      [bofa],a    ;start of user assigned memory
        ld      a,h
        ld      [bofa+1],a

        ld      hl,$dfff
        ld      a,l
        ld      [memtop],a  ;end of assigned memory pointer
        ld      a,h
        ld      [memtop+1],a

        ld      a,l
        ld      [stb],a
        ld      a,h
        ld      [stb+1],a

        call    new             ;new program

        ld      a,$77           ;turn sound volume up
        ldh     [rAUDVOL],a

        xor     a               ;set sound outputs to off
        ldh     [rAUDTERM],a

        ld      a,$82           ;turn sound 2 generator on
        ldh     [rAUDENA],a

        ld      a,$84           ;set sound duty
        ldh     [rAUD2LEN],a

        ld      a,$f0          ;set envelope
        ldh     [rAUD2ENV],a

        ld      a,2*fpnib
        ld      [infes],a

; initialize random number
        ld      de,frand
        ld      hl,rands
        call    vcopy         ;frand=random number seed

        ld      a,CART_SRAM_ENABLE
        ld      [rRAMG],a           ;enable sram

        ld      a,[_SRAM]
        cp      "B"             ;is this file okay?
        jr      nz,sineon       ;no

        ld      a,[_SRAM+1]
        cp      "F"             ;is this file okay?
        jr      nz,sineon       ;no

        ld      a,[_SRAM+2]
        cp      "0"             ;is this file format 0?
        jr      nz,sineon       ;no

        ld      a,[_SRAM+6]
        and     $80             ;is high bit set?
        jr      z,sineon        ;no, don't autoload

        call    loadp           ;load file
        call    crun            ;run program
        jr      cmnd0

sineon::
        xor     a
        ld      [rRAMG],a           ;disable sram

; print sign on message

        ld      hl,signon
        call    prnt
;
;    command processor
;
cmnd0::  call    crlf

cmnd1::  ld      hl,rdys      ;print 'Ok'
        call    prnt

        call    crlf

cmndr::  ld      a,1          ;set direct input flag
        ld      [dirf],a

        ld      sp,stack

cmnd2::
;       ld      b,GBB_RDY    ;Send input ready char.
;       call    chout        ;Only needed by external terminal.

        call    inline         ;get input line from operator
        ld      hl,ibuf
        ld      a,cr
        cp      [hl]         ;is line blank?
        jr      z,cmnd2      ;yes

        call    pp           ;pre-process it
        jr      c,cmnd3

        call    line         ;line number..go edit
        call    cclear
        jr      cmnd2

cmnd3::
        call    cmnd4

        jr      cmnd1

cmnd4::  ld      hl,ibuf         ;point to command or statement
        ld      a,l
        ld      [txa],a
        ld      a,h
        ld      [txa+1],a

cmnd5::
        call    istat         ;process statement (if allowed]
        call    gci
        cp      ":"
        jr      z,cmnd5
        cp      cr
        ret     z
        jp      e1

;* Error Statements *

ermbs::  DB   "Syntax",term           ;'bs'
ermba::  DB   "Argument",term         ;'ba'
ermcs::  DB   "Control Stack",term    ;'cs'
ermdi::  DB   "Direct input",term     ;'di'
ermob::  DB   "Out of range",term     ;'ob'
ermof::  DB   "Overflow",term
ermdm::  DB   "Duplicate",term        ;'dm'
ermdz::  DB   "Divide by 0",term
ermfp::  DB   "Floating point",term   ;'fp'
ermrd::  DB   "Out of DATA",term      ;'rd'
ermif::  DB   "Illegal function call",term
ermin::  DB   "Input",term      ;'in'
ermso::  DB   "Out of memory",term    ;'so'
ermll::  DB   "Line too long",term    ;'ll'
ermln::  DB   "Undefined line number",term


e1::     ld      hl,ermbs        ; 6273h 'bs'
        jr      error
e3::     ld      hl,ermba        ; 6261h 'ba'
        jr      error
e4::     ld      hl,ermcs        ; 6373h 'cs'
        jr      error
e5::     ld      hl,ermob        ; 6f62h 'ob'
        jr      error
e6::     ld      hl,ermdm        ; 646dh 'dm'
        jr      error
e7::     ld      hl,ermof
        jr      error
e8::     ld      hl,ermso

error::
        push    hl
        call    text_mode       ;set to text mode if not already
        pop     hl

        call    prnt

        ld      hl,ers
erm1::   call    prnt
        ld      a,[dirf]
        or      a
        jp      nz,cmnd0

        ld      hl,ins
        call    prnt

; find line number
        ld      a,[bofa]
        ld      l,a
        ld      a,[bofa+1]
        ld      h,a
erm2::   ld      b,h
        ld      c,l
        ld      e,[hl]
        ld      d,0
        add     hl,de

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        ld      hl,txa
        call    dcmp

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        jp      c,erm2
        inc     bc
        ld      a,[bc]
        ld      l,a
        inc     bc
        ld      a,[bc]
        ld      h,a
        ld      de,ibuf         ;use ibuf to accumulate the line line number string
        call    cns
        ld      a,cr
        ld      [de],a
        ld      hl,ibuf
        call    prntcr
        jp      cmnd0
;
; line editor
;
line::   ld      a,[bofa]     ;check for empty file
        ld      l,a
        ld      a,[bofa+1]
        ld      h,a

fin::    ld      a,[hl]       ;check if appending line at end
        dec     a
        jr      z,app

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        inc     de

        ld      a,[ibln]     ;get input line number
        ld      l,a
        ld      a,[ibln+1]
        ld      h,a

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        call    dcmp         ;compare with file line number
        dec     hl
        jr      c,insr       ;less than
        jr      z,insr       ;equal
        ld      a,[hl]         ;length of line
        call    aa2hl        ;jump forward
        jr      fin

; append line at end case
app::    ld      a,[ibcnt]    ;don't append null line
        cp      4
        ret     z

        call    full         ;check for room in file

        ld      a,[eofa]     ;place line in file
        ld      l,a
        ld      a,[eofa+1]
        ld      h,a

        call    imov
        ld    [hl],eof

        ld      a,l
        ld      [eofa],a
        ld      a,h
        ld      [eofa+1],a
        ret

; insert line in file case
insr::   ld      b,[hl]         ;old line count
        ld      a,l
        ld      [insa],a     ;insert line pointer
        ld      a,h
        ld      [insa+1],a
        ld      a,[ibcnt]    ;new line count
        jr      c,lt2        ;jmp if new line # not = old line number
        sub     4
        jr      z,lt1        ;test if should delete null line
        add     a,4
lt1::    sub     b
        jr      z,lin1       ;line lengths equal
        jr      c,gt2

; expand file for new or larger line
lt2::    ld      b,a
        ld      a,[ibcnt]
        cp      4         ;don't insert null line
        ret     z

        ld      a,b
        call    full

        ld      a,[insa]
        ld      l,a
        ld      a,[insa+1]
        ld      h,a

        call    nmov

        ld      a,[eofa]
        ld      l,a
        ld      a,[eofa+1]
        ld      h,a

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        ld      a,l
        ld      [eofa],a
        ld      a,h
        ld      [eofa+1],a

        inc     bc
        call    rmov
        jr      lin1

; contract file for smaller line
gt2::    cpl
        inc     a
        call    aa2hl
        call    nmov

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        ld      a,[insa]
        ld      l,a
        ld      a,[insa+1]
        ld      h,a

        call    nz,lmov
        ld      [hl],eof

        ld      a,l
        ld      [eofa],a
        ld      a,h
        ld      [eofa+1],a

; insert current line into file
lin1::   ld      a,[insa]
        ld      l,a
        ld      a,[insa+1]
        ld      h,a

        ld      a,[ibcnt]
        cp      4
        ret     z

; insert current line at addr hl
imov::   ld      de,ibcnt
        ld      a,[de]
        ld      c,a
        ld      b,0

; copy block from beginning
; hl is destin addr, de is source addr, bc is count
lmov::   ld      a,[de]
        ld      [hl],a
        inc     de
        inc     hl
        dec     bc
        ld      a,b
        or      c
        jr      nz,lmov
        ret

; copy block starting at end
; hl is destin addr, de is source addr, bc is count
rmov::   ld    a,[de]
        ld    [hl],a
        dec    hl
        dec    de
        dec    bc
        ld    a,b
        or    c
        jr      nz,rmov
        ret

;  compute file move count
; bc gets (eofa) - (hl], ret z set means zero count
nmov::   ld    a,[eofa]
        sub    l
        ld    c,a
        ld    a,[eofa+1]
        sbc     a,h
        ld    b,a
        or    c
        ret

; add a to hl
aa2hl::
        add     a,l
        ld    l,a
        ret    nc
        inc    h
        ret

; check for file overflow, leaves new eofa in de
; a has increase in size
full::   push    af
        ld      a,[eofa]
        ld      l,a
        ld      a,[eofa+1]
        ld      h,a
        pop     af

        call    aa2hl

        ld      e,l
        ld      d,h

        ld    hl,memtop
        call    dcmp
        jp    nc,e8

        ret
;
;    commands
;
;cls::    ld      b,26
;        jp      chout        ;clear screen

; "new"
new::    
        ld      a,[bofa]
        ld      [eofa],a
        ld      l,a

        ld      a,[bofa+1]
        ld      [eofa+1],a
        ld      h,a

        ld    [hl],eof

; "clear"
cclear:: ld      a,[eofa]    ;clear from eofa to memtop
        ld      e,a
        ld      a,[eofa+1]
        ld      d,a

        inc     de

        ld      a,e
        ld      [mata],a
        ld      a,d
        ld      [mata+1],a

        ld      hl,memtop
cclr1::  xor     a
        ld      [de],a
        call    dcmp
        inc     de
        jr      nz,cclr1

        ld      a,[memtop]
        ld      l,a
        ld      a,[memtop+1]
        ld      h,a

        ld      a,l
        ld      [stb],a
        ld      a,h
        ld      [stb+1],a

        ld      hl,cstkl+cstksz-1
        ld      [hl],etype

        ld      a,l
        ld      [cstka],a
        ld      a,h
        ld      [cstka+1],a

        ld      hl,astkl+astksz+fpsiz-1
        ld      a,l
        ld      [astka],a
        ld      a,h
        ld      [astka+1],a
        ret

; "list"
clist::
        ld      a,1             ;setup list speed
        ld      [miscW1],a
        xor     a
        ld      [miscW1+1],a

        ld      de,0
        ld      bc,-1
        
        call    gc         ;check for parameters
        cp      cr
        jr      z,clst3      ;no parameters

        cp      minrw        ;list -X ?
        jr      z,clst1      ;yes

        call    intger       ;line number valid?
        jp      c,e1         ;no

        ld      e,l          ;first line = hl
        ld      d,h
        ld      c,l          ;last line = hl
        ld      b,h

        call    gci

        cp      cr              ;Is it just list X?
        jr      z,clst3         ;yes

        cp      minrw           ;is it list X-?
        jp    nz,e1

        call    gc              ;yes
        ld    bc,-1
        cp      cr              ;is it list X-X?
        jr      z,clst3         ;no
        jr      clst2

clst1::  call    gci         ;get rid of char
clst2::  push    de
        call    intger
        pop     de
        jp      c,e1

        ld      c,l
        ld      b,h

clst3::  ld      a,[bofa]
        ld      l,a
        ld      a,[bofa+1]
        ld      h,a

clst4::  ld    a,[hl]
        dec     a               ;is a program present?
        ret     z               ;no, exit

        inc    hl
        call    dcmp
        dec    hl         ;point to count char again
        jp      c,clst5
        jp      z,clst5

; inc to next line
        ld    a,[hl]
        call    aa2hl

        jr      clst4

clst5::
        ld      e,c          ;mark last line to list
        ld      d,b

clst6::  inc    hl
        call    dcmp
        dec    hl         ;point to char count
        jr      c,clstx      ;exit

        push    de
        ld    de,ibuf         ;area for unprocessing
        call    uppl
        inc    hl
        push    hl
        ld    hl,ibuf
        call    prntcr
        call    crlf
        pop    hl
        pop    de

        push    hl
        ld      a,[miscW1]
        ld      l,a
        ld      a,[miscW1+1]
        ld      h,a

        call    getbuts
        push    af
        bit     subit,a       ;speed up button pressed?
        jr      z,clst7       ;no

        dec     hl
        ld      a,h
        or      l             ;does hl = 1 ?
        inc     hl
        jr      z,clst7       ;yes, already at max speed

        srl     h             ;hl=hl/2
        rr      l

clst7::
        pop     af
        bit     sdbit,a       ;slow down button pressed?
        jr      z,clst8       ;no

        add     hl,hl         ;hl=hl*4
        add     hl,hl
clst8::
        and     BRKBTN
        cp      BRKBTN          ;break pressed?
        jr      z,clst9         ;yes

        ld      a,l
        ld      [miscW1],a
        ld      a,h
        ld      [miscW1+1],a
        push    de
        ld      e,l
        ld      d,h
        call    dely1
        pop     de
        pop     hl

        ld      a,[hl]
        dec     a               ;end of program?
        jr      nz,clst6        ;not yet
clstx::
        jp      bend
clst9::
        pop     hl
        jr      clstx

;
;
; "Locate"
locat::
        call    exprb   ;get y coordinate
        call    pfix
        ld      c,e
        push    bc

        ld      b,","
        call    eatc

        call    exprb   ;get x coordinate
        call    pfix

        pop     bc
        ld      b,e

        jp      locate

;
; "Poke"
poke::
        call    exprb   ;get address
        call    pfix
        push    de

        ld      b,","
        call    eatc

        call    exprb   ;get data
        call    pfix

        ld      a,d
        or      a       ;is data > 255 ?
        jp      nz,e5   ;yes, Out of Range error

        ld      a,e
        pop     de
        ld      [de],a  ;write byte
        ret

; "load"
loadp::
        ld      a,CART_SRAM_ENABLE
        ld      [rRAMG],a           ;enable sram

        ld      hl,_SRAM+7

        ld      a,[_SRAM+5]
        ld      c,a
        ld      a,[_SRAM+6]
        and     $7f             ;remove auto-run bit
        ld      b,a

        call    calccrc         ;file okay?
        jr      c,loaderr       ;no

        ld      a,[bofa]
        ld      e,a
        ld      a,[bofa+1]
        ld      d,a

        call    move

        call    findeof         ;set eofa

        xor     a
        ld      [rRAMG],a           ;disable ram

        jp      cclear

loaderr::
        xor     a
        ld      [rRAMG],a           ;disable ram

        call    ilprc
        DB   "Corrupt program",0
        ret

; "save"
save::
        ld      a,CART_SRAM_ENABLE
        ld      [rRAMG],a           ;enable sram

        ld      hl,bofa
        ld      de,eofa
        ld      a,[de]
        sub     [hl]
        ld      c,a
        inc     de
        inc     hl
        ld      a,[de]
        sbc     a,[hl]
        ld      b,a
        inc     bc

        ld      a,c
        ld      [_SRAM+5],a
        ld      a,b
        ld      [_SRAM+6],a

        ld      a,[bofa]
        ld      l,a
        ld      a,[bofa+1]
        ld      h,a

        ld      de,_SRAM+7

        call    move

        ld      a,"B"           ;Basic File indicator
        ld      [_SRAM],a
        ld      a,"F"
        ld      [_SRAM+1],a
        ld      a,"0"
        ld      [_SRAM+2],a

        ld      a,[miscW1]
        ld      [_SRAM+3],a
        ld      a,[miscW1+1]
        ld      [_SRAM+4],a

        xor     a
        ld      [rRAMG],a
        ret

;Compare file with it's crc
;Set carry if no match
calccrc::
        push    bc
        push    hl
        ld      a,[_SRAM]
        cp      "B"
        jr      nz,calcc3       ;error
        ld      a,[_SRAM+1]
        cp      "F"
        jr      nz,calcc3       ;error
        ld      a,[_SRAM+2]
        cp      "0"
        jr      nz,calcc3       ;error

        ld      de,0
calcc1:: ld      a,[hl]

        push    hl
        ld      l,a
        ld      h,0
        add     hl,de
        ld      e,l
        ld      d,h
        pop     hl

        inc     hl
        dec     bc
        ld      a,b
        or      c
        jr      nz,calcc1

        ld      a,[_SRAM+3]
        cp      e               ;does crc check okay?
        jr      nz,calcc3       ;no
        ld      a,[_SRAM+4]
        cp      d               ;does crc check okay?
        jr      nz,calcc3       ;no
        or      a
        jr      calcc4

calcc3:: scf
calcc4:: pop     hl
        pop     bc
        ret

;Move BC bytes from HL to DE
move::   xor     a
        ld      [miscW1],a
        ld      [miscW1+1],a
mov1::   ld      a,[hl]
        ld      [de],a

        ld      a,[miscW1]
        add     a,[hl]
        ld      [miscW1],a
        ld      a,[miscW1+1]
        adc     a,0
        ld      [miscW1+1],a

        inc     hl
        inc     de
        dec     bc
        ld      a,b
        or      c
        jr      nz,mov1
        ret

; "free"
free::
        ld      a,[mata]        ;Upward growing matrix storage
        ld      l,a
        ld      a,[mata+1]
        ld      h,a

        ld      a,[stb]         ;Downward growing variable storage
        sub     l
        ld      l,a
        ld      a,[stb+1]
        sbc     a,h
        ld      h,a

        ld      de,cnsbuf
        call    cns

        ld      a,term
        ld      [de],a          ;terminate number string

        ld      hl,cnsbuf
        call    prnt

        call    ilprc
        DB   " bytes left.",0
        ret

;
; "on"
;on::
;        call    exprb           ;get expression
;        call    pfix            ;convert to integer
;        ld      c,e
;
;        ld      a,d
;        or      a               ;is expr > 255?
;        jp      z,rem           ;yes, ignore rest of line
;
;        call    gci
;        ld      b,a
;        cp      gotorw          ;is it a goto?
;        jr      z,on1           ;yes
;        cp      gosubrw         ;is it a gosub?
;        jp      nz,e1           ;no
;
;on1::    gln                     ;line number present?
;        jp      c,e1            ;no, error
;
;        dec     c               ;have we got the right line# ?
;        jr      z,ondo          ;yes
;
;        call    gc
;        cp      ","             ;comma?
;        ret     nz              ;no
;
;        call    gci
;        jr      on1
;
;ondo::   ld      a,b
;        cp      gotorw          ;goto request?
;        jp      z,goto1         ;yes
;
;
;        ld      de,-3        ;create control stack entry
;        call    pshcs
;        push    hl           ;save stack addr
;
;        call    gln
;        jp      c,e1         ;no line # present
;
;        ld      e,l          ;line number in de
;        ld      d,h
;
;        call    joe
;        ld      b,h
;        ld      c,l
;        pop     hl           ;stack addr
;        ld      [hl],b       ;stack return addr returned by joe
;        dec     hl
;        ld      [hl],c
;        dec     hl
;        ld      [hl],gtype   ;make control stack entry type 'gosub'
;        call    findln
;        inc     hl
;        inc     hl
;        inc     hl
;        jp      next6

;
; "renum"
renum::

        ld      a,[eofa]
        ld      e,a
        ld      a,[eofa+1]
        ld      d,a

        inc     de

        ld      a,[bofa]
        ld      l,a
        ld      a,[bofa+1]
        ld      h,a

        ld      a,[hl]
        dec     a               ;is there a program to renumber?
        jp      z,bend          ;no

; Build lookup table
ren0::   ld      a,[hl]
        dec     a               ;have we reached end of program?
        jr      z,ren2          ;yes

        push    hl
        inc     hl
        call    lhli

        push    de
        inc     de
        inc     de
        inc     de
        inc     de

        push    hl
        ld    hl,memtop
        call    dcmp            ;is table too large?
        jp      nc,e8           ;yes, out of memory

        pop     hl
        pop     de

        ld    a,l
        ld    [de],a
        inc    de
        ld    a,h
        ld    [de],a
        inc    de

        pop    hl

        ld    a,[hl]
        call    aa2hl
        jr      ren0

ren2::   xor     a           ;end of table marker
        ld    [de],a
        inc    de
        ld    [de],a

        ld    bc,10

        ld      a,[bofa]
        ld      l,a
        ld      a,[bofa+1]
        ld      h,a

ren3::   ld    a,[hl]
        dec     a               ;have we renumbered whole program?
        jp      z,bend          ;yes

        push    hl
        inc    hl
        ld    [hl],c
        inc    hl
        ld    [hl],b

ren4::   inc     hl
        ld      a,[hl]
        cp      cr              ;end of line?
        jr      z,ren9          ;yes

        cp      linent          ;line number token?
        jr      nz,ren4         ;no

        inc     hl
        ld      e,[hl]
        inc     hl
        ld      d,[hl]

        ld      a,c             ;save line number for errors in conversion
        ld      [ibuf],a
        ld      a,b
        ld      [ibuf+1],a

        call    cnvtln          ;convert de

        ld      [hl],d
        dec     hl
        ld      [hl],e
        inc     hl
        jr      ren4

ren9::
        pop     hl

        ld      a,[hl]
        call    aa2hl

;increment line number by 10

        ld    a,10
ren10::  inc     bc
        dec    a
        jr      nz,ren10

        jr      ren3

; Convert de from old to new line number
cnvtln::

        push    bc
        push    hl

        ld      bc,10

        ld      a,[eofa]
        ld      l,a
        ld      a,[eofa+1]
        ld      h,a

cnvtl1::
        inc     hl
        xor     a
        cp      [hl]            ;end of table?
        jr      nz,cnvtl2       ;no
        inc     hl
        cp      [hl]
        dec     hl              ;end of table?
        jr      z,cnvtl9        ;yes

cnvtl2::
        ld      a,e
        cp      [hl]            ;lsb match?
        inc     hl
        jr      nz,cnvtl6       ;no
        ld      a,d
        cp      [hl]            ;msb match?
        jr      nz,cnvtl6       ;no

        ld      e,c
        ld      d,b
        jr      cnvtl8

cnvtl6::
        ld    a,10

cnvt17::
        inc     bc
        dec    a
        jr      nz,cnvt17

        jr      cnvtl1

cnvtl8::
        pop     hl
        pop     bc
        ret

; Undefined Line Number x in x.
cnvtl9::
        push    de
        push    bc
        ld      hl,ermln
        call    prnt
        call    space

        ld      l,e
        ld      h,d

        ld      de,cnsbuf
        call    cns
        ld      a,cr
        ld      [de],a
        ld      hl,cnsbuf
        call    prntcr

        ld      hl,ins
        call    prnt

        ld      a,[ibuf]
        ld      l,a
        ld      a,[ibuf+1]
        ld      h,a
        ld      de,cnsbuf
        call    cns
        ld    a,cr
        ld    [de],a
        ld      hl,cnsbuf
        call    prntcr

        call    crlf

        pop     bc
        pop     de
        jr      cnvtl8
;
; "run"
crun::   call    cclear
        call    def_color       ;setup default drawing color

        ld      a,[bofa]
        ld      l,a
        ld      a,[bofa+1]
        ld      h,a

        ld    a,[hl]
        dec    a         ;check for null program
        jp    z,bend

        call    resto4          ;update rtxa

        ld      a,l
        ld      [txa],a
        ld      a,h
        ld      [txa+1],a

        xor    a
        ld    [dirf],a     ;clear direct flag and fall through to driver

        jp      iloop

; interpret statement located by txa
istat::  call    gc         ;get first non blank
        cp      39         ;is it a "'" ?
        jp      z,rem         ;yes

        cp      128
        jp      c,let        ;must be let if not rw

        cp      irwlin
        jp      nc,e1        ;this token not allowed initially

        ld      de,cmndd     ;statement dispatch table base
ista1::  call    gci         ;advance text pointer
        and     $7f
        rlca             ;multiply by two preparing for table lookup
        ld      l,a
        ld      h,0
        add     hl,de
        call    lhli
        jp      hl         ;branch to statement or command

;
;    statements
;

; "let"
let::
        call    var          ;check for variable
        jp      c,e1         ;not found

        push    hl         ;save value address

        ld    b,eqrw
        call    eatc

        call    exprb
        pop    de         ;destination address
        jp      popa1        ;copy expr value to variable

; "for"
sfor::   call    dirt
        call    var         ;control variable
        jp      c,e4            ;not found

        push    hl         ;control variable value address
        ld      b,eqrw
        call    eatc

        call    exprb         ;initial value
        pop     de         ;variable value address
        push    de         ;save
        call    popa1         ;set initial value
        ld      b,torw         ;rw for 'to'
        call    eatc
        call    exprb         ;limit value computation
        call    gc         ;check next character for possible step
        cp      steprw
        jr      z,for1

; use step of 1
        ld      de,fpone
        call    psha1
        jr      for2

; compute step value
for1::   call    gci         ;eat the step rw
        call    exprb         ;the step value

; here the step and limit are on arg stack
for2::   ld    de,-2         ;prepare to allocate 2 bytes on control stack
        call    pshcs         ;returns address of those 2 bytes in hl

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        call    joe         ;test for junk on end
        jp    c,e4         ;no "for" statement at end of program

        push    hl           ;de has loop text addr, hl has control stack adr
        ld      l,e
        ld      h,d
        pop     de

        ld    [hl],d         ;high order text address byte
        dec    hl
        ld    [hl],e         ;low   "
        ld    de,-fpsiz    ;allocate space for limit on control stack
        call    pshcs
        push    hl         ;addr on control stack for limit
        ld    de,-fpsiz    ;allocate space for step on control stack
        call    pshcs
        call    popas         ;copy step value to control stack
        pop    de         ;control stack addr for limit value
        call    popa1         ;limit value to control stack
        ld    de,-3         ;allocate space for text addr & cs entry
        call    pshcs
        pop    de         ;control variable addr
        ld    [hl],d         ;high order byte of control variable addr
        dec    hl
        ld    [hl],e         ;low   "
        dec    hl
        ld    [hl],ftype   ;set control stack entry type for 'for'
        jp    next5         ;go finish off carefully

; "next"
next::   call    dirt

        ld      a,[cstka]    ;control stack addr
        ld      l,a
        ld      a,[cstka+1]
        ld      h,a

        ld    a,[hl]         ;stack entry type byte
        dec    a         ;must be for type else error
        jp    nz,e4         ;improper nesting error

        inc    hl         ;control stack pointer to control var addr
        push    hl
        call    var         ;check variable, in case user wants
        jr      c,next1      ;skip check if var not there

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        pop    hl         ;control variable addr
        push    hl         ;save it again
        call    dcmp
        jp    nz,e4         ;improper nesting if not the same

next1::  pop    hl         ;control variable addr
        push    hl
        push    hl
        ld    de,fpsiz+2-1 ;compute addr to step value
        add    hl,de
        EX_SP_HL             ;now addr to var in hl
        call    lhli         ;var addr
        ld    b,h         ;copy var addr to bc
        ld    c,l
        pop    de         ;step value addr
        push    de
        call    fadd         ;do increment
        pop    hl         ;step value
        dec    hl         ;point to sign of step value
        ld    a,[hl]         ;sign 0=pos, 1=neg
        ld    de,fpsiz+1
        add    hl,de         ;puts limit addr in hl

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        pop    hl         ;var addr
        call    lhli         ;get addr
        push    de         ;save control stack pointer to get text address
        or    a         ;set conditions based on sign of step value
        jr      z,next2      ;reverse test on negative step value

        push    hl
        ld      l,e
        ld      h,d
        pop     de

next2::  ld    b,h         ;set up args for compare
        ld    c,l
        call    relop         ;test <=
        pop    de         ;test addr
        jr      nc,next3     ;still smaller?
        jr      z,next3      ;jump if want to continue loop

; terminate loop
        ld    hl,3         ;remove cstack entry
        add    hl,de
        ld      a,l
        ld      [cstka],a
        ld      a,h
        ld      [cstka+1],a
        ret

next3::  inc    de         ;test addr

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        call    lhli         ;get text address in hl

; iterate, skipping normal junk on end test at iloop
next4::  push    hl           ;save new text addr in de
        ld      l,e
        ld      h,d
        pop     de

        call    joe

        push    hl
        ld      l,e
        ld      h,d
        pop     de

next6::  ld      a,l
        ld      [txa],a
        ld      a,h
        ld      [txa+1],a
next5::
        pop     hl
        jp      iloop        ;to dispatcher skipping joe call there

; "if"
sif::    ld      b,1          ;specify principal operator is relational
        call    expb1

        ld      a,[astka]   ;addr of boolean value on arg stack
        ld      l,a
        ld      a,[astka+1]
        ld      h,a

        inc    [hl]         ;sets zero condition if relational was true
        push    af         ;save conditions to test later
        call    popas         ;remove value from arg stack copy to self
        pop    af
        jp    nz,rem         ;if test false treat rest of line as rem

; test succeeded
        ld    b,thenrw
        call    eatc

        call    gln          ;check if line number is desired action
        jp      c,istat      ;no, must be a command

        jr      goto1

; "goto"
sgoto::  xor    a
        ld    [dirf],a     ;clears direct statement flag

        call    getbuts      ;break buttons pressed?
        and     BRKBTN
        cp      BRKBTN
        jp      z,iloopb     ;yes

        call    gln          ;returns integer in hl if line # present
        jp      c,e1         ;syntax error - no line error

goto1::  ld      e,l          ;line # in de
        ld      d,h

        call    findln         ;returns text address points to count value
goto2::  inc    hl
        inc    hl
        inc    hl         ;advance text pointer past line # and count
        jp    next4

; "gosub"
gosub::  call    dirt
        ld    de,-3         ;create control stack entry
        call    pshcs
        push    hl         ;save stack addr

        call    gln
        jp      c,e1         ;no line # present

        ld      e,l          ;line number in de
        ld      d,h

        call    joe
        ld    b,h
        ld    c,l
        pop    hl         ;stack addr
        ld    [hl],b         ;stack return addr returned by joe
        dec    hl
        ld    [hl],c
        dec    hl
        ld    [hl],gtype   ;make control stack entry type 'gosub'
        call    findln
        inc    hl
        inc    hl
        inc    hl
        jp    next6

; "return"
retrn::  call    dirt
        ld    [dirf],a     ;clears dirf if acc is clear

        ld      a,[cstka]
        ld      l,a
        ld      a,[cstka+1]
        ld      h,a

ret1::   ld    a,[hl]
        or    a         ;check for stack empty
        jp    z,e4

        cp    gtype         ;check for gosub type
        jr      z,ret2

; remove for type from stack
        ld    de,forsz
        add    hl,de
        jr      ret1

; found a gtype stack entry
ret2::   inc    hl
        ld    e,[hl]         ;low order text address
        inc    hl
        ld    d,[hl]         ;high   "
        inc    hl         ;addr of previous control stack entry

        ld      a,l
        ld      [cstka],a
        ld      a,h
        ld      [cstka+1],a

        push    hl           ;put text addr in hl
        ld      l,e
        ld      h,d
        pop     de

        ld    a,[hl]         ;addr points to eof if gosub was last line
        dec    a         ;end of file?
        jp      nz,next4     ;no

        jp    bend

; "data" and "rem"
data::    call    dirt         ;data statement illegal as direct
rem::    call    gci
        cp    cr
        jr      nz,rem

rem1::   dec     hl           ;backup pointer so normal joe will work

        ld      a,l
        ld      [txa],a
        ld      a,h
        ld      [txa+1],a
        ret

; "dimension"
dim::    call    name1         ;look for variable name
        jp      c,e3         ;no variable name error

        ld      a,c          ;prepare turn on high bit to signify matrix
        or      $80
        ld    c,a
        call    stlk
        jp    nc,e6         ;error if name already exists

        push    hl         ;symbol table addr
        ld    b,lparrw
        call    eatc
        call    exprb
        ld    b,")"
        call    eatc
        call    pfix         ;return integer in de
        ld    hl,matub     ;max size for matrix
        call    dcmp
        jp      nc,e8        ;matrix too large error

        pop    hl         ;symbol table address
        call    dims
        call    gc         ;see if more to do
        cp    ","
        ret    nz

        call    gci         ;eat the comma
        jr      dim

; "stop"
_stop::    call    dirt
;        call    crlf2
stop1::  ld      hl,stops
        jp    erm1

; "end"
bend: MACRO
  cmnd1
ENDM

; "read"
read::   call    dirt

        ld      a,[txa]
        ld      l,a
        ld      a,[txa+1]
        ld      h,a

        push    hl         ;save txa temporarily
        ld      a,[rtxa]     ;the 'read' txa
        ld      l,a
        ld      a,[rtxa+1]
        ld      h,a

read0::  ld      a,l
        ld      [txa],a
        ld      a,h
        ld      [txa+1],a

        call    gci
        cp      ","             ;comma?
        jr      z,read2         ;yes, process input value
        cp    datarw
        jr      z,read2
        dec     a               ;end of file?
        jr      z,read4         ;yes

; skip to next line
        call    rem         ;leaves addr to last cr in hl
        inc    hl
        ld    a,[hl]
        dec    a
        jr      z,read4

        inc    hl
        inc    hl
        inc    hl         ;hl now points to first byte of next line
        jr      read0

; process value
read2::  call    exprb
        call    gc
        cp    ","         ;skip joe test if comma
        jr      z,read3

; junk on end test
        call    joe

read3::  ld      a,[txa]
        ld      l,a
        ld      a,[txa+1]
        ld      h,a

        ld      a,l
        ld      [rtxa],a     ;save new "read" text addr
        ld      a,h
        ld      [rtxa+1],a

        pop    hl         ;real txa

        ld      a,l
        ld      [txa],a
        ld      a,h
        ld      [txa+1],a

        call    var
        jp    c,e1

        call    popas         ;put read value into variable
        call    gc
        cp    ","         ;check for another variable
        ret    nz

        call    gci         ;eat the comma
        jr      read

read4::  pop    hl         ;program txa

        ld      a,l
        ld      [txa],a
        ld      a,h
        ld      [txa+1],a

        ld      hl,ermrd        ;7264h 'rd'
        jp    error

; "restore"
restor::
        call    gln          ;returns integer in hl if line # present
        jp      c,resto3     ;no line number present

resto1:: ld      e,l          ;line # in de
        ld      d,h

        call    findln         ;returns text address points to count value
        jr      resto4

resto3:: ld      a,[bofa]     ;beginning of file pointer
        ld      l,a
        ld      a,[bofa+1]
        ld      h,a

; update rtxa

resto4:: inc     hl           ;advance text pointer past line # & count
        inc    hl
        inc    hl

        ld      a,l
        ld      [rtxa],a
        ld      a,h
        ld      [rtxa+1],a
        ret

; "print"
print:: call    gc
        cp    cr         ;check for stand alone print
        jp    z,crlf

prin9::  cp    "\""
        jr      z,pstr       ;print the string

        cp      tabrw
        jr        z,ptab       ;tabulation

        cp      "%"
        jp      z,pform         ;set format

        cp      cr
        ret      z
        cp      ":"
        ret      z

        call    exprb         ;must be expression to print

        ld      de,fpsink
        call    popa1         ;pop value to fpsink

;        ld      a,[phead]
;        cp      56
;        call    nc,crlf      ;do crlf if print head is past 56

        ld      hl,fpsink
        call    fpout

        ld      b," "
        call    chout
pr1::   call    gc         ;get delimiter
        cp      ";"        
        jp      nz,crlf

pr0::   call    gci
        call    gc
        jr      prin9

pstr::  call    gci         ;gobble the quote
        call    prnt         ;print up to double quote
        inc     hl         ;move pointer past double quote

        ld      a,l
        ld      [txa],a
        ld      a,h
        ld      [txa+1],a

        jr      pr1

pform:: ld      a,2*fpnib
        ld      [infes],a
        call    gci         ;gobble previous char
pfrm1:: call    gci
        ld      hl,infes
        cp      "%"         ;delimiter
        jr      z,pr1

        ld      b,$80
        cp      "z"         ;trailing zeros?
        jr      z,pf1

        ld      b,1
        cp      "e"         ;scientific notation?
        jr      z,pf1

        call    nmchk
        jp      nc,e1

        sub     "0"         ;number of decimal places
        rlca
        ld      b,a
        ld      a,[hl]
        and     $c1
        ld      [hl],a
pf1::   ld      a,[hl]
        or      b
        ld      [hl],a
        jr      pfrm1

ptab::  call    gci         ;gobble tab rw
        ld      b,lparrw
        call    eatc
        call    exprb
        ld    b,")"
        call    eatc
        call    pfix

ptab1::  ld      a,[phead]
        cp      e
        jr      nc,pr1

        ld    b," "
        call    chout
        jr      ptab1

; "input"
input::  call    gc
        cp    ","
        jp    z,ncrlf

        call    crlf
inp0::   ld    b,"?"
        call    chout
linp::   call    inline
        ld    de,ibuf
in1::    push    de         ;save for fpin

        call    var
        jp    c,e1

        pop    de
        ld    b,0
        ld    a,[de]
        cp    "+"         ;look for leading plus or minus on input
        jr      z,in2

        cp    "-"
        jr      nz,in3

        ld    b,1
in2::    inc    de
in3::    push    bc
        push    hl
        call    fpin         ;input fp number
        jp    c,inerr

        pop    hl
        dec    hl
        pop    af
        ld    [hl],a
        call    gc
        cp    ","
        ret    nz         ;done if no more

        call    gci         ;eat the comma
        ld    a,b         ;get the terminator to a
        cp    ","
        jr      z,in1        ;get the next input value from string

; get new line from user
        ld    b,"?"
        call    chout
        jr      inp0

ncrlf::  call    gci
        jr      linp         ;now get line

inerr::  ld      hl,ermin        ;696eh 'in'
        jp    error

;
;    evaluate an expression from text
; hl take op table addr of previous operator (not changed]
; result value left on top of arg stack, argf left true
;
exprb::  ld    b,0
expb1::  ld    hl,opbol
        xor    a
        ld    [reltyp],a

; zero in b means principal operator may not be relational
expr::   push    bc
        push    hl         ;push optba
        xor    a
        ld    [argf],a
expr1::  ld    a,[argf]
        or    a
        jr      nz,expr2

        call    var             ;is there a variable?
        call    nc,pshas        ;yes, push onto arg stack
        jr      nc,expr2

        call    const           ;is there a fp constant?
        jr      nc,expr2        ;yes

        call    gc
        cp      lparrw          ;is there a ( ?
        ld    hl,oplpar
        jp      z,xlpar         ;yes

; isn't or shouldn't be an argument
expr2::  call    gc
        cp      $e0         ;check for reserved word operator
        jr      nc,xop          ; e0 or >

        cp      $c0         ;check for built in function
        jp      nc,xbilt        ; c0 - df

; illegal expression character
        pop    hl         ;get optaba
        ld    a,[argf]
        or    a
        jp    z,e1

xdon1::  pop    af
        ld    hl,reltyp    ;check if legal principal operation
        cp    [hl]
        ret    z

        jp    e1

xop::    and     $1f          ;cleans off rw bits

        push    af
        ld      a,[argf]    ;test for argf true
        ld      l,a
        ld      a,[argf+1]
        ld      h,a
        pop     af

        dec    l
        jr      z,xop1

; argf was false, unary ops only possibility
        cp    "-"-opbase
        jr      z,xopm

        cp    "+"-opbase
        jp    nz,e1

        call    gci         ;eat the "+"
        jr      expr1

xopm::   ld    a,uminu-opbase
xop1::   call    opadr
        pop    de         ;previous optba
        ld    a,[de]
        cp    [hl]
        jr      nc,xdon1     ;non-increasing precedence

; increasing precedence case
        push    de         ;save previous optba
        push    hl         ;save current optba
        call    gci         ;to gobble operator
        pop    hl
        push    hl
        ld    b,0         ;specify non-relational
        call    expr
        pop    hl

; hl has optba addr
; set up args and perform operation action
xop2::   push    hl
        ld    a,[hl]

        push    af
        ld      a,[astka]
        ld      l,a
        ld      a,[astka+1]
        ld      h,a
        pop     af

        ld    b,h
        ld    c,l
        and    1
        jr      nz,xop21

; decrement stack pointer by one value binary case
        ld    de,fpsiz
        add    hl,de

        push    af
        ld      a,l
        ld      [astka],a
        ld      a,h
        ld      [astka+1],a
        pop     af

        ld    d,h
        ld    e,l
xop21::  ld      hl,expr1
        EX_SP_HL             ;change return link
        inc    hl         ;skip over precidence
        call    lhli         ;load action address
        jp    hl

;
;    action routine convention
; de left arg and result for binary
; bc right arg for binary, arg and result for unary
; built in function processing
;
xbilt::  call    gci         ;eat token
        and     $3f          ;clean off rw bits

        push    af
        ld      a,[argf]     ;built in function must come after operator
        ld      l,a
        ld      a,[argf+1]
        ld      h,a
        pop     af

        dec    l
        jp    z,e1

        call    opadr         ;optba to hl

xlpar::  push    hl
        ld    b,lparrw
        call    eatc

        call    exprb

        ld    b,")"
        call    eatc

        pop    hl         ;code for built-in function
        jr      xop2

; compute optable address for operator in acc
opadr::  ld    c,a
        ld    b,0
        ld    hl,optab
        add    hl,bc
        add    hl,bc
        add    hl,bc         ;optab entry addr is 3*op+base
        ret
;
; preprocessor, un-preprocessor
; preprocess line in ibuf back into ibuf
; sets carry if line has no line number
; leaves correct length of line after preprocessing in ibcn
; if there is a line number, it is located at ibln=ibuf-2
; txa is clobbered
;
pp::     ld    hl,ibuf         ;first character of input line

        ld      a,l
        ld      [txa],a      ;so gci will work
        ld      a,h
        ld      [txa+1],a

        call    intger       ;sets carry if no line number

        push    af         ;save state of carry bit for returning

        ld      a,l
        ld      [ibln],a     ;store line number value (even if none]
        ld      a,h
        ld      [ibln+1],a

        ld      a,[txa]      ;addr of next char in ibuf
        ld      l,a
        ld      a,[txa+1]
        ld      h,a

        ld    c,4         ;set up initial value for count
        ld    de,ibuf         ;initialize write pointer

; come here to continue preprocessing line
ppl::    push    de
        ld    de,rwt         ;base of rwt
ppl1::   push    hl         ;save text addr
        ld    a,[de]         ;rw value for this entry in rwt
        ld    b,a         ;save in b in case of match
ppl2::   inc    de         ;advance entry pointer to next byte
        call    hl2lower
        ld    a,[de]         ;get next char from entry
        cp    [hl]         ;compare with char in text
;        jr      z,ppl0
;        and     0dfh         ;see if case different
;        cp      [hl]
        jr      nz,ppl3
ppl0::   inc     hl           ;advance text pointer
        jr      ppl2         ;continue comparison

; come here when comparison of byte failed
ppl3::
;        or      20h
        cp      128
        jr      nc,ppl6       ;jump if found match

; scan to beginning of next entry
ppl4::   inc    de         ;advance entry pointer
        ld    a,[de]         ;next byte is either char or rw byte
        cp      128
        jr      c,ppl4       ;keep scanning if not rw byte

; now see if at end of table, and fail or return condition
        pop    hl         ;recover original text pointer
        xor    255         ;check for end of table byte
        jr     nz,ppl1      ;continue scan of table

; didn't find an entry at the given text addr
        pop    de
        ld    a,[hl]         ;get text char
        cp    cr         ;check for end of line
        jr      z,ppl88      ;go clean up & return

        ld    [de],a
        inc    de
        inc    c
        inc    hl         ;advance text pointer
        cp    "\""         ;check for quoted string possibility
        jr      nz,ppl       ;restart rwt search at next character position

; here we have a quoted string, so eat till endquote
ppl5::   ld    a,[hl]         ;next char
        cp    cr
              jr      z,ppl88      ;no string endquote, let interpreter worry

        ld    [de],a
        inc    de
        inc    c
        inc    hl         ;advance text pointer
        cp    "\""
        jr      z,ppl        ;begin rwt scan from new character position
        jr      ppl5

; found match so put rw value in text
ppl6::   pop    af         ;remove unneeded test pointer from stack
        pop    de
        ld    a,b
        ld    [de],a
        inc    de
        inc    c

        cp      gotorw          ;is it a goto?
        jr      z,ppl7          ;yes
        cp      gosubrw         ;is it a gosub?
        jr      z,ppl7          ;yes
        cp      restorw         ;is it a restore?
        jr      z,ppl7          ;yes
        cp      thenrw          ;is it a then?
        jr      z,ppl7          ;yes

        jr      ppl

;look for line number to compress
ppl7::   push    hl

        ld      a,l
        ld      [txa],a
        ld      a,h
        ld      [txa+1],a

        push    bc
        push    de
        call    intger          ;carry set if no line number
        pop     de
        pop     bc
        jr      c,ppl79

        pop     af

;        ld      a," "
;        ld      [de],a
;        inc     de
;        inc     c

        ld      a,linent
        ld      [de],a
        inc     de
        inc     c

        ld      a,l
        ld      [de],a
        inc     de
        inc     c

        ld      a,h
        ld      [de],a
        inc     de
        inc     c

        ld      a,[txa]
        ld      l,a
        ld      a,[txa+1]
        ld      h,a

        jp      ppl

;        ld      a,[hl]
;        cp      ","     ;is this a ON x GOSUB x,x,x?
;        jp      nz,ppl
;
;        inc     hl
;        ld      [de],a
;        inc     de
;        inc     c
;
;        jp     ppl7


ppl79::  pop     hl


        jp      ppl

ppl80::  pop     hl

; come here when done
ppl88::  ld      a,cr
        ld    [de],a
        ld      hl,ibcnt     ;set up count in case line has line number
        ld    [hl],c
        pop    af         ;restore carry condition (line number flag]
        ret

; Print contents of HL to screen.
;trace::
;        push    af
;        push    bc
;        push    de
;        push    hl
;
;        ld      de,cnsbuf      ;use ibuf to accumulate the line line number string
;        call    cns
;        ld      a,cr
;        ld      [de],a
;        ld      hl,cnsbuf
;        call    prntcr
;
;        pop     hl
;        pop     de
;        pop     bc
;        pop     af
;        ret


hl2lower::
        ld      a,[hl]
        cp      "A"
        ret     c
        cp      "Z"+1
        ret     nc
        or      $20
        ld      [hl],a
        ret
;
; un-preprocess line addr in hl to de buffer
; return source addr of cr in hl on return
;
uppl::   inc    hl         ;skip over count byte
        push    hl         ;save source text pointer
        call    lhli         ;load line # value
        call    cns          ;convert line #
        ld    a," "
        ld    [de],a         ;put blank after line number
        inc    de         ;increment dest pointer
        pop    hl
        inc    hl         ;     "    source  "
upp0::   inc    hl
        ld    a,[hl]         ;next token in source
        cp      128
        jr      nc,upp2      ;jump if token is rw

        ld    [de],a         ;put char in buffer
        cp    cr         ;check for done
        ret    z

        cp      linent       ;is it a line number token?
        jr      nz,upp1      ;no

        inc     hl
        push    hl
        call    lhli         ;load line # value
        call    cns          ; convert line #
        pop     hl
        inc     hl
        dec     de
upp1::
        inc     de           ;advance dest buffer addr
        jr      upp0

; come here when rw byte detected in source
upp2::   push    hl           ;save source pointer
        ld    hl,rwt         ;base of rwt
upp3::   cp      [hl]         ;see if rw matched rwt entry
        inc    hl         ;advance rwt pointer
        jr      nz,upp3      ;continue looking if not found

; found match, entry pointer locates first char
upp4::   ld      a,[hl]       ;char of rw
        cp      128          ;check for done
        jr      nc,upp5

        ld    [de],a
        inc    de
        inc    hl
        jr      upp4

; come here if done with rw transfer
upp5::   pop     hl           ;source pointer
        jr      upp0
;
;    constants and tables
;
signon:: DB   "GameBoy Basic V2.10",term
rdys::   DB    "Ok",term
ers::    DB    " error",term
ins::    DB    " in ",term
stops::  DB    "Break",term
trues::   DB    "true",term
falses::  DB    "false",term

;
        DB    -1           ;flags end of sine coefficient list
        DB    0
        DB    1*16
        DW    0
        DB    0

fpone::  DB    129          ;exponent
;    sine coefficient list
; note:: the floating pnt 1 above is part of this table
        DB    $16
        DB    $66
        DB    $67
        DB    1
        DB    128          ;-.166667 e 0 (-1/3]
        DB    $83
        DB    $33
        DB    $33
        DB    0
        DB    128-2        ;.833333 e-2 (1/5]
        DB    $19
        DB    $84
        DB    $13
        DB    1
        DB    128-3        ;-.198413 e-3 (-1/7]
        DB    $27
        DB    $55
        DB    $73
        DB    0
        DB    128-5        ;.275573 e-5 (1/9]
        DB    $25
        DB    $5
        DB    $21
        DB    1
sinx::   DB    128-7        ;-.250521 e-7 (-1/11]
;    cosine coefficient list
        DB    -1           ;marks end of list
        DB    0
        DB    $10
        DB    0
        DB    0
        DB    0
        DB    128+1        ;.100000 e 1 (1/1]
        DB    $50
        DB    0
        DB    0
        DB    1
matub::  DB    128          ;-.500000 e 0 (-1/2]
        DB    $41
        DB    $66
        DB    $67
        DB    0
rands::  DB    128-1        ;.416667 e-1 (1/4]
        DB    $13
        DB    $88
        DB    $89
        DB    1
        DB    128-2        ;.138889 e-2 (-1/6]
        DB    $24
        DB    $80
        DB    $16
        DB    0
        DB    128-4        ;.248016 e-4 (1/8]
        DB    $27
        DB    $55
        DB    $73
        DB    1
cosx::   DB    128-6        ;.275573 e-6 (-1/10]
        DB    $20
        DW    0
        DB    0
fptwo::  DB    129
        DB    $15
        DB    $70
        DB    $80
        DB    0
pic2::   DB    128+1        ;pi/2 .157080 e 1
        DB    $63
        DB    $66
        DB    $20
        DB    0
pic1::   DB    128          ;2/pi .636620 e 0
lcstka:: DW    cstkl

        DB    $13
        DB    $10
        DB    $72
        DB    0
snd2::   DB    128+6

;
;       statement table
;
cmndd::  DW    let
        DW    next
        DW    sif
        DW    sgoto
        DW    gosub
        DW    retrn
        DW    read
        DW    data
        DW    sfor
        DW    print
        DW    input
        DW    dim
        DW    _stop
        DW    bend
        DW    restor
        DW    rem
        DW    cclear
        DW    crun
        DW    clist
        DW    new
        DW    abc
        DW    cls
        DW    renum
        DW    locat
        DW    loadp
        DW    save
        DW    free
        DW    poke
        DW    delay
        DW    screen
        DW    set_color
        DW    draw_point
        DW    draw_line
        DW    auto
        DW    sound
        DW    servo
        DW    setLink
        DW    SetRegs
        DW    SetMemTop
        DW    SetSpeed
        DW    SerInit
        DW    SerOut
;
; r/w word table format is reserved word followed by chr
; of reserved word. last entry is followed by 255.
; rw's that are substrings of other rw's (e.g. >) must
; follow the larger word.
;
rwt::    DB    $80
        DB    "let"

        DB    $81
        DB    "next"

        DB    $81
        DB    "n."

        DB    $82
        DB    "if"

gotorw  EQU     $83
        DB    gotorw
        DB    "goto"

        DB    gotorw
        DB    "g."

gosubrw EQU     $84
        DB    gosubrw
        DB    "gosub"

        DB    $85
        DB    "return"

        DB    $86
        DB    "read"

datarw  EQU    $87
        DB    datarw
        DB    "data"

        DB    $88
        DB    "for"

        DB    $88
        DB    "f."

        DB    $89
        DB    "print"

        DB    $89
        DB    "p."

        DB    $89
        DB    "?"

        DB    $8a
        DB    "input"

        DB    $8a
        DB    "i."

        DB    $8b
        DB    "dim"

        DB    $8c
        DB    "stop"

        DB    $8d
        DB    "end"

restorw EQU     $8e
        DB    restorw
        DB    "restore"

        DB    $8f
        DB    "rem"

clrrw   EQU    $90
        DB    clrrw
        DB    "clear"

        DB    $91
        DB    "run"

        DB    $91
        DB    "r."

        DB    $92
        DB    "list"

        DB    $92
        DB    "l."

        DB    $93
        DB    "new"

        DB    $94
        DB    "abc"

        DB    $95
        DB    "cls"

        DB    $96
        DB    "renum"

        DB    $97
        DB    "locate"

        DB    $98
        DB    "load"

        DB    $99
        DB    "save"

        DB    $9a
        DB    "free"

        DB    $9b
        DB    "poke"

        DB    $9c
        DB    "delay"

        DB    $9c
        DB    "d."

        DB    $9d
        DB    "screen"

        DB    $9e
        DB    "color"

        DB    $9f
        DB    "point"

        DB    $a0
        DB    "line"

        DB    $a1
        DB    "auto"

        DB    $a2
        DB    "sound"

        DB    $a3
        DB    "servo"

        DB    $a4
        DB    "link"

        DB    $a5
        DB    "reg"

        DB    $a6
        DB    "memtop"

        DB    $a7
        DB    "speed"

        DB    $a8
        DB    "sinit"

        DB    $a9
        DB    "sout"

irwlin  EQU    $b0         ;last initial reserved word value + 1

steprw  EQU    $b0
        DB    steprw
        DB    "step"

torw    EQU    $b1
        DB    torw
        DB    "to"

thenrw  EQU    $b2
        DB    thenrw
        DB    "then"

        DB    thenrw
        DB    "t."

tabrw   EQU    $b3
        DB    tabrw
        DB    "tab"

lparrw  EQU     "("-opbase+$e0
        DB    lparrw
        DB    "("

        DB    $2a-opbase+$e0        ;*
        DB    "*"

plsrw   EQU     "+"-opbase+$e0
        DB    plsrw
        DB    "+"

minrw   EQU     "-"-opbase+$e0
        DB    minrw
        DB    "-"

        DB    $2f-opbase+$e0        ;/
        DB    "/"

        DB    $37-opbase+$e0
        DB    ">="

        DB    $38-opbase+$e0
        DB    "<="

        DB    $39-opbase+$e0
        DB    "<>"

        DB    $32-opbase+$e0
        DB    "=>"

        DB    $33-opbase+$e0
        DB    "=<"

        DB    $3c-opbase+$e0
        DB    "<"

eqrw    EQU     $3d-opbase+$e0
        DB    eqrw
        DB    "="

        DB    $3e-opbase+$e0
        DB    ">"

        DB    $c1
        DB    "abs"

        DB    $c6
        DB    "int"

        DB    $cd
        DB    "usr"

        DB    $ce
        DB    "rnd"

        DB    $d2
        DB    "sgn"

        DB    $d3
        DB    "sin"

        DB    $c4
        DB    "sqr"

        DB    $d7
        DB    "tan"

        DB    $d8
        DB    "cos"

        DB    $d9
        DB    "peek"

        DB    $da
        DB    "keypad"

        DB    $ff
;
;    operation table
;
oplpar: MACRO
  optab
ENDM
optab::  DB    15
        DW    alpar
        DB    15
        DW    aabs
        DB    10
        DW    amul
        DB    6
        DW    aadd
        DB    15
        DW    asqr
        DB    6
        DW    asub
        DB    15
        DW    aint
        DB    10
        DW    adiv
opbol::  DB    1
        DW    0
        DB    13
        DW    aneg
        DB    4
        DW    age
        DB    4
        DW    ale
        DB    15
        DW    0             ;not used
        DB    15
        DW    acall
        DB    15
        DW    arnd
        DB    4
        DW    age
        DB    4
        DW    ale
        DB    4
        DW    ane
        DB    15
        DW    asgn
        DB    15
        DW    _asin
        DB    4
        DW    alt
        DB    4
        DW    aeq
        DB    4
        DW    agt
        DB    15
        DW    _atan
        DB    15
        DW    _acos
        DB    15
        DW    apeek
        DB    15
        DW    akeypad
;
;    action routines for relational operators
;
agt::    call    relop
        jr      z,rfalse
        jr      nc,rtrue

rfalse::  xor    a
        ld    [de],a
        ret

alt::    call    relop
        jr      z,rfalse
        jr      nc,rfalse

rtrue::  ld    a,255
        ld    [de],a
        ret

aeq::    call    relop
        jr      z,rtrue
        jr      rfalse

ane::    call    relop
        jr      z,rfalse
        jr      rtrue

age::    call    relop
        jr      z,rtrue
        jr      nc,rtrue
        jr      rfalse

ale::    call    relop
        jr      z,rtrue
        jr      nc,rfalse
        jr      rtrue

;    common routine for relational operator action
; left arg addr in de, saved
; right arg addr in bc
; on return nc = gt, zero set=equal
relop::    push    de
        dec    bc
        dec    de
        ld    h,b
        ld    l,c
        ld    a,[de]
        sub    [hl]
        inc    hl
        inc    de
        jr      nz,rlop1     ;test signs of args if different then ret

        ld    bc,fpsink
        call    fsub
        ld    a,[fpsink]   ;check for zero result
        or    a
        jr      z,rlop1

        ld    a,[fpsink-1] ;sign of fpsink
        rlca
        dec    a
rlop1::  push    af
        cp      128
        jr      c,rlop2

        pop     af
        scf
        ccf
        jr      rlop3

rlop2::
        pop     af
        scf
rlop3::
        ld    a,1
        ld    [reltyp],a   ;set reltyp true
        pop    de
        ret
;
;    action routines for arithmetic operators
;        [code wasters]
aadd::   ld    h,b
        ld    l,c
        ld    b,d
        ld    c,e
aadd1::  call    fadd
        jr      fpetst

asub::   ld    h,b
        ld    l,c
        ld    b,d
        ld    c,e
asub1::  call    fsub
        jr      fpetst

amul::   ld    h,b
        ld    l,c
        ld    b,d
        ld    c,e
amul1::  call    fmul
        jr      fpetst

adiv::   ld    h,b
        ld    l,c
        ld    b,d
        ld    c,e
adiv1::  call    fdiv
fpetst:: xor    a
        ld    [reltyp],a
        ld    a,[erri]
        or    a
        ret    z

        ld      a,[astka]   ;zero result on underflow
        ld      l,a
        ld      a,[astka+1]
        ld      h,a

fpet1::  ld    [hl],0
alpar::  ret
;
;    unary and built in function action routines
;
aneg::   ld    a,[bc]
        or    a
        jr      z,aneg1

        dec    bc
        ld    a,[bc]
        xor    1
        ld    [bc],a
aneg1::  xor    a
        ld    [reltyp],a
        ret

aabs::   dec    bc
        xor    a
        ld    [bc],a
        jr      aneg1

asgn::   call    aneg1
        ld    d,b
        ld    e,c
        ld    a,[bc]         ;get exponent
        or    a
        jr      nz,asgn1
        ld    [de],a         ;make argument zero
        ret

asgn1::  dec    bc
        ld    a,[bc]
        or    a
        ld    hl,fpone
        jp      z,vcopy

        ld    hl,fpnone
        jp    vcopy
;
;    compute sin(x) x=top of argument stack
;    return result in place of x
;
_asin::   call    quadc         ;compute quadrant

        ld      a,[astka]
        ld      l,a
        ld      a,[astka+1]
        ld      h,a

        ld    d,h
        ld    e,l
        ld    bc,ftemp
        call    amul1         ;ftemp=x*x
        pop    af
        push    af         ;a=quadrant
        rra
        jr      c,sin10      ;quad odd, compute cosine

;  compute x*p(x*x) -- sine
        ld    de,ftem1

        ld      a,[astka]
        ld      l,a
        ld      a,[astka+1]
        ld      h,a

        call    vcopy         ;ftem1=x*x
        ld    bc,sinx
        call    poly         ;p(x*x]
        call    prepop
        ld    hl,ftem1
        call    amul1         ;x*p(x*x]

;   compute sign of result
; positive for quadrants 0,1. negative for 2,3
; negate above fro negative arguments
sin5::   pop    af         ;quadrant
        ld    b,a
        pop    af         ;sign
        rlca             ;sign, 2 to the 1st bit
        xor    b         ;quadrant, maybe modified for negative arg.

        push    af
        ld      a,[astka]
        ld      l,a
        ld      a,[astka+1]
        ld      h,a
        pop     af

        dec    hl         ;ptr to sign
        sub    2
        cp      128
        ret     nc           ;quadrant 0 or 1
        inc    [hl]         ;else set result negative
        ret

; compute p(x*x) -- cosine
sin10::  ld    bc,cosx
        call    poly         ;p(x*x]
        jr      sin5

sound::
        call    exprb        ;get frequency

        ld      a,[astka]
        ld      l,a
        ld      a,[astka+1]
        ld      h,a

        ld    de,ftemp
        call    vcopy         ;save x in ftemp

        call    prepop
        ld      hl,snd2      ;131072
        call    vcopy        ;put 131072 on stack

        call    prepop
        ld      hl,ftemp     ; tos=131072/ftemp
        call    adiv1

        call    pfix

        ld      a,e          ;de = -de
        cpl
        ld      e,a
        ld      a,d
        cpl
        ld      d,a
        inc     de

        ld      hl,2048      ;hl = 2048 - de
        add     hl,de
        push    hl

        ld      b,","
        call    eatc

        call    exprb
        call    pfix

        pop     hl

        ld      a,d
        or      e               ;is duration 0?
        jr      z,sound1        ;yes

;        ld      a,77h           ;turn sound on
;        ld      [rAUDVOL],a

        ld      a,$ff
        ldh     [rAUDTERM],a

;        ld      a,82h
;        ld      [rAUDENA],a

;        ld      a,84h           ;set sound duty
;        ld      [0ff16h],a

;        ld      a,$f0          ;set envelope
;        ld      [0ff17h],a

        ld      a,l             ;set frequency
        ldh     [rAUD2LOW],a
        ld      a,h
        and     7
        or      $80
        ldh     [rAUD2HIGH],a

        ld      a,d
        cp      $ff             ;is duration 65535?
        jr      z,sound2        ;yes

        call    dely1           ;delay for duration
sound1::
        xor     a               ;turn all sound off
        ldh     [rAUDTERM],a
sound2::
        ret

;
;    compute cos(x) x=top of argument stack
; return result in place of x
; cos(x)=sin(x+pi/2]
;
_acos::   call    prepop
        ld    hl,pic2         ;pi/2
        call    aadd1         ;tos=tos+pi/2
        jp      _asin

;    compute tan(x) x=top of argument stack
; return result in place of x
; tan(x)=sin(x)/cos(x]
;
_atan::   ld      a,[astka]
        ld      l,a
        ld      a,[astka+1]
        ld      h,a

        call    pshas         ;push copy of x onto arg stack
        call    _acos         ;cos(x]
        ld    de,ftem2
        call    popa1         ;ftem2=cos(x]
        call    _asin
        call    prepop
        ld    hl,ftem2
        jp    adiv1         ;sin(x)/cos(x]
;
;    compute sqr(x) x=top of argument stack
; return result in place of x
;
asqr::   ld      a,[astka]
        ld      l,a
        ld      a,[astka+1]
        ld      h,a

        ld    de,ftemp
        call    vcopy         ;save x in ftemp

; compute exponent of first guess as exponent of x/2
        ld      a,[astka]
        ld      l,a
        ld      a,[astka+1]
        ld      h,a

        ld    a,[hl]
        or    a
        ret    z         ;x=0

        sub     128
        cp      128
        jr      nc,sqr5       ;negative exponent

        rrca
        and    127
        jr      sqr6

sqr5::   cpl
        inc    a
        rrca
        and    127
        cpl
        inc     a
sqr6::
        add     a,128
        ld    [hl],a

; test for negative argument
        dec    hl
        ld    a,[hl]
        ld      hl,ermif        ;6e61h 'na'
        or    a
        jp    nz,error     ;neg argument

; do newton iterations
; newguess =( x/oldguess + oldguess ) /2
        ld    a,6         ;do 6 iterations
sqr20::  push    af         ;set new iteration count
        ld    bc,ftem1
        ld    de,ftemp     ;ftemp is 'x'

        ld      a,[astka]    ;guess
        ld      l,a
        ld      a,[astka+1]
        ld      h,a

        call    adiv1         ;ftem1=x/guess
        ld    de,ftem1

        ld      a,[astka]
        ld      l,a
        ld      a,[astka+1]
        ld      h,a

        ld    b,h
        ld    c,l
        call    aadd1         ;tos=(x/guess)+guess
        call    prepop
        ld    hl,fptwo
        call    adiv1         ;tos=(x/guess+guess)/2
        pop    af
        dec    a         ;decrement count
        jr      nz,sqr20     ;do another iteration
        ret
;
;    compute rnd(x) x=top of argument stack
; frand is updated to new random value
; a random number in the range 0<rnd<1 is returned in place
;
arnd::   call    prepop
        ld    de,frand
        ld      hl,frand
        call    amul1         ;tos=frand*frand

; set exponent=0
        ld      a,[astka]
        ld      l,a
        ld      a,[astka+1]
        ld      h,a

        ld    [hl],128     ;exponent=128 (0 in internal form]

; permute digits of x as
; 123456 into 345612
        ld    bc,-4
        add    hl,bc
        ld    b,[hl]         ;save 12
        inc    hl
        inc    hl
        call    permu         ;56=12
        call    permu         ;34=56
        call    permu         ;12=34
; normalize number
rnd5::   ld      a,[astka]    ;tos
        ld      l,a
        ld      a,[astka+1]
        ld      h,a

        ld    bc,-fpsiz+1
        add    hl,bc
        ld    a,[hl]         ;first digit pair
        and    15*16
        jr      nz,rnd10     ;number is normalized

; shift left one digit
        ld      a,[astka]
        ld      l,a
        ld      a,[astka+1]
        ld      h,a

        ld    a,[hl]         ;exponent
        dec    a
        ld    [exp],a
        call    _load         ;tos into temp
        ld    b,4
        call    left         ;shift left
        call    prepop
        call    store
        jr      rnd5         ;test if normalized

; save new random # in frand cell
rnd10::  ld    de,frand

        ld      a,[astka]
        ld      l,a
        ld      a,[astka+1]
        ld      h,a

        jp      vcopy        ;frand=tos

; permute pair of digit pairs
permu::  ld    a,[hl]
        ld    [hl],b
        ld    b,a
        dec    hl
        ret
;
;   evaluate p(x) using horners method (x is in ftemp]
; coefficient list pointer is in bc
; result replaces number on top of argument stack (y]
poly::   ld      a,[astka]
        ld      l,a
        ld      a,[astka+1]
        ld      h,a

        push    hl           ;de=ptr to y
        ld      l,e
        ld      h,d
        pop     de

        ld    h,b
        ld    l,c         ;hl ptr to coefficient list
        call    vcopy         ;y=first coefficient

; multiply by x
poly1::  push    hl         ;save coeff list pointer
        call    prepop
        ld    hl,ftemp
        call    amul1         ;y=y*x

; add next coeff
        call    prepop
        pop    hl
        push    hl         ;hl=coeff. list pointer
        call    aadd1         ;y=y+coeff.

; bump pointer to next coefficient
        pop    hl         ;coeff. pointer
        ld    bc,-fpsiz-1
        add    hl,bc         ;next coef sign
        ld    a,[hl]
        inc    hl         ;ptr to exponent
        cp      128
        jr      c,poly1      ;process next coefficient
        ret             ;negative sign (-1) - ends list
;
; prepare for operation
;
prepop:: ld      a,[astka]
        ld      e,a
        ld      a,[astka+1]
        ld      d,a

        ld    b,d
        ld    c,e
        ret
;
;     quadrant computation
; pops top of argument stack
; compute/gets sine of argument, quadrant of argument
; and index into quadrant
;
;    exits with::
; sp pointing to quadrant, mod 4
; sp+2 pointing to sign of argument
; top of argument stack has index into quadrant
quadc::  ld      a,[astka]
        ld      l,a
        ld      a,[astka+1]
        ld      h,a

        dec    hl         ;point to sign
        ld    b,[hl]
        xor    a
        ld    [hl],a         ;arg. sign=0
        ld    h,b

        pop     de           ;pop return addr
        push    hl           ;put sign on stack
        push    de           ;push return

; compute quadrant of abs(x]
        ld      a,[astka]
        ld      l,a
        ld      a,[astka+1]
        ld      h,a

        call    pshas         ;put copy of arg. onto stack
        call    prepop
        ld    hl,pic1         ;2/pi
        call    amul1         ;tos=x*2/pi
        call    prepop
        call    aint         ;tos=int(x*2/pi]

        ld      a,[astka]
        ld      l,a
        ld      a,[astka+1]
        ld      h,a

        call    pshas         ;another copy
        call    pfix         ;pop tos to de
        ld    a,e
        push    af         ;quadrant
        call    prepop
        ld    hl,pic2
        call    amul1        ;tos=int(x*pi/2]
        ld    de,ftemp
        call    popa1         ;ftemp=tos
        call    prepop
        ld    hl,ftemp
        call    asub1         ;tos=tos-ftemp
        pop    af         ;a=quadrant, low order byte
        and    3         ;mod 4
        pop    hl
        push    af         ;save quadrant on stack
        jp    hl         ;return

; "reg"

SetRegs::
        call    exprb        ;get value for DE regs for USR
        call    pfix

        ld      a,e
        ld      [callRegC],a
        ld      a,d
        ld      [callRegB],a

        ld      b,","
        call    eatc

        call    exprb        ;get value for DE regs for USR
        call    pfix

        ld      a,e
        ld      [callRegE],a
        ld      a,d
        ld      [callRegD],a
        ret

; "memtop"

SetMemTop::
        call    exprb        ;get value for top of RAM memory
        call    pfix

        ld      a,e
        ld      [memtop],a
        ld      a,d
        ld      [memtop+1],a

        jp      cclear          ;clear all variable space

; x=peek(x]
;  return memory byte
;
apeek::  call    pfix         ;get the address in de

        ld      a,[de]

acal2::  ld      l,a
        ld      h,0
        jr      acal3

; used to call user routine
acall::  call    pfix         ;get the address
        ld      l,e
        ld      h,d

        ld      bc,acal3     ;return link for user routine
        push    bc

        ld      a,[callRegC]    ;get user arguments
        ld      c,a
        ld      a,[callRegB]
        ld      b,a
        ld      a,[callRegE]
        ld      e,a
        ld      a,[callRegD]
        ld      d,a

        jp    hl

;Return HL as a floating point number on arg stack

acal3::  ld      de,cnsbuf
        call    cns
        ld    a,cr
        ld    [de],a
              ld      de,cnsbuf
        ld    hl,fpsink
        call    fpin
        ld    de,fpsink
        jp    psha1         ;put the returned user value on arg stack
;
;   int function action routine
aint::    ld    a,[bc]
        sub    129
        cp      128
        jr      c,aint1

; zero if value less than one
        xor    a
        ld    [bc],a
        ret

; exp > 0
aint1::  sub    fpnib-1
        ret    nc
        ld    d,a         ;count
        dec    bc
aint2::    dec    bc
        ld    a,[bc]
        and    $f0
        ld    [bc],a
        inc    d
        ret    z
        xor    a
        ld    [bc],a
        inc    d
        jr      nz,aint2
        ret
;
;    dimension matrix
; symtab addr in hl, hl not clobbered
; de contains size in # of elements
;
dims::   push    hl
        inc    de
        push    de
        ld    hl,0
        ld    c,fpsiz
        call    radd         ;multiply nelts by bytes per value

        ld      e,l
        ld      d,h

        ld      a,[mata]
        ld      l,a
        ld      a,[mata+1]
        ld      h,a

        push    hl
        add    hl,de
        call    stov         ;check that storage not exhausted

        ld      a,l
        ld      [mata],a     ;up date matrix free pointer
        ld      a,h
        ld      [mata+1],a

        pop    bc         ;base addr
        pop    de         ;nelts
        pop    hl         ;symtab addr
        push    hl
        ld    [hl],d
        dec    hl
        ld    [hl],e
        dec    hl
        ld    [hl],b
        dec    hl
        ld    [hl],c         ;symtab entry now set up
        pop    hl
        ret
;
;    find variable optionally subscripted in text
; sets carry if not found
; returns addr of variable in hl
; updates txa if found
;
var::    call    alpha   ;is first char a letter?
        ret     c       ;no

        call    name2
        call    gc
        cp    lparrw
        jr      z,var1       ;test for subscripted

; must be scalar variable
        call    stlk         ;returns entry addr in hl
;        jr      c,varsk1
;        call    true
        or      a
        ret

;varsk1::
;        call    false
;        or      a            ;clear carry
;        ret

; must be subscripted
var1::    call    gci         ;gobble left parenthesis
        ld    a,$80
        or    c
        ld    c,a         ;set type to matrix
        call    stlk
        push    hl         ;symbol table
        ld    de,10         ;default matrix size
        call    c,dims         ;default dimension matrix
        call    exprb         ;evaluate subscript expression
        call    pfix         ;de now has integer
        ld    b,")"
        call    eatc         ;gobble right parenthesis
        pop    hl
        dec    hl
        call    dcmp         ;bounds check index
        jp    nc,e5

        dec    hl
        dec    hl
        call    lhli         ;get base addr
        ld    c,fpsiz
        inc    de         ;because base addr is to element-1
        jp      radd         ;add index, clear carry
;
;   junk on end of statement, test if at eof.
; exit::    de is unaffected
;    eats char & line count after cr
;    leaves new txa in hl
;    sets carry if eof
;
joe::    call    gci
        cp    ":"
        ret    z
        cp    cr
        jp    nz,e1

        ld    a,[hl]
        dec    a
        jr      z,joe2

        inc    hl
        inc    hl
        inc    hl         ;skip over count & line #
joe1::   ld      a,l
        ld      [txa],a
        ld      a,h
        ld      [txa+1],a
        ret

joe2::   scf
        jr      joe1

;
;    get name from text
; exit::    carry set if name not found
;    if name found, it is returned in bc.
;    if no digit in name, c=0.
name1::  call    alpha
        ret    c
name2::  ld    b,a
        ld    c,0
        call    dig
        ccf
        ret    nc
        ld    c,a
        or    a         ;clear carry
        ret
;
;    symbol table lookup
; bc contains name and class
; if not found then create zero'ed entry & set carry
; hl has address on ret
;
stlk::
        ld      a,[memtop]
        ld      l,a
        ld      a,[memtop+1]
        ld      h,a

        ld    de,-stesiz   ;set up base and inc for search loop
stlk0::  ld    a,[hl]
        or      a            ;end of table ?
        jr      z,stlk2      ;yes, add to table

        cp    b
        jr      nz,stlk1     ;test if alpha compares

        dec    hl
        ld    a,[hl]         ;look for digit
        cp    c
        dec    hl
        ret    z         ;carry clear on ret

        inc    hl
        inc    hl
stlk1::  add    hl,de         ;didn't compare, dec pointer
        jr      stlk0

; add entry to symtab
stlk2::  ld    [hl],b
        dec    hl
        ld    [hl],c
        inc    hl

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        add     hl,de

        ld      a,l
        ld      [stb],a     ;store new end of symtab pointer
        ld      a,h
        ld      [stb+1],a

        dec    de
        dec    de

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        scf
        ret
;
;  gobbles new text character if alphabetic
; set carry if not
; next char in "a" on failure
;
alpha::  call    gc
        cp    "a"
        ret    c
        cp    "z"+1
        ccf
        ret    c
        jr      digt1

; gobbles next text char if digit
; sets carry if not
; next char in "a" on failure
dig::
        call    gc
        cp    "0"
        ret    c
        cp    "9"+1
        ccf
        ret    c
digt1::  inc    hl

        push    af
        ld      a,l
        ld      [txa],a
        ld      a,h
        ld      [txa+1],a
        pop     af

        ret
;
;   copys fpsiz bytes at addr hl to addr de
; on exit hl points to adr-1 of last byte copied
;
vcopy::  ld    c,fpsiz
vcop1::  ld    a,[hl]
        ld    [de],a
        dec    hl
        dec    de
        dec    c
        jr      nz,vcop1
        ret
;
; push value addr by hl onto arg stack
; sets argf, clears carry
;
pshas::  ld      e,l
        ld      d,h
psha1::  ld      a,[astka]
        ld      l,a
        ld      a,[astka+1]
        ld      h,a

        ld    bc,-fpsiz
        add    hl,bc

        ld      a,l
        ld      [astka],a    ;dec arg stack pointer
        ld      a,h
        ld      [astka+1],a

        push    hl           ;exchange de & hl
        ld      l,e
        ld      h,d
        pop     de

        call    vcopy
        ld    a,1
        ld    [argf],a     ;clear argf
        or    a         ;clear carry
        ret
;
;   pop arg stack
; hl contains addr to put popped value at
;
popas::  push    hl
        ld      l,e
        ld      h,d
        pop     de
popa1::  ld      a,[astka]
        ld      l,a
        ld      a,[astka+1]
        ld      h,a

        push    hl
        ld    bc,fpsiz
        add    hl,bc

        ld      a,l
        ld      [astka],a    ;inc stack pointer
        ld      a,h
        ld      [astka+1],a

        pop    hl
        jp    vcopy
;
;   push frame onto control stack
; takes minus amount to sub from cstka in de
; does overflow test and returns old cstka-1
;
pshcs::  ld      a,[cstka]
        ld      l,a
        ld      a,[cstka+1]
        ld      h,a

        push    hl
        add    hl,de

        ld      a,l
        ld      [cstka],a
        ld      a,h
        ld      [cstka+1],a

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        ld      hl,lcstka    ;addr contains cstkl
        call    dcmp
        jp    c,e4

        pop    hl
        dec    hl
        ret
;
;    storage overflow test
; test that value in hl is between mata & stb
; does not clobber hl
;
stov::   push    hl
        ld      l,e
        ld      h,d
        pop     de

        ld    hl,mata
        call    dcmp
        jr      c,stov1            ; Out of Memory error

        ld      hl,stb
        call    dcmp

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        ret    c

stov1::  jp      e8               ; Out of Memory error

;
; increment txa if next non-blank char is equal to b
; else syntax error
;
eatc::   call    gci
        cp    b
        ret    z
        jp    e1
;
; put next non-blank char in "a"
;
gc::     call    gci
        dec    hl

        push    af
        ld      a,l
        ld      [txa],a
        ld      a,h
        ld      [txa+1],a
        pop     af

        ret

;
; get line number from program
;
gln::
        ld      a,[txa]
        ld      l,a
        ld      a,[txa+1]
        ld      h,a

gln1::
        ld      a,[hl]
        inc     hl
        cp      " "
        jr      z,gln1

        cp      linent          ;is this a line # token?
        jr      nz,glnerr       ;no

        ld      e,[hl]
        inc     hl
        ld      d,[hl]
        inc     hl

        ld      a,l
        ld      [txa],a
        ld      a,h
        ld      [txa+1],a

        ld      l,e
        ld      h,d

        or      a               ;clear carry flag
        ret

glnerr:: scf
        ret

;
; put next non-blank char in "a" & inc txa
;
gci::    ld      a,[txa]
        ld      l,a
        ld      a,[txa+1]
        ld      h,a

gci0::   ld    a,[hl]
        inc    hl
        cp    " "
        jr      z,gci0

        push    af
        ld      a,l
        ld      [txa],a
        ld      a,h
        ld      [txa+1],a
        pop     af
        ret
;
;    repeat add
; adds de to hl c times
;
radd::   add     hl,de
        dec     c
        jr      nz,radd
        ret
;
prntcr:: ld      c,cr
        jr      prn1
;
prnt::  ld      c,term
;
; print message addressed by hl
; char in c specifies terminator.
; exit::    hl points to term addr
;
prn1::  ld      a,[hl]         ;get next char
        ld      b,a         ;for chout
        cp      c         ;end of message test
        ret     z

        cp      cr
        jp      z,e1         ;never print a cr in this routine

        call    chout
        inc     hl
        jr      prn1

;
; 16 bit unsigned compare
; compare de against value addressed by hl
;
dcmp::   ld      a,e
        sub     [hl]
        inc     hl
        ld      a,d
        sbc     a,[hl]
        dec     hl
        ret     nz

        ld      a,e
        sub     [hl]
        or      a         ;clear carry
        ret
;
; indirect load hl thru hl
;
lhli::   push    af
        ld      a,[hl]
        inc     hl
        ld      h,[hl]
        ld      l,a
        pop     af
        ret
;
; get fp constant from text
; pushes value on arg stack & sets argf flag
; sets carry if not found
;
const::  ld      a,[txa]      ;prepare call fpin
        ld      l,a
        ld      a,[txa+1]
        ld      h,a

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        ld      hl,fpsink
        call    fpin
        ret     c

        dec     de

        ld      a,e
        ld      [txa],a      ;now points to terminator
        ld      a,d
        ld      [txa+1],a

        ld      de,fpsink
        call    psha1

        xor     a
        inc     a         ;set a to 1 & clear carry
        ld      [argf],a
        ret
;
; direct statement checking routine
;
dirt::   ld    a,[dirf]
        or    a
        ret    z
              ld      hl,ermdi        ; 6469h 'di'
        jp    error

;
; Set eof address
; This needs to be done after a file load
;
findeof::
        ld      a,[bofa]
        ld      l,a
        ld      a,[bofa+1]
        ld      h,a

        ld    b,0
finde1:: ld      c,[hl]
        ld    a,c
        cp      eof             ;at eof yet?
        jr      z,finde2        ;yes

        add    hl,bc
        jr      finde1

finde2:: ld      a,l
        ld      [eofa],a
        ld      a,h
        ld      [eofa+1],a
        ret

;
; find text line with line # given in de
; returns text addr count byte in hl
;
findln:: ld      a,[bofa]
        ld      l,a
        ld      a,[bofa+1]
        ld      h,a

        ld    b,0
find1::  ld    c,[hl]
        ld    a,c
        cp    eof
        jr      z,lerr

        inc    hl
        call    dcmp
        dec    hl
        ret    z

        add    hl,bc
        jr      find1

lerr::   ld      hl,ermln          ; 6c6eh 'ln'
        jp    error
;
; fix floating to positive integer
; return integer value in de
; fp value from top of arg stack, pop arg stack
;
pfix::   ld      a,[astka]
        ld      l,a
        ld      a,[astka+1]
        ld      h,a

        ld    b,h
        ld    c,l
        push    hl
        call    aint
        ld    hl,fpsink
        call    popas
        pop    hl
        ld    c,[hl]         ;exponent
        dec    hl
        ld    a,[hl]         ;sign
        or    a
        jp    nz,e5         ;negative no good

        ld    de,-fpsiz+1
        add    hl,de
        ld    de,0
        ld    a,c
        or    a
        ret    z

        dec    c         ;set up for loop close test
pfix4::  inc    hl
        ld    a,[hl]
        rrca
        rrca
        rrca
        rrca
        call    mul10
        jp    c,e5

        dec    c
        ld      a,c
        cp      128
        ret     c       ;return if C is positive

        ld    a,[hl]
        call    mul10
        jp    c,e5

        dec    c
        ld      a,c
        cp      128
        jr      nc,pfix4 ;jump if C is negative

        ret
;
; take next digit in a (mask to $f], accumulate to de
;
mul10::  push    af
        ld      a,l
        ld      [miscW1],a
        ld      a,h
        ld      [miscW1+1],a
        pop     af

        ld    h,d         ;get original value in hl
        ld    l,e
        add    hl,hl         ;double it
        ret    c
        add    hl,hl         ;quaddruple it
        ret    c
        add    hl,de         ;add original for result of 5 x
        ret    c
        add    hl,hl         ;result is 10 x
        ret    c

        ld      e,l
        ld      d,h

        push    af
        ld      a,[miscW1]
        ld      l,a
        ld      a,[miscW1+1]
        ld      h,a
        pop     af

        and    $f
        add     a,e
        ld    e,a
        ld    a,d
        adc     a,0            ;propogate the carry
        ld    d,a
        ret
;
; Get integer from text
;
; Return::
;  set carry if not found
;  return integer in hl
;  return terminator in a
;
intger:: call    dig
        ret    c

        ld    de,0
        jr      intg2

intg1::  call    dig
        ld    h,d
        ld    l,e
        ccf
        ret    nc

intg2::  sub    "0"
        call    mul10
        jr      nc,intg1

        ret

;
; convert string to integer
; de = addr of string
; exit::
; de = updated
; hl = converted value
;csn::
;        ld      hl,0
;csn1::   ld      a,[de]
;        inc     de
;        cp      " "     ;is it a space?
;        jr      z,csn1  ;yes
;
;        cp      "0"     ;is it a digit?
;        jr      c


;
; convert integer to string
; de = addr of string
; hl = value to convert
; exit::    de = updated value
;
cns::
        xor    a         ;set for no leading zeroes
        ld    bc,-10000
        call    rsub
        ld    bc,-1000
        call    rsub
        ld    bc,-100
        call    rsub
        ld    bc,-10
        call    rsub
        ld    bc,-1
        call    rsub
        ret    nz
        ld    a,"0"
        ld    [de],a
        inc    de
        ret
;
; Take value in hl sub # in bc the
; most possible times.
; Put value on string at de.
; If a=0 then don't put zero on string.
; Return non-zero if a put on string
;
rsub::   push    de
        ld    d,-1
        di                      ;<----+
rsub1::  push    hl              ;     |
        inc     sp              ;     |
        inc     sp              ;     |
        inc     d               ;     |
        add     hl,bc           ;     +---- Kill interrupts since we're
        jr      c,rsub1         ;     |     unusually messing with stack.
                                ;     |
        dec     sp              ;     |
        dec     sp              ;     |
        ei                      ;<----+
        pop    hl
        ld    b,d
        pop    de
        or    b         ;a gets 0 if a was 0 and b is 0
        ret    z

        ld    a,"0"
        add     a,b
        ld    [de],a
        inc    de
        ret

;
;    input character from terminal
;
;inchar:: push    bc
;        push    de
;        push    hl
;vkeyin:: call    $-$
;        pop     hl
;        pop     de
;        pop     bc
;        and     $7f          ;strip parity bit
;        cp      esc
;        jp      z,cmnd1
;        ld      b,a
;        ret

;
inl0::   call    crlf
inline:: ld    hl,ibuf
        ld    c,linlen
inl1::   ld      b,GBB_RDY    ;Send input ready char.
        call    chout        ;Only needed by external terminal.

        call    inchar
        cp    8
        jr      z,inl2       ;backspace

        ld    [hl],a
        call    chout         ;echo
        ld    a,b
        cp    "@"         ;line deletion
        jr      z,inl0

        ld    b,lf         ;in case we are done
        cp    cr
        jr      z,chout      ;do lf then return

        inc    hl
        dec    c
        jr      nz,inl1

        ld      hl,ermll        ;6c6ch 'll'
        jp    error

inl2::    ld    a,c
;        ld      b,bell
        cp    linlen
        jr      z,inl1

        ld    b,8
        call    chout
        ld      b," "
        call    chout
        ld      b,8
        dec    hl
        inc    c

inl3::   call    chout
        jr      inl1
;
;    output to screen
;
chout::
        ld      a,b
        cp      10
        jr      z,chchk

        call    SerialTransmit

        cp      8               ;Is it 00 - 07 ?
        jr      c,chchk         ;yes, don't display control chars

        call    outch
chchk:: cp      cr
        jr      nz,chlf      ;not cr, is it lf?
        xor    a
        jp    pstor         ;return phead to zero
;
chlf::   cp    " "         ;no phead inc if control char
        ret    c
        ld    a,[phead]
        inc    a
pstor::  ld    [phead],a
        ret
;
crlf2::  call    crlf
crlf::   ld      b,13
;        call    chout
;        ld      b,10
        jp      chout
;
;    get integer from terminal
; de contains string to print first
; hl has 1 less than acceptable lower bound
; this routine goes to start if bad #
; integer value returned in hl
;
gint::   push    hl

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        ld    a,[phead]
        or    a
        call    nz,crlf
        call    prnt
        call    inline
        ld    hl,ibuf

        ld      a,l
        ld      [txa],a
        ld      a,h
        ld      [txa+1],a

        call    intger
        jp    c,start
        cp    cr
        jp    nz,start
        pop    de

        ld      a,l
        ld      [ibuf],a     ;use ibuf as a temp
        ld      a,h
        ld      [ibuf+1],a

        ld    hl,ibuf
        call    dcmp
        jp    nc,start

        ld      a,[ibuf]     ;get the value back to hl
        ld      l,a
        ld      a,[ibuf+1]
        ld      h,a

        ld    a,[hl]
        cpl
        ld    [hl],a         ;try to store there
        cp    [hl]
        jp    nz,start     ;bad or missing memory
        ret
;
;    output fp number addr by hl
;
fpout::  ld    bc,-digit-1
        add    hl,bc
        ld    b,h
        ld    c,l
        ld    hl,abuf         ;output buffer
        ld    a,[infes]    ;output format
        ld    [fes],a         ;store it
        ld    e,digit
        ld    [hl],0         ;clear _round-off overflow buffer
        inc    hl         ;abuf+1
;
nxt::    ld    a,[bc]         ;get digit and unpack
        ld    d,a
        rra
        rra
        rra
        rra
        and    $f         ;remove bottom digit
        ld    [hl],a         ;store top digit in output buffer (abuf]
        inc    hl
        ld    a,d         ;now get bottom digit
        and    $f
        ld    [hl],a         ;store it
        inc    hl
        inc    bc
        dec    e
        jr      nz,nxt

        ld    a,[bc]
        ld    [fsign],a    ;store sign of number
        xor    a
        ld    [hl],a         ;clear _round-off buffer (abuf+13) 12 digit no rnd
        ld    hl,xsign     ;exponent sign store
        ld    [hl],a         ;clear xsign
;
fix::    inc    bc         ;get exponent
        ld    a,[bc]
        or    a         ;exponent zero?
        jr      z,zro

        sub    128         ;remove normalizing bias
        jr      nz,fix2

        inc    [hl]         ;inc xsign to negative flag (1)later zero

fix2::
        cp      128
        jr      c,chk13

        cpl             ;it's a negative exponent
        inc    [hl]         ;inc xsign to negative (1]
zro::    inc    a
chk13::    ld    hl,expo         ;exponent temp store
        ld    [hl],a
        ld    e,a
        cp    digit*2
        ld    hl,fes         ;format temp byte
        jr      c,chkxo

chk40::    ld    a,1         ;force exponential printout
        or    [hl]         ;set format for xout
        ld    [hl],a
;
chkxo::    ld    a,[hl]         ;check if exponential printout
        rra
        jr      nc,chkx3

        and    $f
        cp    digit*2
        jr      c,chkx2

        ld    a,digit*2-1  ;max digits
chkx2::    ld    d,a
        inc    a
        jr      _round
;
chkx3::  and    $f         ;add exponent & decimal places
        ld    d,a
        add     a,e
        cp    digit*2+1
        ld    b,a
        jr      c,chkxn

        ld    a,[hl]
        and    $40
        jr      nz,chk40
;
chkxn::    ld    a,[xsign]    ;check exponent sign
        or    a
        jr      nz,xneg      ;it's negative

        ld    a,b
        jr      _round
;
xneg::    ld    a,d         ;sub exponent & decimal place count
        sub    e
        jr      nc,xn2

xn1::    ld    a,[infes]
        cp      128
        jp      c,zero

        and    $e
        jp    z,zero

        rrca
        ld    e,a
        dec    e
        ld    c,1
        ld    hl,abuf-1
        jr      nrnd

xn2::    jr      z,xn1
        jr      _round
;
;
clean::  ld    b,$1f         ;clear flags
        and    b
        cp    digit*2+1
        ret    c

        ld    a,digit*2+1  ;max digits out
        ret
;
; this routine is used to _round data to the
; specified decimal place
_round::  call    clean
        ld    c,a
        ld    b,0
        ld    hl,abuf+1
        add    hl,bc         ;get _round-off addr

        ld      a,l
        ld      [addt],a
        ld      a,h
        ld      [addt+1],a

        ld    a,[hl]
        cp    5         ;_round if >=5
        jr      c,trl1
;
less1::  dec    hl
        inc    [hl]         ;_round up
        ld    a,[hl]
        or    a
        jr      z,trl2

        cp    10         ;check if rounded number >9
        jr      nz,trail

        ld    [hl],0
        jr      less1
;
; this routine eliminates trailing zeros
trail::  ld      a,[addt]
        ld      l,a
        ld      a,[addt+1]
        ld      h,a

trl1::   dec     hl
trl2::   ld    a,[fes]         ;check if trailing zeros are wanted
        rla
        jr      c,fprnt      ;yes, go print data

trl3::   ld    a,[hl]
        or    a         ;is it a zero?
        jr      nz,fprnt     ;no, go print

        dec    hl
        dec    c         ;yes, fix output digit count
        ld      a,c
        cp      128
        jp      nc,zeron     ;jump if C is negative

        jr      trl3
;
; print format routines
fprnt::  ld    hl,abuf
        ld    a,[hl]         ;check if rounded up to 1
        or    a
        jr      z,nrnd       ;jump if not

        ld    b,1
        ld    a,[xsign]    ;is exponent negative?
        or    a
        jr      z,posr

        ld    b,-1
;
posr::   ld    a,[expo]     ;get exponent
        or    a
        jr      nz,po2       ;is it zero? (e+0]

        ld    [xsign],a
        ld    b,1
po2::    add     a,b          ;fix exponent count
        ld    [expo],a
        inc    e
        inc    c
        dec    hl
;
nrnd::    inc    hl
        ld    a,c
        cp    digit*2+1    ;check for maximum digits out
        jr      nz,nrnd1

        dec    c
nrnd1::    ld    a,[fsign]    ;check if neg #
        rra
        jr      nc,prin2     ;go output radix & number

        call    neg         ;output (-]
        jr      pri21
;
prin2::    call    space         ;output a space
pri21::    ld    a,[fes]         ;get output format
        rra             ;check if exponential output format
        jr      c,xprin

        ld    a,[xsign]    ;get exp sign
        or    a         ;check if neg exp
        jr      z,posit

        ld    a,c
        or    a
        jr      nz,prin4     ;output radix & number

        jp      zero         ;no digits after radix, output zero & done
;
prin4::    call    radix         ;print decimal point
prin6::  xor     a
        or    e
        jr      z,prin5      ;jump if no zeros to print

        call    zero         ;force print a zero
        dec    e
        jr      nz,prin6
;
prin5::    call    nout         ;print ascii digit
        jr      nz,prin5
        ret
;
posit::  call    nout
        dec    e         ;bump exp count
        jr      nz,posit

        ld    a,c         ;check if more digits to output
        or    a
        ret    z         ;no, done
        cp      128
        ret     nc

        jr      prin4        ;now print decimal point
;
; exponential format output
xprin::    call    nout
        jr      z,ndec       ;integer?

        call    radix         ;no. print decimal point
xpri2::    call    nout
        jr      nz,xpri2
;
ndec::    ld    b,"e"         ;print "e"
        call    chout
        ld    a,[xsign]
        or    a
        jr      z,xpri3

        call    neg         ;print exp sign (-]
        ld    a,[expo]
        inc    a
        jr      xout2

xpri3::    ld    b,"+"         ;exp (+]
  call    chout
;
; convert the exponent from binary-to-ascii
; and print the result.
xout::    ld    a,[expo]
        dec    a
xout2::    ld    c,100
        ld    d,0
        call    conv
        cp    "0"         ;skip leading zeros
        jr      z,xo21

        inc    d
        call    chout
xo21::    ld    a,e
        ld    c,10
        call    conv
        cp    "0"
        jr      nz,xo3

        dec    d
        jr      nz,xo4

xo3::    call    chout
xo4::    ld    a,e
        add     a,"0"          ;add ascii bias
        ld    b,a
        jp      chout
;
conv::    ld    b,"0"-1
conv1::  inc     b
        sub    c
        jr      nc,conv1

        add     a,c
        ld    e,a
        ld    a,b
        ret
;
; change bcd digit to ascii & print
nout::    ld    a,[hl]
        add     a,"0"
        ld    b,a
        call    chout
        inc    hl
        dec    c         ;dec total digits printed count
        ret

; print fp zero
zeron::  ld      b," "
        call    chout
        jr      zero
;
; common symbol loading routines
neg::    ld    b,"-"
        jp    chout
zero::   ld    b,"0"
        jp    chout
space::  ld    b," "
        jp    chout
radix::  ld    b,"."
        jp    chout

; converts fp string at de, update de past terminator
; puts terminator in b, puts fp # at addr in hl
; sets carry if not found
fpin::
        push    hl

        ld      l,e
        ld      h,d

        dec     hl

        ld      a,l
        ld      [adds],a
        ld      a,h
        ld      [adds+1],a

        call    ibscn           ;get first non-space
        cp      "&"
        jr      z,fpin6

        dec     hl
        call    ibscn2          ;add back to buffer
        call    fpins
        pop     hl
        jp      nc,entr3
        ret

; get hex number from input
fpin6::
        call    ibscn           ;get "h"
        cp      "h"             ;is it hex?
        jp      nz,e1           ;no

        call    getnib
        jp      c,e1            ;bad hex number

        ld      e,a
        ld      d,0

        ld      b,4
fpin7::  call    getnib
        jp      c,fpin8

        dec     b
        jp      z,e7            ;overflow

        push    hl              ;de = de * 16
        ld      l,e
        ld      h,d
        add     hl,hl
        add     hl,hl
        add     hl,hl
        add     hl,hl
        ld      e,l
        ld      d,h
        pop     hl

        add     a,e             ;add a to de
        ld      e,a
        ld      a,0
        adc     a,d
        ld      d,a

        jr      fpin7

fpin8::
        push    hl

        ld      l,e             ;put hex number in hl
        ld      h,d

        ld      de,cnsbuf       ;convert it to a ascii decimal string
        call    cns
        ld      a,cr
        ld      [de],a

        ld      de,cnsbuf-1
        ld      a,e
        ld      [adds],a
        ld      a,d
        ld      [adds+1],a
        call    fpins
        pop     de
        pop     hl
        push    de
        call    entr3
        pop     de
;        inc     de
        ld      a,[de]
        ld      b,a
        inc     de
        ret

getnib::
        call    ibscn
        sub     "0"
        cp      "9"+1-"0"
        ccf
        ret     nc

        sub     "a"-"0"
        cp      "F"+"1"-"a"-"0"
        ccf
        ret     c
        add     a,10
        ret

;fpin::
;        push    hl
;        push    de
;
;        ld      l,e
;        ld      h,d
;
;        dec     hl
;
;        ld      a,l
;        ld      [adds],a
;        ld      a,h
;        ld      [adds+1],a

fpins::
        push    de

        ld    hl,opst         ;clear temporary storage areas & bc buffer
        ld    c,digit+6
        call    clear
;
scanc::    ld    de,0
        ld      hl,bcs       ;bc=pack buffer
scan0::  ld      a,l
        ld      [bcadd],a    ;pack buffer pointer
        ld      a,h
        ld      [bcadd+1],a

scanp::    ld    hl,scanp
        push    hl         ;used for return from other routines
        xor    a
        ld    [xsign],a    ;clear exp sign byte
;
scang::    call    ibscn
        jr      c,scanx      ;found a #, go pack it
        cp    "."         ;radix?
        jr      z,scan5      ;process radix pointers
        cp    "e"         ;exp?
        jp    z,excon         ;found "e"', go process exp #

;this char not legal in #
        ld    b,a         ;move terminator to b
        ld    a,[opst]     ;check if any digits yet
        and    $10
        jp    nz,entr2

;legal fp number not found
fpin1::    pop    hl         ;rid of scanp link
        pop     de           ;text pointer
        scf
        ret

;found decimal point
scan5::    xor    a         ;found radix process radix pointers for exp
        or    d         ;any digits yet?
        jr      nz,scan6

        add     a,$c0       ;set ecnt - stop counting digits
        or    e         ;no int digits, bit 7 is count (or don't) flag
        ld    e,a         ;bit 6 is negative exp flag
        ret

scan6::    ld    a,$80         ;set ecnt to count digits
        or    e
        ld    e,a
        ret
;
scanx::    and    $f         ;found number - remove ascii bias
        ld    b,a
        ld    hl,opst         ;set first char flag
        ld    a,$30
        or    [hl]
        ld    [hl],a
        xor    a
        or    b         ;is char zero?
        jr      nz,pack

        or    d         ;leading zero? ie; any int digits?
        jr      nz,pack

        or    e
        ld    e,a
        ret    z         ;if counting yet,
        inc    e         ;ecnt+1-count zeros for exp count
        ret
;
; bcd pack digits into pair bc
;
pack::    ld    a,e
        rla
        jr      c,pack1

        inc    e
pack1::    ld    a,e
        ld    [ecnt],a     ;digit count for exp count
        inc    d         ;total digit count (d has top/bot flag bit 7]
        ld    a,d
        and    $7f         ;remove top/bot flag
        cp    digit*2+1    ;limit input digits
        ret    nc

        ld      a,d
        cp      128
        jr      nc,botm
;
top::    or    $80         ;set msb for top flag
        ld      d,a

        ld      a,[bcadd]    ;get bc addr
        ld      l,a
        ld      a,[bcadd+1]
        ld      h,a

        ld      a,b
        rlca
        rlca
        rlca
        rlca
        ld    [hl],a         ;save char in bc
        ret
;
botm::    and    $7f         ;strip msb (bottom flag]
        ld    d,a
        ld    a,b

        ld      a,[bcadd]
        ld      l,a
        ld      a,[bcadd+1]
        ld      h,a

        ld      a,b
        or    [hl]         ;or in top number
        ld    [hl],a         ;put number back in bc
        inc    hl
        pop    bc
        jp    scan0

ibscn::  ld      a,[adds]     ;input buffer pointer
        ld      l,a
        ld      a,[adds+1]
        ld      h,a

ibscn1:: inc     hl           ;get next byte
        ld    a,[hl]
        cp    " "
        jr      z,ibscn1

ibscn2:: push    af
        ld      a,l
        ld      [adds],a
        ld      a,h
        ld      [adds+1],a
        pop     af

; check for ascii numbers
nmchk::  cp    "9"+1
        ret    nc
        cp    "0"
        ccf
        ret
;
; adjust a number in bc buffer & return value
entr2::  ld    de,0
ent1::   push    bc         ;terminator
        call    fixe         ;normalize floating point #
        pop    bc         ;terminator
        pop    de         ;scanp link
        pop    de         ;old text addr
        or      a
        ret


        pop    de         ;ret addr

entr3::
        ld      e,l
        ld      d,h
        ld      c,digit+2
        ld      hl,bcs+digit+1
        call    vcopy

        push    af
        ld      a,[adds]
        ld      l,a
        ld      a,[adds+1]
        ld      h,a
        pop     af

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        inc    de
        or    a
        ret

; clear storage areas
; hl = starting address
; c = count
clear::  xor    a
clear1:: ld      [hl],a
        inc    hl
        dec    c
        jr      nz,clear1
        ret
;
; convert ascii exponent of number in the input buffer
; to binary. normalize exponent according to the input
; format of the number.
excon::  call    ibscn         ;get character
        jr      c,exc3

        cp    plsrw         ;check for unary sign
        jr      z,exc4

        cp    "+"
        jr      z,exc4

        cp    minrw
        jr      z,exc2

        cp    "-"
        jr      nz,fperr     ;no sign or number?

exc2::   ld    a,1
        ld    [xsign],a    ;save sign
exc4::   call    ibscn
        jr      nc,fperr     ;no number?

exc3::   call    ascdc         ;convert ascii to binary
        jr      ent1         ;normalize # & return
;
; convert ascii to binary
; three consecutive numbers <128 may be converted
ascdc::  push    hl
        ld      l,e
        ld      h,d
        pop     de

        ld    hl,0
asc1::   ld    a,[de]         ;get chr from input buffer, no spaces allowed
        call    nmchk         ;check if #
        jr      nc,asc2
        sub    "0"         ;remove ascii bias
        ld    b,h
        ld    c,l
        add    hl,hl
        add    hl,hl
        add    hl,bc
        add    hl,hl
        ld    c,a
        ld    b,0
        add    hl,bc
        inc    de
        jr      asc1

asc2::   push    hl
        ld      l,e
        ld      h,d
        pop     de

        ld    b,a         ;save terminator

        ld      a,l
        ld      [adds],a     ;save ibuf addr
        ld      a,h
        ld      [adds+1],a

        ld    a,d
        or    a
        jr      nz,fperr     ;too big >255

        ld    a,e
        rla
        jr      c,fperr      ;too big >127

        rra
        ret

fperr::  pop    bc         ;ascdc ret link
        jp    fpin1
;
; normalize input buffer
fixe::   push    hl
        ld      l,e
        ld      h,d
        pop     de

        ld      a,[bcs]
        or    a         ;is it zero?
        jr      z,zz2

        call    chkpn         ;set exp pos/neg
        add     a,$80          ;add exp bias
zz2::    ld      [bcs+digit+1],a;store normalized exp in bc
        ret
;
chkpn::  ld    a,[ecnt]     ;get exp count-set in 'scan' routine
        ld    e,a
        and   $3f         ;strip bits 7&8
        ld    b,a
        ld    a,[xsign]
        or    a
        jr      z,lpos       ;exponent is positive

        inc    h         ;set sign in h
        ld    a,$40         ;l is neg
        and    e         ;check if e is negative
        jr      z,epos

        ld    a,l         ;both e&l neg
        ld    l,b
        call    bpos1
        cpl
        inc    a
        ret             ;back to fixe
;
epos::    ld    a,l         ;e&l neg
epos1::  cpl
        inc    a
        add     a,b
        ret             ;to fixe
;
lpos::    ld    a,$40         ;exponent positive
        and    e         ;is e negative?
        jr      z,bpos

        ld    a,b
        ld    b,l
        jr      epos1
;
bpos::    ld    a,b         ;e&l pos
bpos1::  add     a,l
        cp      128
        ret     c

        pop    hl
        jr      fperr

        DB    $10
        DW    0
        DB    1
fpnone:: DB    129
;
;    four function floating point bcd
;
;        bc = de # hl
;          # is +,-,*, or /.
;    <bc>=address of result
;    <de>=address of 1st argument
;    <hl>=address of 2nd argument
; all addresses on entry point to the exponent part of #.
; each # consists of (2*digit) packed decimal digits,
; a sign, and a biased binary exponent. the exponent range
; is 10**-127 to 10**127. the number 0 is represented by
; the exponent 0. the numbers are stored in memory as
; digit bytes of decimal digits starting at the low order
; address. all numbers are assumed to be normalized.
;
;    floating point addition
;
fadd::   push    bc
        call    expck         ;fetch arguments
        ld    c,0
adsum::  dec    de

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        ld    a,[sign]
        xor    [hl]         ;form sign of result
        ld    b,a

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        ld    a,[de]
        dec    de
        xor    c
        ld    [sign],a
        ld    hl,rctrl     ;rounding control flag
        ld    a,[hl]
        or    a
        inc    hl
        ld    a,[hl]         ;get rounding digit
        jr      z,ads8

        rlca
        rlca
        rlca
        rlca
ads8::   add     a,$b0       ;force carry if digit > 5
        ld    a,b
        rra
        jr      c,ads1       ;have sub

        rla             ;restore carry
        call    add0         ;perform addition
        jr      nc,ads2

        ld    b,4
        call    right
        ld    hl,exp
        inc    [hl]         ;inc exp
        jp    z,over

ads2::    pop    bc         ;get results addr
        jp      store        ;save results

zerex::  pop    hl
        jr      ads2

add0::   ld    hl,buf+digit-1
        ld    b,digit
add1::   ld    a,[de]
        adc     a,[hl]
        daa
        ld    [hl],a
        dec    hl
        dec    de
        dec    b
        jr      nz,add1

        ret    nc
        inc    [hl]
        ret
;
;    floating point subtraction
;
fsub::   push    bc
        call    expck         ;get arguments
        ld    a,[sign]
        xor    1         ;complement sign
        ld    [sign],a
        jr      adsum

ads1::   rla             ;restore carry
        ccf             ;complement for rounding
        call    sub0         ;subtract arguments
        ld    hl,sign
        jr      c,ads4

        ld    a,[hl]         ;get sign
        xor    1         ;complement
        ld    [hl],a
ads7::    dec    hl
        ld    b,digit
ads3::    ld    a,$9a
        sbc     a,[hl]       ;complement result
        add     a,0
        daa
        ld    [hl],a
        dec    hl
        dec    b
        ccf
        jr      nz,ads3

ads4::    ld    hl,buf
        ld    bc,digit
ads5::    ld    a,[hl]
        or    a
        jr      nz,ads6

        inc    hl
        inc    b
        inc    b
        dec    c
        jr      nz,ads5

        xor    a
        ld    [exp],a
        jr      ads2

ads6::    cp    $10
        jr      nc,ads9

        inc    b
ads9::    ld    hl,exp
        ld    a,[hl]
        sub    b
        jp    z,under
        jp    c,under

        ld    [hl],a
        ld    a,b
        rlca
        rlca
        ld    b,a
        call    left
        jr      ads2

sub0::    ld    hl,buf+digit-1
        ld    b,digit
sub1::    ld    a,$99
        adc     a,0
        sub    [hl]

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        add     a,[hl]
        daa

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        ld    [hl],a
        dec    hl
        dec    de
        dec    b
        jr      nz,sub1
        ret
;
;    floating point multiply
;
fmul::    push    bc
        ld    a,[hl]
        or    a         ;argument = 0?
        jr      z,fmul1+2

        ld    a,[de]
        or    a         ;argument = 0?
        jr      z,fmul1+2

        add     a,[hl]       ;form result exponent
        jr      c,fmovr
        cp      128
        jp      c,under         ;jump if A is positive

        jr      fmul1

fmovr::
        cp      128
        jp      nc,over

fmul1::    sub    128         ;remove excess bias
        ld    [exp],a         ;save exponent
        dec    de
        dec    hl
        ld    a,[de]

        xor    [hl]         ;form result sign
        dec    hl
        dec    de
        push    hl
        ld    hl,sign         ;get sign addr
        ld    [hl],a         ;save sign
        dec    hl
        xor    a
        ld    b,digit+2
fmul2:: ld    [hl],a         ;zero working buffer
        dec    hl
        dec    b
        jr      nz,fmul2

        ld    a,[exp]
        or    a
        jp    z,zerex

        ld    c,digit
        ld    hl,hold1+digit
; get multiplier into holding register
fmul3:: ld    a,[de]
        ld    [hl],a         ;put in register
        dec    hl
        dec    de
        dec    c
        jr      nz,fmul3

        ld    [hl],c
        dec    hl
        ld    b,250         ;set loop count
fmul4:: ld    de,digit+1
        ld    c,e
        add    hl,de

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        add    hl,de         ;hl=next holding register
        inc    b
        ld      a,b
        cp      128
        jr      c,fmul8      ;finished

fmul5:: ld    a,[de]         ;get digits
        adc     a,a          ;times 2
        daa
        ld    [hl],a         ;put in holding register
        dec    de
        dec    hl
        dec    c
        jr      nz,fmul5

        inc    b         ;inc loop count
        jr      nz,fmul4

; form 10x by adding 8x & 2x
; first get 8x

        inc    hl
        ld    de,hold5     ;next holding register
        ld    c,digit+1
        ld    b,c
fmul6::    ld    a,[hl]
        ld    [de],a
        inc    hl
        inc    de
        dec    c
        jr      nz,fmul6

        ld    hl,hold2+digit;get 2x
        dec    de
fmul7::    ld    a,[de]
        adc     a,[hl]       ;form 10x
        daa
        ld    [de],a
        dec    de
        dec    hl
        dec    b
        jr      nz,fmul7

        ld    b,249

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        jr      fmul4

fmul8::  push    hl
        ld      l,e
        ld      h,d
        pop     de

        inc    hl

        ld    [hl],digit+1 ;set next loop count
; perform accumulation of product
fmul9::    pop    bc         ;get multiplier
        ld    hl,hold8+digit+1
        dec    [hl]         ;dec loop count
        jr      z,fmu14      ;finished

        ld    a,[bc]
        dec    bc
        push    bc
        dec    hl

        push    hl
        ld      l,e
        ld      h,d
        pop     de

fmu10::  add     a,a          ;check for bit in carry
        jr      c,fmu11      ;found a bit
        jr      z,fmu12      ;zero, finished this digit

        ld    hl,-digit-1
        add    hl,de         ;point to next holding register

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        jr      fmu10

fmu11::    ld    c,a
        or    a         ;clear carry
        call    add0         ;accumulate product
        ld    a,[de]
        add     a,[hl]
        daa
        ld    [hl],a
        ld    a,c
        dec    de
        jr      fmu10

; rotate right 1 byte
fmu12::    ld    b,8
        call    right
        jr      fmul9

fmu14::    ld    a,[buf]
        and    $f0         ;check if normalized
        jr      z,fmu17

        ld    a,d
        and    $f0
        ld    hl,sign-1
        jr      fmu18

fmu17::    ld    b,4
        ld    hl,exp
        dec    [hl]
        jp    z,under

        call    left         ;normalize
        ld    a,d         ;get digit shifted off
; perform rounding
        rrca
        rrca
        rrca
        rrca
fmu18::    cp    $50
        jr      c,fmu16

        inc    a
        and    $f
        ld    c,digit
fmu15::  adc     a,[hl]
        daa
        ld    [hl],a
        ld    a,0
        dec    hl
        dec    c
        jr      nz,fmu15

; check for rounding overflow
        jp    nc,ads2         ;no overflow

        inc    hl
        ld    [hl],$10
        ld    hl,exp
        inc    [hl]
        jp    nz,ads2
        jp    over

; rounding not needed
fmu16::    and    $f
        add     a,[hl]
        ld    [hl],a
        jp    ads2
;
;    floating point division
;
fdiv::    push    bc
        ld    a,[hl]         ;fetch divisor exp
        or    a         ;divide by 0?
        jp    z,divz

        ld    a,[de]
        or    a         ;dividend = 0?
        jp    z,insp

        sub    [hl]
        jr      c,divun
        cp      128
        jp      nc,over
        jr      fdi1

divun::  cp      128
        jp      c,under         ;jump if positive

fdi1::   add     a,129        ;form quotient exp
        ld    [expd],a

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        push    de
        call    _load         ;fetch dividend
        pop    de

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        ld    a,[sign]
        dec    hl
        xor    [hl]         ;form quotient sign
        ld    [signd],a

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        dec    de
        ld    bc,hold1
div0::    ld    l,digit+digit
div1::    push    bc
        push    hl
        ld    c,0         ;quotient digit = 0
div3::    scf             ;set carry
        ld    hl,buf+digit-1
        ld    b,digit
div4::    ld    a,$99
        adc     a,0

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        sub    [hl]

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        add     a,[hl]
        daa
        ld    [hl],a
        dec    hl
        dec    de
        dec    b
        jr      nz,div4

        ld    a,[hl]
        ccf
        sbc     a,0
        ld    [hl],a
        rra
        ld    hl,digit
        add    hl,de

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        inc    c         ;inr quotient
        rla
        jr      nc,div3

        or    a         ;clear carry
        call    add0         ;restore dividend
        ld    hl,digit
        add    hl,de

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        push    bc
        ld    b,4
        call    left         ;shift dividend
        pop    bc
        dec    c
        pop    hl
        ld    h,c
        pop    bc
        ld    a,l
        jr      nz,div5

        cp      digit+digit
        jr      nz,div5

        ld    hl,expd
        dec    [hl]
        call    z,under
        jr      div0

div5::    rra
        ld    a,h
        jr      nc,div6

        ld    a,[bc]
        rlca
        rlca
        rlca
        rlca
        add     a,h
        ld    [bc],a         ;store quotient
        inc    bc
        jr      div7

div6::    ld    [bc],a         ;store quotient
div7::    dec    l         ;dec digit count
        jr      nz,div1

        ld    hl,expd
        pop    bc
        jr      storo

; fetch & align arguments for
; addition & subtraction
expck::    ld    a,[de]
        sub    [hl]         ;difference of exps
        ld    c,0
        jr      nc,expc1

        inc    c

        push    hl
        ld      l,e
        ld      h,d
        pop     de

        cpl
        inc    a
expc1::    ld    b,a
        ld    a,[de]
        ld    [exp],a
        ld    a,b
        cp    digit+digit
        jr      c,expc2

        ld    a,digit+digit
expc2::    rlca
        rlca
        ld    b,a
        and    4
        ld    [rctrl],a    ;set rounding control
        push    bc
        push    de
        call    _load         ;load smaller value
        ld    a,8*digit+16
        sub    b
        cp    8*digit+16
        jr      z,expc3

        and    $f8
        rra
        rra
        rra
        add     a,e
        ld    e,a
        ld    a,d
        adc     a,0
        ld    d,a
        ld    a,[de]         ;get rounding digit
        ld    [rdigi],a    ;save
expc3::    call    right         ;align values
        pop    de
        pop    bc
        ret

; load argument into buffer
_load::   ld      de,sign
        ld      c,digit+1
        dec     hl
load1::  ld      a,[hl]
        ld      [de],a
        dec     hl
        dec     de
        dec     c
        jr      nz,load1

        xor     a
        ld      [de],a
        dec     de
        ld      [de],a
        ld      [rdigi],a    ;zero rounding digit
        ret

; store results in memory
store::  ld      hl,exp
storo::  ld    e,digit+2
stor1::  ld    a,[hl]
        ld    [bc],a
        dec    bc
        dec    hl
        dec    e
        jr      nz,stor1
        ret

; shift right number of digits in b/4
right::    ld    c,digit+1
righ1::    ld    hl,buf-1
        ld    a,b
        sub    8         ;check if byte can be shifted
        jr      nc,righ3

        dec    b
        push    af
        ld      a,b
        cp      128
        jr      c,righ5

        pop     af
        ret

righ5::
        pop     af

        or    a
righ2::    ld    a,[hl]
        rra
        ld    [hl],a
        inc    hl
        dec    c
        jr      nz,righ2
        jr      right

; shift right one byte
righ3::    ld    b,a
        xor    a
righ4::    ld    d,[hl]
        ld    [hl],a
        ld    a,d
        inc    hl
        dec    c
        jr      nz,righ4
        jr      right

; shift left number of digits in b/4
left::    ld    c,digit+1
        ld    hl,sign-1
lef1::    ld    a,b
        sub    8
        jr      nc,lef3

        dec    b
        push    af
        ld      a,b
        cp      128
        jr      c,lef5

        pop     af
        ret

lef5::
        pop     af

        or    a
lef2::    ld    a,[hl]
        rla
        ld    [hl],a
        dec    hl
        dec    c
        jr      nz,lef2
        jr      left

; shift left one byte
lef3::    ld    b,a
        xor    a
lef4::    ld    d,[hl]
        ld    [hl],a
        ld    a,d
        dec    hl
        dec    c
        jr      nz,lef4
        jr      left

; set flags for overflow, underflow
; and divide by zero
over::   ld      hl,ermfp                ;6670h 'fp'
        jp    error

under::  ld      a,$ff
        ld    [erri],a
insp::    inc    sp
        inc    sp
        ret

divz::   ld      hl,ermdz
        jp      error

; Print ascii string terminated by $ immediately
; after CALL. Return to location immediately following
; the $ when done.

ilprc::  EX_SP_HL
        push    af
        push    bc
        push    de
ilpr1::  ld      a,[hl]
        inc     hl
        or      a
        jr      z,ilprt2
        ld      b,a
        call    chout
        jr      ilpr1
ilprt2::
        call    crlf
        pop     de
        pop     bc
        pop     af
        EX_SP_HL
        ret

        .block ;$8000-$ ;fill up whole 32768 block

        .end
