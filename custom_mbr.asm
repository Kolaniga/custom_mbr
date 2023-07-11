;
; 1. make .bin and boot from floppy. (example, bochs)
; 2. eject floopy and boot from hdd.
; 3. input pass. if currect - loading OS. else - try again.
;

use16

org 7C00h

addr_mbr          equ 0x7C00
size_mbr          equ 0x200
size_data_mbr     equ 0x1BE           ; data size mbr.
new_point         equ 0x0600          ; new addr where will be copy myMBR to start.
addr_buf          equ 0x8000          ; buffer.

partition1        equ 0x7BE           ; new_point + size_data_mbr (0600h + 1BEh).

plus              equ 0x2B            ; +  if OK.
minus             equ 0x2D            ; -  if ERR.

press_enter       equ 0x0D            ; ENTER
buff_pass         equ 0x0800          ; buffer for entered password.
password_crc      equ 0x92A3A1CC      ; myMbr CRC32. init = 0x0
                                      ;              refIn = false
                                      ;              refOut = false
                                      ;              xorOut = 0x0

jmp START

mes          db '[',plus,'] save orig mbr', 0x0D, 0x0A, 0
mes1         db '[',plus,'] myMbr -> Sec 1. Ctrl+Alt+Del', 0

addr_buf_l   dd addr_buf, 0
addr_myMbr_l dd myMbrStart, 0

START:

;----- save original MBR ---------------------------------------------------------

       mov ax, 0x0201              ;
       mov dx, 0x0080              ; Read from HDD HEAD = 0 ...
       mov cx, 0x0001              ; CYLINDER = 0, SECTOR = 1 ...
       lea bx, [addr_buf]          ; in that address.
       int 0x13

       mov ax, 0x0301              ; Write in HEAD = 0, CYLINDER = 0, ...
       mov cx, 0x0003              ; SECTOR = 3.
       int 0x13                    ;
       jnc ok_s                    ; save OK.

       mov [mes+1], minus          ; save ERR. set [-] in mes.
       push mes                    ;
       call newPointStart + (write - (new_point + (newPointStart - myMbrStart))) 
       jmp $

ok_s:  push mes
       call newPointStart + (write - (new_point + (newPointStart - myMbrStart)))

;--------------------------------------------------------------------------------

;----- copy outselves to HDD Sector 1 from 'addr_buf' ---------------------------
copy:
       cld
       xor al, al
       les di, [addr_buf_l]       ; [ES:DI] = 0:8000.
       mov cx, size_data_mbr
       rep stosb                  ; make zero-buf until partition table (0:01BEh).

       lds si, [addr_myMbr_l]     ; copy from addr mark 'myMbrStart'...
       les di, [addr_buf_l]       ; to zero-buf.
       mov cx, (ENDS - (new_point + (newPointStart - myMbrStart))) + (newPointStart - myMbrStart)
       rep movsb

       mov ax, 0x0301             ;
       mov dx, 0x0080             ;
       mov cx, 0x0001             ;
       lea bx, [addr_buf]         ;
       int 0x13                   ; write zero_buf at sector 1.
       jnc ok_c                   ; copy OK.

       mov [mes1+1], minus        ; copy ERR. set [-] in mes1.

ok_c:  push mes1
       call newPointStart + (write - (new_point + (newPointStart - myMbrStart))) 

       jmp $
;--------------------------------------------------------------------------------

;================================================================================
;======= myMbr start work =======================================================
;================================================================================
myMbrStart:

        xor ax, ax
        mov dx, ax
        mov es, ax
        cli
        mov ss, ax
        mov sp, 0xFFFF          ; set stack.
        sti

;----- copy outselves to 0x0600 from 0x7c00 and jmp to it------------------------

        mov si, addr_mbr        ; from 0:7c00
        mov di, new_point       ; to 0:0600
        mov cx, size_mbr
        rep movsb

        push new_point + (newPointStart - myMbrStart)
        ret                     ; jmp to 0:0600 + start code find active partition.

;--------------------------------------------------------------------------------

newPointStart:

org new_point + (newPointStart - myMbrStart)

;----- read and check correct password ------------------------------------------
p:      mov ax,0x0003           ; clear display
        int 0x10

        mov ah, 0x0e
        lea si, [mes3]
p1:     lodsb
        int 0x10                ; write 'pass:'
        test al, al
        jnz p1

        mov di, buff_pass       ; addr for entered password.
p2:     mov ah, 0               ;
        int 0x16                ; input password.
        cmp al, press_enter     ; ENTER - to end input pass.
        je crc                  ; jump to compair CRC inputing pass and CRC-pass.
        stosb
        loop p2
crc:
        call getCRC             ; return ebx = CRC inputing pass.
        cmp ebx, password_crc   ; compair CRC-input-pass and CRC-pass.
        je f                    ; OK pass.

        push mes4               ; wrong pass.
        call write              ; write 'wrong...'

e:      mov ah, 0               ;
        int 0x16                ; wait until press ENTER.
        cmp al, press_enter     ;
        jnz e                   ;

        jmp p                   ; try input pass againt....
;--------------------------------------------------------------------------------

;----- find active partition ----------------------------------------------------
f:
        mov bp, partition1
        mov ax, 0x0080
        mov cx, 4               ; because we need check all 4 partition.

a:      cmp [bp], ah            ; check for correct(only 0 or 80h).
        jl a3                   ; not 0. may be 80h?

        add bp, 0x10            ; check next partition.
        loop a

        int 0x18                ; no active found. return to BIOS.

a3:     cmp [bp], al            ; 80h?
        je load_VBR             ; YES. load VBR.
        push invalidPartition   ; NO. mes and 'jmp $'
        call write
        jmp $

;----- load VBR active partition, check 0x55AA and start OS ---------------------
load_VBR:
        mov ax, 0x0201
        mov dh, [bp + 1]        ; dh = head.
        mov cl, [bp + 2]        ; cl = sector.
        mov ch, [bp + 3]        ; ch = cylinder.
        mov bx, addr_mbr
        mov dl, 0x80
        int 0x13
        jc errVBR

        mov ax, [0x7DFE]        ;
        cmp ax, 0xAA55          ; check signature 55AA
        jne errSig

        mov ax, addr_mbr        ;
        push 0                  ;
        push ax                 ;
        retf                    ; jump to 0:7C00, where was VBR upload.

errSig: push missingOS          ; mes: Missing OS.
        jmp err_

errVBR: push errReadVBR         ; mes: Error read VBR.
err_:   call write
        jmp $

;--------------------------------------------------------------------------------

;--------------------------------------------------------------------------------
;    write
;    in : addr message
;--------------------------------------------------------------------------------
write:
       push bp
       mov bp, sp

       mov ah,0x0e
       xor bx, bx
       mov si,[bp + 4]
m:     lodsb
       test al, al
       je m2
       int 0x10
       jmp m
m2:
       mov sp, bp
       pop bp
       ret 2
;--------------------------------------------------------------------------------

;--------------------------------------------------------------------------------
;    CRC32
;    in  :  di  = addr input password
;    out :  ebx = CRC32 input password
;--------------------------------------------------------------------------------
getCRC:
        ;poly_r        equ 0xEDB88320
        poly          equ 0x04C11DB7
        init          equ 0x00000000
        ;init          equ 0xFFFFFFFF
        xorOut        equ 0xFFFFFFFF
        refIn         equ 0
        refOut        equ 0

        push bp
        mov bp, sp

        xor al, al              ;
        mov ecx, 0x4            ;
        rep stosb               ; align password  waidth algoritm.

        mov cx, di
        mov bx, buff_pass
        sub cx, bx              ; length inputed password.

        dec di                  ; set correct addr password.

        mov ebx, init           ; register(CRC). 0x00000000
        mov esi, buff_pass      ; password.

h:
        mov eax, ebx
        and eax, 0xFF000000      ; get TOP byte of Register.

        push ecx                 ; save counter chars of message.
        mov ecx, 8
h2:                              ; Get need poly for char.
        shl eax, 1               ; check bit.
        jnc h3                   ; if 0 -> next bit.
        xor eax, poly            ; if 1 -> xor with 'poly'.
h3:
        loop h2

        pop ecx

        push eax                 ; save poly.
        lodsb                    ; al = char of message.
        shl ebx, 8               ;
        or bl, al                ; TOP byte left, NEXT byte message came.
        pop eax

        xor ebx, eax             ; xor CRC with Poly.

        loop h                   ; next char.

        mov sp, bp
        pop bp
        ret
;--------------------------------------------------------------------------------

mes3              db 'pass:', 0
mes4              db 'wrong...', 0

invalidPartition  db '[-] Invalid PT', 0
errReadVBR        db '[-] Err read VBR', 0
missingOS         db '[-] Sig err', 0

ENDS:

times 510 - (($ - $$) + (newPointStart - addr_mbr)) db 0
db 0x55, 0xAA

