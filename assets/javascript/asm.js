.
 _                _ _     _   _            _
| |              (_) |   | | | |          | |
| |     ___  __ _ _| |_  | |_| | __ _  ___| | _____ _ __
| |    / _ \/ _` | | __| |  _  |/ _` |/ __| |/ / _ \ '__|
| |___|  __/ (_| | | |_  | | | | (_| | (__|   <  __/ |
\_____/\___|\__, |_|\__| \_| |_/\__,_|\___|_|\_\___|_|
            __/ |
           |___/





; get filename
; parse filename into subdirs
; locate root dir and cluster size
; follow subdir routing to filename
; report file size, date & time
;
        .MODEL small
        .STACK 0200h
        .586P

        .DATA
PartEntry STRUC
        Bootable        db ?    ;80h = bootable, 00h = nonbootable
        BeginHead       db ?    ;beginning head
        BeginSector     db ?    ;beginning sector
        BeginCylinder   db ?    ;beginning cylinder
        FileSystem      db ?    ;name of file system
        EndHead         db ?    ;ending head
        EndSector       db ?    ;ending sector
        EndCylinder     db ?    ;ending cylinder
        StartSector     dd ?    ;starting sector (relative to beg. of disk)
        PartSectors     dd ?    ;number of sectors in partition
PartEntry ENDS

BootSector STRUC
        Jump            db ?            ;E9 xx xx or EB xx 90
        JumpTarget      dw ?            ;E9 xx xx or EB xx 90
        OemName         db '????????'   ;OEM name & version
                                        ;Start of BIOS parameter block
        BytesPerSec     dw ?            ;bytes per sector
        SecPerClust     db ?            ;sectors per cluster
        ResSectors      dw ?            ;number of reserved sectors
        FATs            db ?            ;number of file allocation tables
        RootDirEnts     dw ?            ;number of root-dir entries
        Sectors         dw ?            ;total number of sectors
        Media           db ?            ;media descriptor byte
        FATsecs         dw ?            ;number of sectors per FAT
        SecPerTrack     dw ?            ;sectors per track
        Heads           dw ?            ;number of heads
        HiddenSecs      dd ?            ;number of hidden sectors
        HugeSectors     dd ?            ;num sectors if Sectors==0
                                        ;End of BIOS parameter block
BootSector ENDS

DirEntry STRUC
        FileName        db '????????'   ;name
        Extension       db '???'        ;extension
        Attributes      db ?            ;attributes
        Reserved        db 10 dup (?)   ;reserved
        Time            dw ?            ;time stamp
        Date            dw ?            ;date stamp
        StartCluster    dw ?            ;starting cluster
        FileSize        dd ?            ;file size
DirEntry ENDS

BootFileName  db  "CONFIG  SYS"         ;the boot loader for this OS
MBR     DB      0200h DUP (?)
buff    DB      0200h * 40h DUP (?)
ClustOffs       dd      ?

CR      EQU     0DH
LF      EQU     0AH


        .CODE
main PROC
        STARTUPCODE                     ;initialize stuff
        call    FetchMBR C              ;fetch the master boot record
        jc      @@exit
        mov     cx,4                    ;search up to four partitions
        add     bx,01aeh                ;point to partition table (-10h)
@@FindBootable:
        add     bx,10h                  ;point to next entry
        cmp     BYTE ptr [bx],80h       ;is it a bootable partition?
        loopnz  @@FindBootable
        call    FetchSector C,                                \
                WORD ptr [(PartEntry PTR bx).BeginHead],      \
                WORD ptr [(PartEntry PTR bx).BeginSector],    \
                WORD ptr [(PartEntry PTR bx).BeginCylinder],  \
                OFFSET MBR, ds ;SEG MBR
;
; here's the point at which our OS loader would begin, with the
; BootSector structure in memory.
;
        mov     bx, OFFSET MBR
        call    CalcClustOff C, \
                WORD ptr [(BootSector PTR bx).ResSectors],    \
                WORD ptr [(BootSector PTR bx).FATsecs],       \
                WORD ptr [(BootSector PTR bx).FATs],          \
                WORD ptr [(BootSector PTR bx).RootDirEnts],   \
                WORD ptr [(BootSector PTR bx).BytesPerSec],   \
                WORD ptr [(BootSector PTR bx).SecPerClust]
        mov     WORD ptr [ClustOffs],ax
        mov     WORD ptr [ClustOffs+2],dx
        call    CalcClust2 C,                                 \
                WORD ptr [(BootSector PTR bx).ResSectors],    \
                WORD ptr [(BootSector PTR bx).FATsecs],       \
                WORD ptr [(BootSector PTR bx).FATs]
        ; now dx:ax contains the logical sector for cluster 2
        call    LsectToGeom C,                                \
                ax, dx,                                       \
                WORD ptr [(BootSector PTR bx).HiddenSecs]  ,  \
                WORD ptr [((BootSector PTR bx).HiddenSecs)+2],\
                [(BootSector PTR bx).Heads],                  \
                [(BootSector PTR bx).SecPerTrack]

        mov     dl,80h
        mov     bx,offset buff
        mov     al,[(BootSector PTR MBR).SecPerClust]
        mov     ah,2h                   ; get ready to read
        int     13h
        ; now find our desired filename within buffer (which has the root dir)

        call    FindFile C, \
                bx, 200h * 40h, offset BootFileName
        xor     dh,dh
        mov     dl,[(BootSector PTR MBR).SecPerClust]
        mov     si,ax
        mov     ax,[(DirEntry PTR si).StartCluster]
        mul     dx
        add     ax,WORD ptr [ClustOffs]
        adc     dx,WORD ptr [ClustOffs+2]
        ; now dx:ax contains logical sector number for start of file

        call    LsectToGeom C, \
                ax, dx, \
                WORD ptr [(BootSector PTR MBR).HiddenSecs]  ,  \
                WORD ptr [((BootSector PTR MBR).HiddenSecs)+2],\
                [(BootSector PTR MBR).Heads],                  \
                [(BootSector PTR MBR).SecPerTrack]
        mov     dl,80h
        mov     ax,204h                 ; read in 2k worth of data
        int     13h

@@exit:
        EXITCODE                        ;exit to DOS
ENDP    main

;
; FetchMBR -    fetches the Master Boot Record from the first physical
;               hard disk and stores it in the location MBR.
;
; INPUT:     none
; OUTPUT:    AX is error code if CY set, ES:BX ==> MBR
; DESTROYED: none
;
FetchMBR PROC    C
        USES    cx, dx                  ;save registers we'll use
        mov     dx,80h                  ;first physical disk
        mov     cx,1                    ;head 1, sector 0
        mov     bx,ds                   ;
        mov     es,bx                   ;point to boot record buffer
        mov     bx,OFFSET MBR           ;read into boot record
        mov     ax,0201h                ;read one sector
        int     13h                     ;BIOS read
        ret                             ;return to main
FetchMBR ENDP

;
; FetchSector - fetches the physical sector described by the passed
;               parameters and stores it in the named buffer
;
; INPUT:     head, sector, cylinder, buffer
; OUTPUT:    AX is error code if CY set, ES:BX ==> Boot
; DESTROYED: none
;
FetchSector PROC C  head:BYTE, sector:BYTE, cylinder:BYTE, buffer:DWORD
        USES    cx, dx                  ;save registers we'll use
        mov     ch, [cylinder]          ;
        mov     cl, [sector]            ;
        mov     dh, [head]              ;
        mov     dl, 80h                 ;first physical hard drive
        les     bx, [buffer]            ;
        mov     ax,0201h                ;read one sector
        int     13h                     ;BIOS read
        ret                             ;return to main
FetchSector ENDP

;
; GeomToLsect - converts to logical sector number from the physical
;               geometry (head, cylinder, track).  See LsectToGeom.
;
; INPUT:     cx, dx are set with cylinder/track, and head respectively
;            HiddenSecs, Heads, SecPerTrack
; OUTPUT:    lsect
; DESTROYED: none
;
GeomToLsect PROC    C lsect:DWORD, dHiddenSecs:DWORD,    \
                      dHeads:WORD, dSecPerTrack:WORD, buffer:DWORD
        USES    ax                      ;save registers we'll use
        mov     ax, WORD ptr [lsect]    ;load lsect into DX:AX
        mov     dx, WORD ptr [lsect+2]  ;
        stc                             ;add one additional
        adc     ax, WORD ptr [dHiddenSecs]   ;add starting sector
        adc     dx, WORD ptr [dHiddenSecs+2] ;
        div     [dSecPerTrack]          ;
        mov     cl,dl                   ;store sector in cl
        xor     dx,dx                   ;
        div     [dHeads]                ;
        mov     dh,dl                   ;store head in dh
        mov     ch,al                   ;store low 8 bits of cylinder in ch
        shr     ax,1                    ;
        shr     ax,1                    ;
        and     al,0c0h                 ;pass through two hi bits only
        or      cl,ah                   ;mov bits into location
        ret                             ;
GeomToLsect ENDP

;
; LsectToGeom - converts from logical sector number to the physical
;               geometry (head, cylinder, track) in the form required
;               by the BIOS (Int 13h) disk read and write calls.
;
; INPUT:     lsect, HiddenSecs, Heads, SecPerTrack
; OUTPUT:    cx, dx are set with cylinder/track, and head respectively
; DESTROYED: none
;
LsectToGeom PROC    C lsect:DWORD, lHiddenSecs:DWORD,    \
                      lHeads:WORD, lSecPerTrack:WORD, buffer:DWORD
        USES    ax                      ;save registers we'll use
        mov     ax, WORD ptr [lsect]    ;load lsect into DX:AX
        mov     dx, WORD ptr [lsect+2]  ;
        stc                             ;add one additional
        adc     ax, WORD ptr [lHiddenSecs]   ;add starting sector
        adc     dx, WORD ptr [lHiddenSecs+2] ;
        div     [lSecPerTrack]          ;
        mov     cl,dl                   ;store sector in cl
        xor     dx,dx                   ;
        div     [lHeads]                ;
        mov     dh,dl                   ;store head in dh
        mov     ch,al                   ;store low 8 bits of cylinder in ch
        shr     ax,1                    ;
        shr     ax,1                    ;
        and     al,0c0h                 ;pass through two hi bits only
        or      cl,ah                   ;mov bits into location
        ret                             ;
LsectToGeom ENDP

;
; CalcClust2  - calculates the starting logical sector number of
;               cluster 2, (the beginning of data space for
;               partitions).
;
; INPUT:     ResSectors, FATsecs, FATs
; OUTPUT:    dx:ax contains the starting logical sector number
; DESTROYED: none
;
CalcClust2 PROC    C cResSectors:WORD, cFATsecs:WORD, cFATs:BYTE
        xor     dx,dx                   ;
        mov     ax,[cFATsecs]           ;
        mul     [cFATs]                 ;
        add     ax,[cResSectors]        ;
        adc     dx,0                    ;
        ret
CalcClust2 ENDP

;
; CalcClustOff - calculates the starting logical sector number of
;               cluster 0, which isn't really a cluster, but the
;               number returned is useful for calculations converting
;               cluster number to logical sector
;
; INPUT:     ResSectors, FATsecs, FATs
; OUTPUT:    dx:ax contains the starting logical sector number
; DESTROYED: none
;
CalcClustOff PROC    C dResSectors:WORD, dFATsecs:WORD, dFATs:BYTE, \
        dRootDirEnts:WORD, dBytesPerSec:WORD, dSecPerClust:BYTE
        LOCAL clustLo:WORD, clustHi:WORD
        xor     dh,dh
        mov     ax,[dFatSecs]
        mov     dl,[dFATs]
        mul     dx
        add     ax,[dResSectors]
        adc     dx,0
;        call    CalcClust2 C, [dResSectors], [dFATsecs], [dFATs]
        ; now dx:ax = FATs * FATsecs + ResSectors
        mov     [clustLo],ax
        mov     [clustHi],dx
        mov     dx,20h                  ; bytes per dir entry
        mov     ax,[dRootDirEnts]       ;
        mul     dx                      ; multiply 'em out
        div     [dBytesPerSec]          ; and divide by bytes/sec
        add     [clustLo],ax            ;
        adc     [clustHi],dx            ; create the aggregate
        mov     al,[dSecPerClust]       ;
        xor     ah,ah                   ;
        shl     ax,1                    ; AX = SecPerClust * 2
        sub     [clustLo],ax            ;
        sbb     [clustHi],0             ; propagate carry flag
        mov     ax,[clustLo]            ;
        mov     dx,[clustHi]            ;
        ret
CalcClustOff ENDP

;
; FindFile -    given a memory buffer containing the directory data
;               and a static file name for which to search, this routine
;               finds the file and returns a pointer to its directory
;               entry in ds:si
;
; INPUT:        dirbuffer, filespec
; OUTPUT:       ax    contains pointer to directory entry (or NULL)
; DESTROYED:    none
;
FindFile PROC C dirbuffer:WORD, limit:WORD, filespec:WORD
        USES    cx, dx, di, si, es
        mov     cx,ds                   ;
        mov     es,cx                   ; es and ds point to same segment
        cld                             ; always count forward
        mov     ax,[dirbuffer]          ; load 'em up
        add     [limit],ax
        mov     dx,[filespec]           ;
keepsearching:
        mov     cx,11                   ; size of dos filename (8.3)
        mov     si,dx                   ;
        mov     di,ax                   ;
        repe    cmpsb                   ; compare 'em
        jz      foundit                 ;
        add     ax,20h                  ; size of directory entry
        cmp     ax,[limit]
        jb      keepsearching
        xor     ax,ax

foundit:
        ret
FindFile ENDP
        END
                .model tiny
                .code
                .586P

        DESC386 STRUC
                limlo   dw      ?
                baselo  dw      ?
                basemid db      ?
                dpltype db      ?       ; p(1) dpl(2) s(1) type(4)
                limhi   db      ?       ; g(1) d/b(1) 0(1) avl(1) lim(4)
                basehi  db      ?
        DESC386 ENDS

                ORG 100h
        start:
                call  flatmode          ; go into flat real mode (fs reg only)
        ;        mov dx,5                ;
        ;        mov fs,dx               ;
                call  fillscreen        ; fill the screen using 4G descriptor
                mov ax,4c00h            ; do a standard DOS exit
                int 21h                 ;
        fillscreen proc
                mov     esi,0F0050h     ; point to ROM
        ifdef BEROSET
                mov     edi,0B8000h     ; point to screen
        else
                mov     di,0b800h       ;
                mov     es,di           ;
                xor     edi,edi         ;
        endif
                mov     cx,160          ; just two lines
                mov     ah,1Eh          ; yellow on blue screen attrib
        myloop:
                mov     al,fs:[esi]     ; read ROM byte
        ifdef BEROSET
                mov     fs:[edi],ax     ; store to screen with attribute
        else
                mov     es:[di],ax      ; store to screen with attribute
        endif
                inc     esi             ; increment source ptr
                inc     edi             ; increment dest ptr by two
                inc     edi             ;
                loop    myloop          ; keep going
                ret                     ; and quit
        fillscreen endp
        flatmode proc
                ; first, calculate the linear address of GDT
                xor     edx,edx         ; clear edx
                xor     eax,eax         ; clear edx
                mov     dx,ds           ; get the data segment
                shl     edx,4           ; shift it over a bit
                add     dword ptr [gdt+2],edx   ; store as GDT linear base addr

                ; now load the GDT into the GDTR
                lgdt    fword ptr gdt   ; load GDT base (286-style 24-bit load)
                mov     bx,1 * size DESC386 ; point to first descriptor
                mov     eax,cr0         ; prepare to enter protected mode
                or      al,1            ; flip the PE bit
                cli                     ; turn off interrupts
                mov     cr0,eax         ; we're now in protected mode
                mov     fs,bx           ; load the FS segment register
                and     al,0FEh         ; clear the PE bit again
                mov     cr0,eax         ; back to real mode
                sti                     ; resume handling interrupts
                ret                     ;
        flatmode endp
        GDT     DESC386 <GDT_END - GDT - 1, GDT, 0, 0, 0, 0>  ; the GDT itself
                DESC386 <0ffffh, 0, 0, 091h, 0cfh, 0>          ; 4G data segment
        GDT_END:
        end start
VID_BIOS_SEG    equ     0c000h          ; the video BIOS segment

SEARCH_AREA     equ     0400h           ; when we look for the video BIOS
                                        ; ID, we only seach this many bytes

CMP_LENGTH      equ     7               ; the number of "significant"
                                        ; characters to compare in each
                                        ; string

DOS_INT         equ     21h             ; DOS' interrupt
VIDEO_INT       equ     10h             ; BIOS' video services int

VID_80x25       equ     03h             ; 80x25 text mode number
UnknownCardMsg   db   'Your video card is not supported.',13,10,'$'

mode1   db      053h                    ; 640 x 480 x 256 mode for Oak
card1   db      'OAK VGA','$'
mode2   db      05dh                    ; 640 x 480 x 256 mode for Trident
card2   db      'TRIDENT','$'

cards   dw      card1, card2, 0
@DosPrint MACRO msgptr
        mov     dx,msgptr               ; handy macro for printing
        mov     ah,9                    ;   '$' terminated strings
        int     DOS_INT                 ;   under DOS
ENDM

@SetVidMode MACRO vmode
        ifnb <vmode>
                mov     ax,(vmode AND 0ffh)
        else
                xor     ah,ah
        endif
        int     VIDEO_INT
ENDM

        .code
main    proc
        .STARTUP                        ; do the usual startup stuff
        call    IDVideoBios             ; identify video BIOS
        or      si,si                   ; Q: unknown card?
        jz      NoSupport               ;  Y: tell the user the bad news
        dec     si                      ; point to video mode (mode1, mode2)
        lodsb                           ; load into al & increment si
        @SetVidMode                     ; change video mode

        @SetVidMode <VID_80x25>         ; switch back to 80x25 text mode
        @DosPrint <si>                  ; print ID string
        .EXIT  0                        ; exit with error code = 0

NoSupport:
        @DosPrint <offset UnknownCardMsg> ; print unknown card message
        .EXIT  1                        ; exit with error code = 1
main    endp


IDVideoBios     proc
        push    ax                      ; save used regs
        push    bx                      ;
        push    cx                      ;
        push    es                      ;
        mov     ax,VID_BIOS_SEG         ; point to video BIOS
        mov     es,ax                   ;
        mov     bx,OFFSET cards         ; point to first card entry
        cld                             ; we'll be scanning forward
next_card:
        mov     si,[bx]                 ;
        or      si,si                   ; Q: is it a NULL pointer?
        jz      @@exit                  ;  Y: we're done, so exit now
        xor     di,di                   ; es:di ==> video BIOS area
        mov     cx,SEARCH_AREA          ;
        mov     al,[si]                 ; get the first letter
scan:
        repne   scasb                   ; scan for AL in es:di
        jnz     nofind                  ; if we didn't find it, skip over
;
; if we got here, ES:DI points to one letter after the matching letter
; in the video BIOS.  We need to compare the rest to assure that we have
; a complete match.
;
        push    cx                      ; temporarily save regs
        push    di                      ;
        push    si                      ;
        inc     si                      ; point to next letter
        mov     cx,CMP_LENGTH-1         ; compare the rest the string
        repe    cmpsb                   ; do it
        pop     si                      ; restore regs
        pop     di                      ;
        pop     cx                      ;
        jz      @@exit                  ; if match, we're done
nofind:
        or      cx,cx                   ; Q: is the count down to zero?
        jnz     scan                    ;   N: keep scanning
        inc     bx                      ;   Y: point to next card
        inc     bx                      ; point to next card entry
        jmp     next_card               ; go back for more
@@exit:
        pop     es                      ; restore used registers
        pop     cx                      ;
        pop     bx                      ;
        pop     ax                      ;
        ret                             ;
IDVideoBios     endp

        end