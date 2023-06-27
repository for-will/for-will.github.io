---
layout: post
title: "使用NASM重写Linux内核的head.s"
subtitle: ""
date: 2023-06-27
author: "will"
tags: [linux, asm, nasm, kernel]
---

boot/bootsect.nasm
------------------

```nasm
; SYS_SIZE is the number of clicks (16 bytes) to be loaded. 
; 0x3000 is 0x30000 bytes = 196kB, more than enough for current 
; versions of linux

; #include <linux/config.h> 
DEF_SYSSIZE     equ 0x3000      ; 默认系统模块长度。单位是节，每节16字节；
DEF_INITSEG     equ 0x9000      ; 默认本程序代码移动目的段位置；
DEF_SETUPSEG    equ 0x9020      ; 默认setup程序代码段位置；
DEF_SYSSEG      equ 0x1000      ; 默认从磁盘加载系统模块到内存的段位置。
SYSSIZE         equ DEF_SYSSIZE ; 编译连接后system模块的大小。

;
;   bootsect.s      (C) 1991 Linus Torvalds
;   modified by Drew Eckhardt
;
; bootsect.s is loaded at 0x7c00 by the bios-startup routines, and moves
; iself out of the way to address 0x90000, and jumps there.
;
; It then loads 'setup' directly after itself (0x90200), and the system
; at 0x10000, using BIOS interrupts. 
;
; NOTE! currently system is at most 8*65536 bytes long. This should be no
; problem, even in the future. I want to keep it simple. This 512 kB
; kernel size should be enough, especially as this doesn't contain the
; buffer cache as in minix
;
; The loader has been made as simple as possible, and continuos
; read errors will result in a unbreakable loop. Reboot by hand. It
; loads pretty fast by getting whole sectors at a time whenever possible.

SETUPLEN        equ 4                   ; nr of setup-sectors
BOOTSEG         equ 0x07c0              ; original address of boot-sector
INITSEG         equ DEF_INITSEG         ; we move boot here - out of the way
SETUPSEG        equ DEF_SETUPSEG        ; setup starts here 0x90200
SYSSEG          equ DEF_SYSSEG          ; system loaded at 0x10000 (65536).
ENDSEG          equ SYSSEG + SYSSIZE    ; where to stop loading

; ROOT_DEV & SWAP_DEV are now written by "build".
ROOT_DEV    equ 0                   ; 根文件系统设备使用与系统引导时同样的设备；
SWAP_DEV    equ 0                   ; 交换设备使用与系统引导时同样的设备；

start:
        mov ax, BOOTSEG                 ; 将ds段寄存器置为0x7c0
        mov ds, ax
        mov ax, INITSEG                 ; 将es段寄存器置为0x9000
        mov es, ax
        mov cx, 256
        sub si, si
        sub di, di
        rep movsw
        jmp INITSEG:go

go:
        mov ax, cs
        mov dx, 0xfef4                  ; arbitary value >> 512 - disk parm size
                                    ; 在这个位置上会放一个自建的驱动器参数表
                                    ; 0xff00 -12（参数表长度），即sp = 0xfef4
        mov ds, ax
        mov es, ax
        push ax

        mov ss, ax
        mov sp, dx

; Many BIOS's default disk parameter tables will not
; recognize multi-sector reads beyoud the maximum sector number
; specified in the default diskette parameter tables - this may
; mean 7 sectors in some cases.
;
; Since single sector reads are slow and out of the question,
; we must take care of this by creating new parameter tables
; (for the first disk) in RAM. We will set the maximum sector
; cout to 18 - the most we will encounter on an HD 1.44.
;
; Hight doesn't hurt. Low does.
; 
; Segments are as follows: ds=es=ss=cs - INITSEG,
;         fs = 0, gs = parameter table segment

        push 0
        pop fs
        mov bx, 0x78                    ; fs:bx is parameter table address

        lgs si, [fs:bx]                 ; gs:si is source

        mov di, dx                      ; es:di is destination ; dx=0xfef4
        mov cx, 6                       ; copy 12 bytes
        cld

        rep gs movsw                    ; rep movsw word ptr es:[di], word ptr gs:[si]

        mov di, dx
        mov byte [di+4], 18             ; patch sector count

        mov [fs:bx], di
        mov [fs:bx+2], es

        pop ax
        mov fs, ax
        mov gs, ax

        xor ah, ah                      ; reset FDC ; 复位软盘控制器，让其采用新参数。
        xor dl, dl                      ; dl = 0, 第1个软驱
        int 0x13

; load the setup-sectors directly after the bootblock.
; Note that 'es' is already set up.

; INT 0x13读扇区使用调用参数设置如下：
; ah = 0x02 - 读磁盘扇区到内存； al = 需要读出的扇区数量；
; ch = 磁道(柱面)号的低8位；     cl = 开始扇区(位0-5)，磁道号高2位(位6-7)；
; dh = 磁头号；；               dl = 驱动器号(如果是硬盘则位7要置位)；
; es:bx -> 指向数据缓冲区；如果出错则CF标志置位，ah是出错码。   
; wiki: https://en.wikipedia.org/wiki/INT_13H
load_setup:
        xor dx, dx                      ; drive 0, head 0
        mov cx, 0x0002                  ; sector 2, track 0
        mov bx, 0x0200                  ; adress = 512, in INITSEG
        mov ax, 0x0200+SETUPLEN         ; service 2, nr of sectors
        int 0x13                        ; read it
        jnc ok_load_setup               ; ok - continue

        push ax                         ; dump error code 
        call print_nl
        mov bp, sp                      ; ss:bp指向欲显示的字（world）
        call print_hex
        pop ax

        xor dl, dl                      ; reset FDC ; 复位磁盘控制器，复试
        xor ah, ah
        int 0x13
        jmp load_setup

ok_load_setup:

; Get disk drive parameters, specifically nr of sectors/track
; 这段代码利用BIOS INT 0x13功能号8来取磁盘驱动器的参数。实际是取每磁道扇区数，并保存在
; 位置sectors处。取磁盘驱动器参数INT 0x13调用格式和返回信息如下：
; ah = 0x08     dl = 驱动器号（如果是硬盘则要置位7为1）。
; 返回信息：
; 如果出错则CF置位，并且 ah = 状态码。
; ah = 0 al = 0                 bl = 驱动器类型(AT/PS2)
; ch = 最大磁道号的低8位           cl = 每磁道最大扇区数(位 0-5)，最大磁道号高2位(w位 6-7)
; dh = 最大磁头数                 dl = 驱动器数量 
; es:di --> 软驱磁盘参数表。
        xor dl, dl
        mov ah, 0x08                    ; ah =8 is get drive parameters
        int 0x13
        xor ch, ch
        mov [cs:sectors], cx
        mov ax, INITSEG
        mov es, ax                      ; 因为上面取磁盘参数中断改了es值，这里重新改回

; Print some inane message
; 下面利用BIOS INT 0x10 功能0x03和0x13来显示信息：“'Loading'+回车+换行”，显示包括
; 回车和换行控制字符在内共9个字符。
; BIOS中断0x10功能号 ah = 0x03, 记取光标位置。
; 输入：bh = 页号
; 返回：ch = 扫描开始线； cl = 扫描结束线； dh = 行号(px00顶端)； dl = 列号(0x00最左边)
;
; BIOS中断0x10功能号 ah = 0x13，显示字符串。
; 输入：al = 放置光标的方式及规定属性。0x01-表示使用bl中的属性值，光标停在字符串结尾处。
; bh = 显示页面号； bl =字符属性； dh = 行号； dl = 列号。 cx = 显示的字符串字符数。
; es:bp 此寄存器对指向要显示的字符串起始位置处。
    
        mov ah, 0x03
        xor bh, bh
        int 0x10

        mov cx, 9
        mov bx, 0x0007                  ; page 0, attribute 7 (normal)
        mov bp, msg1
        mov ax, 0x1301                  ; write string, move cursor
        int 0x10

; ok, we've written the message, now
; we want to load the system (at 0x10000)

        mov ax, SYSSEG
        mov es, ax                      ; segment of 0x10000
        call read_it
        call kill_motor
        call print_nl

; After that we check which root-device to use. If the device is
; defined (!= 0), nothong is done and the given device is used.
; Otherwise, either /dev/PS0 (2,28) or /dev/at0 (2,8), depending
; on the number of sectors that the BIOS reports currently.
; 上面一行中两个设备文件的含义说明如下：
; 在Linux中软驱的主设备号是2，次设备号 = type*4 + nr，其中
; nr 为0-3分别对应软驱A、B、C或D；type是软驱的类型（2->12MB或7->1.44MB等）。
; 因为 7*4 + 0 = 28，所以/dev/PS0(2,28)指的是1.44MB A驱动器，其设备号是0x021c
; 同理 /dev/at0(2,8)指的是1.2MB A驱动器，其设备号是 0x0208。
        mov ax, [cs:root_dev]
        or ax, ax
        jne root_defined
        mov bx, [cs:sectors]
        mov ax, 0x0208                  ; /dev/at0 - 1.2MB
        cmp bx, 15
        je root_defined
        mov ax, 0x021c                  ; /dev/ps0 - 1.44MB
        cmp bx, 18
        je root_defined
undef_root:
        jmp undef_root
root_defined:
        mov [cs:root_dev], ax

; after that (everything loaded), we jump to
; the setnup-routine loaded directly after
; the bootblock:
        ; push dx
        ; push cx
        ; push bx
        ; push ax
        ; push 0x55aa
        ; call print_all
        ; hlt
        jmp SETUPSEG:0

; This routine loads the system at address 0x10000, making sure
; no 64 KB boundaries are crossed. We try to load it as fast as
; possible, loading whole tracks whenever we can.
;
; in:   es - staring address segment (normally 0x1000)
;
sread:  dw 1+SETUPLEN                   ; sectors read of current track
head:   dw 0                            ; current head
track:  dw 0                            ; current track

read_it:
        mov ax, es
        test ax, 0x0fff
die:    jne die                         ; es must be at 64KB boundary
        xor bx, bx                      ; bx is starting address within segment
rp_read:
        mov ax, es
        cmp ax, ENDSEG                  ; have we loaded allyet?
        jb ok1_read
        ret
ok1_read:
        mov ax, [cs:sectors]
        sub ax, [sread]
        mov cx, ax
        shl cx, 9
        add cx, bx
        jnc ok2_read
        je ok2_read
        xor ax, ax
        sub ax, bx
        shr ax, 9
ok2_read:
        call read_track
        mov cx, ax
        add ax, [sread]
        cmp ax, [cs:sectors]            ; 若当前磁道上还有扇区未读，则跳转到ok3_read处
        jne ok3_read
        mov ax, 1
        sub ax, [head]
        jne ok4_read                    ; 如果是0磁头，则再去读1磁头面上的扇区数据
        inc word [track]                ; 否则去读下一磁道
ok4_read:
        mov [head], ax                  ; 保存当前磁头号
        xor ax, ax                      ; 清零当前磁道已读扇区数
ok3_read:
        mov [sread], ax
        shl cx, 9
        add bx, cx
        jnc rp_read
        mov ax, es
        add ah, 0x10                    ; 将段基地址调整为指向下一个64KB内存开始处。
        mov es, ax
        xor bx, bx                      ; 清零段内数据开始偏移值
        jmp rp_read

; read_track 子程序。读当前磁道上指定开始扇区和需读扇区数的数据到 es:bx 开始处。
; al - 需读扇区数            es:bx - 缓冲区开始位置
read_track:
; 首先调用BIOS中断0x10，功能0x0e（以电传方式写字符），光标前移一位置。显示一个‘.’
        pusha
        pusha
        mov ax, 0x0e2e                  ; loading... message 2e = .
        mov bx, 7                       ; 字符前景色属性
        int 0x10
        popa

; 然后正式进行磁道扇区读操作
        mov dx, [track]
        mov cx, [sread]
        inc cx                          ; cl = 开始读扇区
        mov ch, dl                      ; ch = 当前磁道号
        mov dx, [head]
        mov dh, dl                      ; dh = 磁头号， dl = 驱动器号（为0表示当前A驱动器）
        and dx, 0x0100                  ; 磁头号不大于1
        mov ah, 2                       ; ah = 2, 读磁盘扇区功能号

        push dx                         ; save for error dump
        push cx
        push bx
        push ax

        int 0x13
        jc bad_rt
        add sp, 8
        popa
        ret

bad_rt: push ax                         ; save error code
        call print_all                  ; ah = error, al = read


        xor ah, ah
        xor dl, dl
        int 0x13                        ; 执行驱动器复位操作（磁盘中断功能号0）


        add sp, 10                      ; 丢弃为出错情况保存的信息
        pop ax
        jmp read_track

;
;       print_all is for debugging purpose.
;       It will print out all of the registers. The assumption is that this is
;       called from a routine, with a stack frame like
;       dx
;       cx
;       bx
;       ax
;       error
;       ret <- sp
;
print_all:
        mov cx, 5                       ; error code + 4 registers
        mov bp, sp

print_loop:
        push cx                         ; save count left
        call print_nl                   ; nl for readability
        jae no_reg                      ; see if register name is needed
        mov ax, 0x0e05 + 0x41 - 1       ; ah = 功能号（0x0e）；al = 字符（0x05 + 0x41 - 1）
        sub al, cl
        int 0x10

        mov al, 0x58                    ; X
        int 0x10

        mov al, 0x3a                    ; :
        int 0x10

no_reg:
        add bp, 2                       ; next register
        call print_hex                  ; print it
        pop cx
        loop print_loop
        ret

print_nl:
        mov ax, 0xe0d                   ; CR
        int 0x10
        mov al, 0xa                     ; LF
        int 0x10
        ret

;       print hex is for debugging purpose, and prints the word
;       pointed to by ss:bp in hexadecmial
print_hex:
        mov cx, 4                       ; 4 hex digits
        mov dx, [bp]                    ; load word into dx
print_digit:
        rol dx, 4                       ; rotate so that lowest 4 bits are used
        mov ah, 0x0e
        mov al, dl                      ; mask off so we have only next nibble
        and al, 0xf
        add al, 0x30                    ; convert to 0 based digit, '0'
        cmp al, 0x39                    ; check for overflow
        jbe good_digit
        add al, 0x41 - 0x30 - 0xa       ; 'A' - '0' - 0xa

good_digit:
        int 0x10
        loop print_digit                ; cx--
        ret

; This procedure turns off the floppy drive motor, so
; that we enter the kernel in a known state, and
; dont't have to worry about it later.
kill_motor:
        push dx
        mov dx, 0x3f2
        xor al, al
        out dx, al
        pop dx
        ret

sectors:
        dw 0

msg1:
        db 13, 10
        db 'Loading'



;;;;;;;;;;;;;;;;;;;;;;;;
times 506-($-$$) db 0
swap_dev:
        dw SWAP_DEV
root_dev:
        dw ROOT_DEV
boot_flag:
        db 0x55, 0xaa

```

boot/setup.nasm
------------

```nasm
; setup.s is responsible for getting the system data from the BIOS,
; and putting them into the appropriate places in system memory.
; both setup.s and system has been loaded by the bootblock.
; 
; This code asks the bios for memory/disk/other parameters, and
; puts them in a "safe" place: 0x90000-0x901FF, ie where the
; boot-block used to be. It is then up to the protected mode
; system to read them from there before the area is overwritten
; for buffer-blocks.

; NOTE! These had better be the same as in bootsect.s!
;#include <linux/config.h>
DEF_SYSSIZE     equ 0x3000      ; 默认系统模块长度。单位是节，每节16字节；
DEF_INITSEG     equ 0x9000      ; 默认本程序代码移动目的段位置；
DEF_SETUPSEG    equ 0x9020      ; 默认setup程序代码段位置；
DEF_SYSSEG      equ 0x1000      ; 默认从磁盘加载系统模块到内存的段位置。
SYSSIZE         equ DEF_SYSSIZE ; 编译连接后system模块的大小。

INITSEG         equ DEF_INITSEG ; we move boot here - out of the way
SYSSEG          equ DEF_SYSSEG  ; system loaded at 0x10000 (65536)
SETUPSEG        equ DEF_SETUPSEG; this is the current segment

start:

; ok, the read went well so we get current cursor position and save it for
; posterity
        mov     ax, INITSEG
        mov     ds, ax

; Get memory size (extended men, KB)
; 取扩展内存的大小（KB）
; 利用BIOS中断0x15 功能号 ah = 0x88 取系统所含扩展内存大小并保存在内存0x90002处。
; 返回：ax = 从0x100000(1M) 处开始的扩展内存大小(KB)。若出错则CF置位，ax = 出错码。
        mov     ah, 0x88
        int     0x15
        mov     [2], ax

; check for EGA/VGA and some config parameters
; 检查显示方式（EGA/VGA）并取参数。
; 调用BIOS中断0x10功能号0x12（视频子系统配置）取EBA配置信息。
; ah = 0x12, bl = 0x10 - 取EGA配置信息。
; 返回：
; bh = 显示状态（0x00 -彩色模式，I/O端口=0x3dX； 0x01 -单色模式，I/O端口=0x3bX）。
; bl = 安装的显示内存(0x00 - 64K; 0x01 - 128K; 0x02 - 192K; 0x03 = 256K。)
; cx = 显示卡特性参数(参见程序后对BIOS视频中断0x10的说明)。

        mov     ah, 0x12
        mov     bl, 0x10
        int     0x10
        mov     [8], ax         ; 0x90008 = ??
        mov     [10], bx        ; 0x9000A = 安装的显示内存； 0x9000B = 显示状态(彩/单色)
        mov     [12], cx        ; 0x9000C = 显示卡特性参数
; 检测屏幕当前行列值。若显示卡是VGA卡时则请求用户选择显示行列值，并保存到0x9000E处。
        mov     ax, 0x5019      ; 在ax中预置屏幕默认行列值（ah = 80列； al = 25行）。
        cmp     bl, 0x10        ; 若中断返回bl值为0x10，则表示不是VGA显示卡，跳转。
        je      novga
        call    chsvga          ; 检测显示卡厂家和类型，修改显示行列值
novga:  mov     [14], ax        ; 保存屏幕当前行列值（0x9000E, 0x9000F）

; 使用BIOS中断0x10功能0x03取屏幕当前光标位置，并保存在内存0x90000处（2字节）。
; 控制台初始化程序console.c会到此处读取该值。
; BIOS 中断0x10功能号 ah = 0x03, 读光标位置。
; 输入：bh = 页号
; 返回：ch = 扫描开始线； cl = 扫描结束线； dh = 行号（0x00顶端）； dl = 列号（0x00最左边）。
        mov     ah, 0x03        ; read cursor pos
        xor     bh, bh
        int     0x10            ; save it in known place, con_init fetches
        mov     [0], dx         ; it from 0x90000

; Get video-card data:
; 下面这段用于取显示卡当前显示模式。
; 调用BIOS中断0x10，功能号 ah = 0x0f。
; 返回：ah = 字符列数； al = 显示模式； bh = 当前显示页。
; 0x90004(1字)存放当前页；0x90006存放显示模式； 0x90007存放字符列数。

        mov     ah, 0x0f
        int     0x10
        mov     [4], bx         ; bh = display page
        mov     [6], ax         ; al = video mode, ah = window width

; Get hd0 data
; 取第一个硬盘的信息（复制硬盘参数表）。
; 第1个硬盘参数表的首地址竟然是中断0x41的中断向量值！而第2个硬盘参数表紧接在第1个
; 表的后面，中断0x46的向量值也指向第2个硬盘参数表首地址。表的长度是16个字节。
; 下面两段程序分别复制ROM BIOS中有关两个硬盘参数表到：0x90080处存放第1个硬盘的表，
; 0x90090处存放第2个硬盘的表。有关硬盘参数表内容说明。
        mov     ax, 0x0000
        mov     ds, ax
        lds     si, [4*0x41]    ; 取中断向量0x41的值，即hd0参数表的地址 -> ds:si
        mov     ax, INITSEG
        mov     es, ax
        mov     di, 0x0080
        mov     cx, 0x10
        rep movsb

; Get hd1 data

        mov     ax, 0x0000
        mov     ds, ax
        lds     si, [4*0x46]
        mov     ax, INITSEG
        mov     es, ax
        mov     di, 0x0090
        mov     cx, 0x10
        rep movsb

; Check that there IS a hd1 :-)
; 检查系统是否有第2个硬盘。如果没有则把第2个表清零。
; 利用BIOS中断调用0x13的取盘类型功能，功能号 ah = 0x15;
; 输入：dl = 驱动器号（0x8X是硬盘：0x80指第1个硬盘，0x81第2个硬盘）
; 输出：ah = 类型码； 00 - 没有这个盘，CFl置位； 01 - 是软驱，没有change-line支持；
;                   02 - 是软驱（或其他可移动设备），有change-line支持；
;                   03 - 是硬盘。

        mov     ax, 0x1500
        mov     dl, 0x81
        int     0x13
        jc      no_disk1
        cmp     ah, 3           ; 是硬盘吗?(类型 = 3)
        je      is_disk1
no_disk1:
        mov     ax, INITSEG     ; 第2个硬盘不存在，则对第2个硬盘表清零。
        mov     es, ax
        mov     di, 0x0090
        mov     cx, 0x10
        mov     ax, 0x00
        rep stosb
is_disk1:

; now we want to move to protected mode ...

        cli                     ; no interrupts allowed

; first we move the system to it's rightful place
        mov     ax, 0x0000
        cld                     ; 'direction'=0, movs moves forward
do_move:
        mov     es, ax          ; destination segment
        add     ax, 0x1000
        cmp     ax, 0x9000 
        jz      end_move
        mov     ds, ax
        sub     di, di
        sub     si, si
        mov     cx, 0x8000       ; 移动0x8000字（64KB字节）
        rep movsw
        jmp     do_move

; then we load the segment descriptors

end_move:
        mov     ax, SETUPSEG    ; right, forgot this at first.didn't work :-)
        mov     ds, ax
        lidt    [idt_48]        ; load idt with 0, 0
        lgdt    [gdt_48]        ; load gdt with whatever appropriate

; that was painless, now we enable A20
        call    empty_8042      ; 测试8042状态寄存器，等待输入缓冲器空。
                                ; 只有当输入缓冲器为空时才可以对其执行写命令。
        mov     al, 0xd1        ; command write ; 0xD1命令码 - 表示要写数据到
        out     0x64, al        ; 8042的P2端口。P2端口位1用于A20线的选通。
        call    empty_8042      ; 等待输入缓冲器空，看命令是否被接受。
        mov     al, 0xDF        ; A20 on                ! 选通A20地址线的参数。
        out     0x60, al        ; 数据要写到0x60口。
        call    empty_8042      ; 若此时输入缓冲器为空，则表示A20线已经选通。

; well, that went ok, I hope. Now we have to reprogram the interrupts :-(
; we put them right after the intel-reserved hardware interrupts, at
; int 0x20-0x2F. There they won't mess up anthing. Sadly IBM really
; messed this up with the original PC, and they haven't been able to
; rectify it afterwards. Thus the bios puts interrupts at 0x08-0x0f,
; which is used for the internal hardware interrupts as well. We just 
; have to reprogram the 8259's, and it isn't fun.
        mov     al, 0x11        ; initialization sequence
        out     0x20, al        ; sent it to 8259A-1    ; 发送到8259A主芯片。
        dw      0x00eb, 0x00eb  ; jmp $+2, jmp $+2      ; $表示当前指令的地址
        out     0xA0, al        ; and to 8259A-2
        dw      0x00eb, 0x00eb
; Linux系统硬件中断号被设置成从0x20开始
        mov     al, 0x20        ; start of hardware int's (0x20)
        out     0x21, al        ; 送主芯片ICW2命令字，设置起始中断号，要送奇端口。
        dw      0x00eb, 0x00eb
        mov     al, 0x28        ; start of hardware int's 2 (0x28)
        out     0xA1, al        ; 送从芯片ICW2命令字，从芯片的起始中断号。
        dw      0x00eb, 0x00eb
        mov     al, 0x04        ; 8259-1 is master
        out     0x21, al        ; 送主芯片ICW3命令字，主芯片的IR2连从芯片 INT。
        dw      0x00eb, 0x00eb
        mov     al, 0x02        ; 8259-2 is slave
        out     0xA1, al        ; 送从芯片ICW3命令字，表示从芯片的INT连到主芯片的IR2引脚上。
        dw      0x00eb, 0x00eb
        mov     al, 0x01        ; 8086 mode for both
        out     0x21, al        ; 送主芯片ICW4命令字。8086模式：普通EOI、非缓冲方式，需发送指令复位
                                ; 初始化结束，芯片就绪。
        dw      0x00eb, 0x00eb
        out     0xA1, al        ; 送从芯片ICW4，内容同上。
        dw      0x00eb, 0x00eb
        mov     al, 0xFF        ; mask off all interrupts for now
        out     0x21, al        ; 屏蔽主芯片所有中断请求。
        dw      0x00eb, 0x00eb
        out     0xA1, al        ; 屏蔽从芯片所有中断请求。

; well, that certainly wasn't fun :-(. Hopefully it works, and we don't
; need no steenking BIOS anyway (except for the initial loading :-).
; The BIOS-routine wants lots of unnecessary data, and it's less 
; "interesting" anyway. This is how REAL programmers do it.
;
; Wellm now's the time to actually move into protected mode. To make
; things as simple as possible, we do no register set-up or anything,
; we let the gnu-compiled 32-bit programs do that. We just jump to 
; absolute address 0x00000, in 32-bit protected mode.
        mov     ax, 0x0001      ; protected mode (PE) bit
        lmsw    ax              ; This is it!
        jmp     8:0             ; jmp offset 0 of segment 8 (cs)

; This routine checks that the keyboard command queue is empty
; No timeout is used - if this hangs there is something wrong with
; the machine, and we probabl couldn't proceed anyway.
; 只有当输入缓冲器为空时（键盘控制器状态寄存器位1 = 0）才可以对其执行写命令。
empty_8042:
        dw      0x00eb, 0x00eb
        in      al, 0x64        ; 8042 status port      ; 读AT键盘控制状态寄存器。
        test    al, 2           ; is input buffer full? ; 测试位1，输入缓冲器满？
        jnz     empty_8042      ; yes - loop
        ret

; Routine trying to recognize type of SVGA-board present (if any)
; and if it recognize one gives the choices of resolution it offers.
; If one is found the resolution chosen is given by al,ah (rows,cols).

chsvga: cld
        push    ds
        push    cs              ; 把默认数据段设置成和代码段同一个段。
        pop     ds
        mov     ax, 0xc000
        mov     es, ax          ; es 指向0xc000段。此处是VGA卡上的 ROM BIOS区。
        lea     si, [msg1]      ; ds:si指向msg1字符串。
        call    prtstr          ; 显示以NULL结尾的msg1字符串。
nokey:  in      al, 0x60        ; 读取键盘控制缓冲中的扫描码
        cmp     al, 0x82        ; 与最小断开码0x82比较。
        jb      nokey           ; 若小于0x82，表示还没有松按键松开。
        cmp     al, 0xe0
        ja      nokey           ; 若大于 0xe0，表示收到的是扩展扫描码前缀。
        cmp     al, 0x9c        ; 若断开码是0x9c表示用户按下/松开了回车键，
        je      svga            ; 于是程序跳转去检查系统是否具有SVGA模式。
        mov     ax, 0x019       ; 否则设置默认行列值 AL=25行、AH=80列。
        pop     ds
        ret

; 下面根据VGA显示卡上的 ROM BIOS 指定位置的特征数据串或者支持的特别功能来判断机器上
; 安装的是什么牌子的显示卡。本程序支持10种显示卡的扩展功能。
; 
; 首先判断是不是ATI显示卡。我们把ds:si指向ATI显示卡特征数据串，并把es:si指向 VGA BIOS
; 中指定位置（偏移0x31）处。该特征串共有9个字符（“761295520”），我们来循环比较这个特征
; 串。如果相同则表示机器中的VGA卡是ATI牌子的，于是让ds:si指向该显示卡可以设置的行列值dscati,
; 让di指向ATI卡可设置的行列个数和模式，并跳转到标号 selmod 处进一步进行设置。
svga:   lea     si, [idati]     ; Check ATI 'clues'
        mov     di, 0x31        ; 特征串从0xc000:0x0031 开始。
        mov     cx, 0x09        ; 特征串有9个字节。
        repe    cmpsb           ; 如果9个字节都相同，表示系统中有一块ATI牌显示卡。
        jne     noati           ; 若特征串不同则表示不是ATI显示卡。跳转继续检测卡。
; Ok, 我们现在确定了显示卡的牌子是ATI。于是si指向ATI显示卡可选行列值表dscati
; di指向扩展模式个数和扩展模式号列表moati，然后跳转到selmod处继续处理。
        lea     si, [dscati]    ; 把dscati的有效地址放入si。
        lea     di, [moati]
        lea     cx, [selmod]
        jmp     cx

; 现在来判断是不是Ahead牌子的显示卡。首先向 EGA/VGA 图形索引寄存器 0x3ec写入想访问的
; 主允许寄存器索引号 0x0f，同时向0x3cf端口（此时对应主允许寄存器）写入开启扩展寄存器
; 标志值 0x20。然后通过0x3cf端口读取主允许寄存器值，以检查是否可以设置开启扩展寄存器
; 标志。如果可以则说明是Ahead牌子的显示卡。注意word输出时 al->端口n，ah->端口n+1。
noati:  mov     ax, 0x200f      ; Check Ahead 'clues'
        mov     dx, 0x3ce       ; 数据端口指向主允许寄存器（0x0f->0x3ce端口）
        out     dx, ax          ; 并设置开启扩展寄存器标志（0x20->0x3cf端口）
        inc     dx              ; 然后再读取寄存器，检查该标志是否被设置上。
        in      al, dx
        cmp     al, 0x20        ; 如果读取值是0x20，则表示是Ahead A显示卡。
        je      isahed
        cmp     al, 0x21        ; 如果赢取值是0x21，则表示是Ahead B显示卡。
        jne     noahed          ; 否则说明不是 Ahead显示卡，于是跳转继续检测其余卡。
isahed: lea     si, [dscahead]
        lea     di, [moahead]
        lea     cx, selmod
        jmp     cx

; 现在来检查是不是 Chips & Tech 生产的显示卡。通过端口0x3c3（0x04或0x46e8）设置VGA允许
; 寄存器的进入设置模式标志（位4），然后从端口 0x104 读取显示卡芯片集标识值。如果该标识值
; 是0xA5，则说明是 Chips & Tech 生产的显示卡。
noahed: mov     dx, 0x3c3       ; Check Chips & Tech. 'clues'
        in      al, dx          ; 从 0x3c3 端口读取VGA允许寄存器值，
        or      al, 0x10        ; 添加上进入设置模式标志（位4）后再写回。
        out     dx, al
        mov     dx, 0x104       ; 在设置模式时从全局标识端口0x104读取显示卡芯片标识值，
        in      al, dx          ; 并暂时存放在bl寄存器中。
        mov     bl, al
        mov     dx, 0x3c3       ; 然后把0x303端口中的进入设置模式标志复位。
        in      al, dx
        and     al, 0xef
        out     dx, al
        cmp     bl, [idcandt]   ; 再把bl中标识值与位于idcandt处的Chips &
        jne     nocant          ; Tech的标识值 0xA5 作比较。如果不同则跳转比较下一种显卡。
        lea     si, [dsccandt]
        lea     di, [mocandt]
        lea     cx, [selmod]
        jmp     cx

; 现在检查是不是 Cirrus 显示卡。方法是使用CRT控制器索引号0x1f寄存器内容来尝试禁止扩展
; 功能。该寄存器被称为鹰标（Eagle ID）寄存器，将其值高低半字节交换一下后写入端口0x3c4索
; 引的6号（定序/扩展）寄存器应该会禁止Cirrus显示卡的扩展功能。如果不会则说明不是Cirrus
; 显示卡。因为从端口0x3d4索引的0x1f鹰标寄存器中读取内容是鹰标值与0x0c索引号对应的显
; 存起始地址高字节寄存器内容异或操作之后的值，因此在读0x1f中内容之前我们需要先把显存起始
; 高字节寄存器内容保存后清零，并在检查后恢复之。另外，将没有交换过的Eagle ID值写到0x3c4
; 端口索引的6号定序/扩展寄存器会重新开启扩展功能。
nocant: mov     dx, 0x3d4       ; Check Cirrus ‘clues’
        mov     al, 0x0c        ; 首先向CRT控制寄存器的索引寄存器端口0x3d4写入要访问
        out     dx, al          ; 寄存器索引号0x0c（对应显存起始地址高字节寄存器），
        inc     dx              ; 然后从0x3d5端口读入显存起始地址高字节并暂存在bl中，
        in      al, dx          ; 再把显存起始地址高字节寄存器清零。
        mov     bl, al
        xor     al, al
        out     dx,al
        dec     dx              ; 接着向0x3d4端口输出索引0x1f，指出我们要在0x3d5端口
        mov     al, 0x1f        ; 访问读取“Eagle ID”寄存器内容。
        out     dx, al
        inc     dx
        in      al, dx          ; 从0x3d5端口读取“Eagle ID”寄存器值，并暂存在bh中。
        mov     bh, al          ; 然后把该值高低4比特互换位置存放到cl中。再左移8位
        xor     ah, ah          ; 后放入ch中，而cl中放入数值6。
        shl     al, 4
        mov     cx, ax
        mov     al, bh
        shr     al, 4
        add     cx, ax
        shl     cx, 8
        add     cx, 6           ; 最后把cx值存放入ax中。此时ah中是换位后的“Eagle
        mov     ax, cx          ; ID”值，al中是索引号6，对应定序/扩展寄存器。把ah
        mov     dx, 0x3c4       ; 写到0x3c4端口索引的定序/扩展寄存器应该会导致Cirrus
        out     dx, ax          ; 显示卡禁止扩展功能。
        inc     dx
        in      al, dx          ; 如果扩展功能真的被禁止，那么此时读入的值应该为0。
        and     al, al          ; 如果不为0则表示不是Cirrus显示卡，跳转继续检查其他卡。
        jnz     nocirr
        call    rst3d4          ; 恢复CRT控制器的显示起始地址高字节寄存器内容。
        lea     si, [dsccirrus]
        lea     di, [mocirrus]
        lea     cx, [selmod]
        jmp     cx

; 该子程序利用保存在bl中的值恢复CRT控制器的显示起始地址高字节寄存器内容。
rst3d4: mov     dx, 0x3d4
        mov     al, bl
        xor     ah, ah
        shl     ax, 8
        add     ax, 0x0c
        out     dx, ax          ; 注意，这是word输出！！al->0x3d4, ah->0x3d5。
        ret

; 现在检查系统中是不是Everex显示卡。方法是利用中断 int 0x10 功能 0x70（ax = 0x7000，
; bx = 0x0000）调用Everex的扩展视频BIOS功能。对于Everes类型显示卡，该中断调用应该
; 会返回模拟状态，即有以下返回信息：
; al = 0x70, 若是基于Tredent的Everex显示卡；
; cl = 显示器类型： 00-单色； 01-CGA； 02-EGA； 03-数字多频； 04-PS/2； 05-IBM 8514； 06-SVGA。
; ch = 属性： 位7-6：00-256K， 01-512K， 10-1MB， 11-2MB； 位4-开启VGA保护； 位0-6845模拟。
; dx = 板卡型号：位15-4：板类型标识号； 位3-0：板修正标识号。
;      0x2360-Ultragraphics II; 0x6200-Vision VGA; 0x6730-EVGA; 0x6780-Viewpoint。
; di = 用BCD码表示的视频BIOS版本号。
nocirr: call    rst3d4          ; Check Everex 'clues'
        mov     ax, 0x7000      ; 设置ax = 0x7000， bx = 0x0000，调用 int 0x10。
        xor     bx, bx
        int     0x10
        cmp     al, 0x70        ; 对于Everes显示卡，al中应该返回值0x70。
        jne     noevrx
        shr     dx, 4           ; 忽略板修正号（位3-0）。
        cmp     dx, 0x678       ; 板类型号是0x678表示是一块Trident显示卡，则跳转。
        je      istrid
        cmp     dx, 0x236       ; 板类型号是0x236表示是一块Trident显示卡，则跳转。
        je      istrid
        lea     si, [dsceverex]
        lea     di, [moeverex]
        lea     cx, selmod
        jmp     cx
istrid: lea     cx, [ev2tri]    ; 是Trident类型的Everex显示卡，则跳转到ev2tri处理。
        jmp     cx

; 现在检查是不是Genoa显示卡。法式是检查其视频BIOS中的特征数字串（0x77、0x00、0x66、
; 0x99）。注意，此时es已经被设置成指向VGA卡上ROM BIOS所在的段0xc000。
noevrx: lea     si, [idgenoa]   ; Check Genoa 'clues'
                                ; 让ds:si指向特征数字串。
        xor     ax, ax          
        mov     al, [es:0x37]   ; 取VGA卡上BIOS中0x37处的指针（它指向特征串）。
        mov     di, ax          ; 因此此时es:di指向特征数字串开始处。
        mov     cx, 0x04
        dec     si
        dec     di
ll:     inc     si              ; 然后循环比较这4个字节的特征数字串。
        inc     di
        mov     al, [si]
        and     al, [es:di]
        cmp     al, [si]
        loope   ll
        cmp     cx, 0x00        ; 如果特征数字串完全相同，则表示 是Genoa显示卡，
        jne     nogen           ; 否则跳转去检查其他类型的显示卡。
        lea     si, [dscgenoa]
        lea     di, [mogenoa]
        lea     cx, [selmod]
        jmp     cx

; 现在检查是不是Paradise显示卡。同样采用比较显示卡上BIOS中特征串（“VGA=”）的方式。
nogen:  lea     si, [idparadise]; Check Paradise ‘clues’        
        mov     di, 0x7d        ; es:di指向 VGA ROM BIOS的0xc000:0x007d处，该处应该有
        mov     cx, 0x04        ; 4个字符 “VGA=”。
        repe    cmpsb
        jne     nopara
        lea     si, [dscparadise]
        lea     di, [moparadise]
        lea     cx, [selmod]
        jmp     cx

; 现在检查是不是Trident（TVGA）显示卡。TVGA显示卡扩充的模式控制寄存器1（0x3c4端口索引
; 的0x0e）的位3--0是64K内存页面个数值。这个字段值有一个特性：当写入时，我们需要首先把
; 值与0x02进行异或操作后再写入；当读取该值时则不需要执行异或操作，即异或前的值应该与
; 写入后再读取的值相同。下面代码就利用这个特性来检查是不是Trident显示卡。
nopara: mov     dx, 0x3c4       ; Check Trident 'clues'
        mov     al, 0x0e        ; 首先在端口0x3c4输出索引号0x0e，索引模式控制寄存器1。
        out     dx, al          ; 然后从0x3c5数据端口读入该寄存器原值，并暂存在ah中。
        inc     dx
        in      al, dx
        xchg    ah, al
        mov     al, 0x00        ; 然后我们向该寄存器写入0x00，再读取其值->al。
        out     dx, al          ; 写入0x00就相当于“原值”0x02异或0x02后的写入值，
        in      al, dx          ; 因此若是Trident显示卡，则此后读入的值应该是0x02。
        xchg    al, ah          ; 交换后， al=原模式控制寄存器1的值，ah=最后读取的值。
; 如果bl中原模式控制寄存器1的位1在置位状态的话就将其复位，否则就将位1置位。
; 实际上这几条语句就是对原模式控制寄存器1的值执行异或0x02的操作，然后用结果值去设置
; （恢复）原寄存器值。
        mov     bl, al
        and     bl, 0x02
        jz      setb2
        and     al, 0xfd
        jmp     clrb2
setb2:  or      al, 0x02
clrb2:  out     dx, al
        and     ah, 0x0f        ; 取最后读入值的页面个数字段（位3--0），如果
        cmp     ah, 0x02        ; 该字段值等于0x02，则表示是Trident显示卡。
        jne     notrid
ev2tri: lea     si, [dsctrident]
        lea     di, [motrident]
        lea     cx, [selmod]
        jmp     cx

; 现在检查是不是Tseng显示卡（ET4000AX或ET4000/W32类）。方法是对0x3cd端口对应的段
; 选择（Segment Select）寄存器执行读写操作。该寄存器高4位（位7--4）是要进行读操作的
; 64KB段号（Bank number）,低4位（位3--0）是指定要写的段号。如果指定段选择寄存器的
; 值是0x55（表示读、写第6个64KB段），那么对于Tseng显示卡来说，把该值写入寄存器后
; 再读出应该不是0x55。
notrid: mov     dx, 0x3cd       ; Check Tseng 'clues'
        in      al, dx          ; Could things be this simple ! :-)
        mov     bl, al          ; 先从0x3cd端口读取段选择寄存器原值，并保存在bl中。
        mov     al, 0x55        ; 然后我们向该寄存器写入0x55。再读入并放在ah中。
        out     dx, al
        in      al, dx
        mov     ah, al
        mov     al, bl          ; 接着恢复该寄存器的原值
        out     dx, al
        cmp     ah, 0x55        ; 如果读取的就是我们写入的值，则表明是Tseng显示卡。
        jne     notsen
        lea     si, [dsctseng]
        lea     di, [motseng]
        lea     cx, [selmod]
        jmp     cx

; 下面检查是不是Video7显示卡。端口0x3c2是混合输出寄存器写端口，而0x3cc是混合输出寄存
; 器读端口。该寄存器的位0是单色/彩色标志。如果为0则表示是单色，否则是彩色。判断是不是
; Video7显示卡的方式是利用这种显示卡的CRT控制扩展标识寄存器（索引号是0x1f）。该寄存器
; 的值实际上就是显存起始地址高字节寄存器（索引号0x0c）的内容和0xea进行进行异或操作后的值。
; 因此我们只要向显存起始地址高字节寄存器中写入一个特定值，然后从标识寄存器中读取标识值
; 进行判断即可。
; 通过以上显示卡和这里Video7显示卡的检查分析，我们可知检查过程通常分为三个基本步骤。
; 首先读取并保存测试需要用到的寄存器原值，然后使用特定测试值进行写入和读出操作，最后恢复
; 寄存器值并对检查结果作出判断。
notsen: mov     dx, 0x3cc       ; Check Video7 'clues'
        in      al, dx
        mov     dx, 0x3b4       ; 先设置dx为单色显示CRT控制索引寄存器端口号0x3b4。
        and     al, 0x01        ; 如果混合混合输出寄存器的位0等于0（单色）则直接跳转，
        jz      even7           ; 否则dx设置为彩色显示CRT控制索引寄存器端口号0x3d4。
        mov     dx, 0x3d4
even7:  mov     al, 0x0c        ; 设置寄存器索引号为0x0c，对应显存地址高字节寄存器。
        out     dx, al
        inc     dx
        in      al, dx          ; 读取显示内存起始地址高字节寄存器内容，并保存在bl中。
        mov     bl, al
        mov     al, 0x55        ; 然后在显存起始地址高字节寄存器中写入值0x55，再读取出来。
        out     dx, al
        in      al, dx
        dec     dx              ; 然后通过CRTC索引寄存器端口0x3b4或0x3d4选择索引号是
        mov     al, 0x1f        ; 0x1f的Video7显示卡标识寄存器。该寄存器内容实际上就是
        out     dx, al          ; 显存起始地址高字节和0xea进行异或操作后的结果值。
        inc     dx
        in      al, dx          ; 读取Video7显示卡寄存器值，并保存在bh中。
        mov     bh, al
        dec     dx              ; 然后再选择显存起始地址高字节寄存器，恢复其原值。
        mov     al, 0x0c        
        out     dx, al
        inc     dx
        mov     al, bl
        out     dx, al
        mov     al, 0x55        ; 随后我们来验证“Video7显示卡标识寄存器值就是显存起始
        xor     al, 0xea        ; 地址高字节和0xea进行异或操作后的结果值”。因此0x55
        cmp     al, bh          ; 和0xea进行异或操作的结果就就忘等于标高寄存器的测试值。
        jne     novid7
        lea     si, [dscvideo7]
        lea     di, [movideo7]

; 下面根据上述代码判断出的显示卡类型以及取得的相关扩展模式信息（si指向的行列值列表；di
; 指向扩展模式个数和模式号列表），提示用户选择可用的显示模式，并设置成相应显示模式。最后
; 子程序返回系统当前设置的屏幕行列值（ah = 列数； al = 行数）。例如，如果系统中是ATI显
; 示卡, 那么屏幕上会显示以下信息；
; Mode: COLSxROWS:
; 0.    132 x 25
; 1.    132 x 44
; Choose mode by pressing the corresponding number.
; 
; 这段程序首先在屏幕上显示NULL结尾的字符串信息“Mode: COLSxROWS:”。
selmod: push    si
        lea     si, [msg2]
        call    prtstr
        xor     cx, cx
        mov     cl, [di]        ; 此时cl中是检查出的显示卡的扩展模式个数。
        pop     si
        push    si
        push    cx
; 然后在每一行上显示出当前显示 卡可选择的扩展模式行列值，供用户选用。
tbl:    pop     bx              ; bx = 显示卡的扩展模式总个数。
        push    bx
        mov     al, bl
        sub     al, cl
        call    dprnt           ; 以十进制格式显示al中的值。
        call    spcing          ; 显示一个点再空4个空格。
        lodsw                   ; 在ax中加载si指向的行列值，随后si指向下一个word值。
        xchg    al, ah          ; 交换位置后 al = 列数。
        call    dprnt           ; 显示列数；
        xchg    ah, al          ; 此时al中是行数值。
        push    ax
        mov     al, 0x78        ; 显示一个小“x”，即乘号。
        call    prnt1 
        pop     ax              ; 此时al中是行数值。
        call    dprnt           ; 显示行数。
        call    docr            ; 回车换行。
        loop    tbl             ; 再显示下一个行列值。
; 在扩展模式行列值都显示之后，显示“Choose mode by pressing the corresponding number.”。
        pop     cx              ; cl中是显示卡扩展模式总个数值。
        call    docr
        lea     si, [msg3]
        call    prtstr

; 然后从键盘口读取用户按键的扫描码，根据该扫描码确定用户选择的行列值模式号，并利用ROM
; BIOS的显示中断int 0x10功能0x00来设置相应的显示模式。
; "模式个数值+0x80"是所按数字键-1的断开扫描码。对于0--9数字键，它们的断开
; 扫描码分别是：0 - 0x8B; 1 - 0x82; 2 - 0x83; 3 - 0x84; 4 - 0x85;
;             5 - 0x86; 6 - 0x87; 7 - 0x88; 8 - 0x89; 9 - 0x8A。
; 因此，如果读取的键盘断开扫描码小于0x82就表示不是数字键； 如果扫描码等于0x8B则表示用户
; 按下数字0键。
        pop     si              ; 弹出原行列值指针（指向显示卡行列值表开始处）。
        add     cl, 0x80        ; cl + 0x80 = 对应“数字键-1”的断开扫描码。
nonum:  in      al, 0x60        ; Quick and dirty...
        cmp     al, 0x82        ; 若键盘断开扫描码小于0x82则表示不是数字键，忽略该键。
        jb      nonum
        cmp     al, 0x8b        ; 若键盘断开扫描码等于0x8B，表示按下了数字键0。
        je      zero
        cmp     al, cl
        ja      nonum
        jmp     nozero

; 下面把断开扫描码转换成对应的数字按键值，然后利用该值从模式个数和模式号列表中选择对应的
; 模式号。接着调用机器ROM BIOS中断 int 0x10功能0把屏幕设置成模式号指定的模式。最后再
; 利用模式号从显示卡行列值表中选择并在ax返回对应的行列值。
zero:   sub     al, 0x0a        ; al = 0x8b - 0x8a = 0x81
nozero: sub     al, 0x80        ; 再送去0x80就可以得到用户选择了第几个模式。
        dec     al              ; 从0起计数。
        xor     ah, ah          ; int 0x10显示功能号=0（设置显示模式）。
        add     di, ax
        inc     di              ; di指向对应的模式号（路过第1个模式个数字节值） 。
        push    ax
        mov     al, [di]        ; 取模式号->al中， 并调用系统BIOS显示中断功能0.
        int     0x10
        pop     ax
        shl     ax, 1           ; 模式号乘2，转换成为行列值表中对应值的指针。
        add     si, ax
        lodsw                   ; 取对应行列值到ax中（ah = 列数， al = 行数）。
        pop     ds              ; 恢复保存的ds原值。在ax中返回当前显示行列值。
        ret

; 若都不是上面检测的显示卡，那么我们只好采用默认的80 x 25 的标准行列值。
novid7: pop     dx              ; Here could be code to support standard 80x50,80x30
        mov     ax, 0x5019
        ret

; Routine that 'tabs' to next col.
; 显示一个点字符‘.’和4个空格。
spcing: mov     al, 0x2e        ; 显示一个字符‘.’
        call    prnt1 
        mov     al, 0x20
        call    prnt1 
        mov     al, 0x20
        call    prnt1
        mov     al, 0x20
        call    prnt1 
        mov     al, 0x20
        call    prnt1 
        ret

; Routine to print asciiz-string at DS:SI
; 显示位于DS:SI处以NULL（0x00）结尾的字符串。

prtstr: lodsb
        and     al, al
        jz      fin
        call    prnt1           ; 显示al中的一个字符。
        jmp     prtstr
fin:    ret

; Routine to print a decimal value on screen, the value to be
; printed is put in al (i.e 0-255)
; 显示十进制数字的子程序。显示值放在寄存器al中（0--255）。

dprnt:  push    ax
        push    cx
        mov     ah, 0x00
        mov     cl, 0x0a
        idiv    cl
        cmp     al, 0x09
        jbe     lt100
        call    dprnt
        jmp     skip10
lt100:  add     al, 0x30
        call    prnt1
skip10: mov     al, ah
        add     al, 0x30
        call    prnt1
        pop     cx
        pop     ax
        ret

; Part of above routine, this one just prints ascii al
; 上面子程序的一部分。显示al中的一个字符。
; 该子程序使用中断0x10的0x0E功能，以电传方式在屏幕上写一个字符。光标会自动移动到下一个
; 位置处。如果写完一行光标就会移动到下一行开始处。如果已经写完一屏最后一行，则整个屏幕
; 会向上滚动一行。字符0x07（BEL）、0x08（BS）、0x0A（LF）和0x0D（CR）被作为命令不会显示。
; 输入：AL -- 欲写字符；BH -- 显示页号； BL -- 前景显示色（图形方式时）。

prnt1:  push    ax
        push    cx
        mov     bh, 0x00        ; 显示页面
        mov     cx, 0x01
        mov     ah, 0x0e
        int     0x10
        pop     cx
        pop     ax
        ret

; Prints <CR> + <LF>    ; 显示回车+换行。

docr:   push    ax
        push    cx
        mov     bh, 0x00
        mov     ah, 0x0e
        mov     al, 0x0a
        mov     cx, 0x01
        int     0x10
        mov     al, 0x0d
        int     0x10
        pop     cx
        pop     ax
        ret

; 全局描述表开始处。描述符表由多个8字节长的描述符项组成。这里给出了3个描述符项。
; 第1项无用，但须存在。第2项是系统代码描述符，第3项是系统数据描述符。
gdt:
        dw      0, 0, 0, 0      ; dummy         ; 第1个描述符，不用。

; 在GDT表中这里的偏移量是0x08。这是内核代码段选择符的值。
        dw      0x07FF          ; 8MB - limit=2047 (0--2047，因此是2048*4096=8MB)
        dw      0x0000          ; base address = 0
        dw      0x9A00          ; code read/exec        ; 代码段为只读、可执行。
        dw      0x00C0          ; granularity=4096, 286 ; 颗粒度为4096，32位模式。

; 在GDT表中这里的偏移量是0x10。它是内核数据段选择符的值。
        dw      0x07FF          ; 8MB - limit=2047 (2048*4096=8MB)
        dw      0x0000          ; base address=0
        dw      0x9200          ; data read/write       ; 数据段为可读可写。
        dw      0x00C0          ; granularity=4096, 386 ; 颗粒度为4096，32位模式。

; 下面是加载中断描述符表寄存器idtr的指令lidt要求的6字节操作数。前2字节是IDT表的
; 限长，后4字节是idt表的线性地址空间中的32位基地址。CPU要求在进入保护模式之前需设
; 置IDT表，因此这里设置一个长度为0的空表。
idt_48:
        dw      0               ; idt limit=0
        dw      0, 0            ; idt base=0L

; 这是加载全局描述符表寄存gdtr的指令lgdt要求的6字节操作数。前2字节是gdt表的限
; 长，后4字节是gdt表的线性基地址。这里全局表长度设置为2KB（0x7ff即可），因为每8
; 字节组成一个段描述符项，所以表中共可有256项。4字节的线性基地址为 0x0009<<16+
; 0x0200 + gdt, 即0x90200 + gdt。（符号gdt是全局表在本程序段中的偏移地址）
gdt_48:
        dw      0x800           ; gdt limit=2048, 256 GDT entries
        dw      512+gdt, 0x9    ; gdt base = 0x9xxxx

msg1:   db      "Press <RETURN> to see SVGA-modes available or any other key to continue."
        db      0x0d, 0x0a, 0x0a, 0x00
msg2:   db      "Mode: COLSxROWS:"
        db      0x0d, 0x0a, 0x0a, 0x00
msg3:   db      "Choose mode by pressing the corresponding number."
        db      0x0d, 0x0a, 0x00

; 下面是4个显示卡的特征数据串。
idati:          db      "761295520"
idcandt:        db      0xa5            ; 标号idcandt意思是 ID of Chip AND Tech.
idgenoa:        db      0x77, 0x00, 0x66, 0x99
idparadise:     db      "VGA="

; 下面是各种显示卡可使用的扩展模式个数和对应的模式号列表。其中每一行第1个字节是模式个
; 数值，随后的些值是中断0x10功能0（AH=0）可使用的模式号。例如，对于ATI牌子的显示卡，
; 除了标准模式以外还可使用两种扩展模式：0x23和0x33。
; Manufacturer:         Numbermodes:    Mode:
; 厂家：                 模式数量：        模式列表：

moati:          db      0x02,           0x23, 0x33
moahead:        db      0x05,           0x22, 0x23, 0x24, 0x2f, 0x34
mocandt:        db      0x02,           0x60, 0x61
mocirrus:       db      0x04,           0x1f, 0x20, 0x22, 0x31
moeverex:       db      0x0a,           0x03, 0x04, 0x07, 0x08, 0x0a, 0x0b, 0x16, 0x18, 0x21, 0x40
mogenoa:        db      0x0a,           0x58, 0x5a, 0x60, 0x61, 0x62, 0x63, 0x64, 0x72, 0x74, 0x78
moparadise:     db      0x02,           0x55, 0x54
motrident:      db      0x07,           0x50, 0x51, 0x52, 0x57, 0x58, 0x59, 0x5a
motseng:        db      0x05,           0x26, 0x2a, 0x23, 0x24, 0x22
movideo7:       db      0x06,           0x40, 0x43, 0x44, 0x41, 0x42, 0x45

; 下面是各种牌子VGA显示卡可使用的模式对应的列、行值列表。例如ATI显示卡两种扩展模式的
; 列、行值分别是 132 x 25、 132 x 44。
;               msb = Cols      lsb = Rows:
;               高字节 = 列数     低字节 = 行数：

dscati:         dw      0x8419, 0x842c
dscahead:       dw      0x842c, 0x8419, 0x841c, 0xa032, 0x5042
dsccandt:       dw      0x8419, 0x8432
dsccirrus:      dw      0x8419, 0x842c, 0x841e, 0x6425
dsceverex:      dw      0x5022, 0x503c, 0x642b, 0x644b, 0x8419, 0x842c, 0x501e, 0x641b, 0xa040, 0x841e
dscgenoa:       dw      0x5020, 0x642a, 0x8419, 0x841d, 0x8420, 0x842c, 0x843c, 0x503c, 0x5042, 0x644b
dscparadise:    dw      0x8419, 0x842b
dsctrident:     dw      0x501e, 0x502b, 0x503c, 0x8419, 0x841e, 0x842b, 0x843c
dsctseng:       dw      0x503c, 0x6428, 0x8419, 0x841c, 0x842c
dscvideo7:      dw      0x502b, 0x503c, 0x643c, 0x8419, 0x842c, 0x841c
```

boot/head.nasm
-----------

```nasm
; 
; head.s contains the 32-bit startup code.
; 
; NOTE!!! Startup happens at absolute address 0x00000000, which is also where
; the page directory will exist. The startup code will overwritten by
; the page directory. 

section .text
; [global _idt, _gdt, _pg_dir, _tmp_floppy_area]
extern stack_start, _start, printk
global _idt, _gdt, _pg_dir, _tmp_floppy_area, startup_32
_pg_dir:                                        ; 页目录将会存放在这里。

; 再次注意！！这里已经处于32位运行模式，因此这里$0x10现在是一个选择符。这里的移动指令
; 会把相应描述符内容加载进段寄存器。这里$0x10的含义是：
; 请求特权级为0（位0-1=0）、选择全局描述符表（位2=0）、选择表中第2项（位3-15=2）。它正好
; 指向表中的数据段描述符项。
; 下面代码的含义是：设置ds，es，fs，gs为setup.s中构造的内核数据段选择符=0x10（对应全局
; 段描述符表第3项），并将堆栈放置在stack_start指向的user_stack数组区，然后使用本程序
; 后面定义的新中断描述符表和全局段描述表。新全局段描述表中初始
; 内容与setup.s中基本一样，仅段限长从8MB改成了16MB。stack_start定义在kernel/sched.c。
; 它是指向user_stack数组末端的一个长指针。第23行设置这里使用的栈，姑且称为系统栈。但在移动
; 到任务0执行（init/main.c中137行）以后该栈就被用作任务0和任务1共同使用的用户栈了。

startup_32:
        mov     eax, 0x10
        mov     ds, ax
        mov     es, ax
        mov     fs, ax
        mov     gs, ax
        lss     esp, [stack_start]              ; 表示_stack_start->ss:esp,设置系统堆栈
                                                ; stack_start定义在kernel/sched.c,82--87行。
        call    setup_idt                       ; 调用设置中断描述符表子程序
        call    setup_gdt                       ; 调用设置全局描述符表子程序
        mov     eax, 0x10                       ; reload all the segment registers
        mov     ds, ax                          ; after changing gdt. CS was already
        mov     es, ax                          ; reloaded in 'setup_gdt'
        mov     fs, ax
        mov     gs, ax

; 由于段描述符中的段限长从setup.s中的8MB改成了本程序设置的16MB(见setup.s 567-568行
; 和本程序后面的235-236行)，因此这里必须再次对所有寄存器执行重加载操作。另外，通过
; 使用bochs仿真软件跟踪观察，如果不对CS再次执行加载，那么在执行到26行时CS代码段不可
; 见部分中的限长不是8MB。这样看来应该重新加载CS。但是由于setup.s中的内核代码段描述符
; 与本程序中重新设置的代码段描述符仅是段限长不同，其余部分完全一样。所以8MB的段限长在
; 内核初始化阶段不会有问题。另外，在以后内核执行过程中段间跳转指令会重新加载CS，所以这
; 里没有加载px并不会导致以后内核出错。
; 针对该问题，目前内核中就在第25行之后添加了一条长跳转指令：‘ljmp $(__KERNEL_CS), $1f’，
; 跳转到第26行来确保CS确实又被重新加载。

        lss     esp, [stack_start]

; 32-36行用于测试A20地址线是否已经开启。采用的方法是向内存地址 0x00000000处写入任意
; 一个数值，然后看内存地址0x100000（1M）处是否也是这个数值。如果一直相同的话，就一直
; 比较下去，也即死循环、死机。表示地址A20线没有选通，结果内核就不能使用1MB以上内存。
; 
; 33行上的‘1：’是一个局部符号构成的标号。标号由符号后跟一个冒号组成。此时该符号表示活动
; 位置计数（Active location counter）的当前值，并可以作为指令的操作数。局部符号用于帮助
; 编译器和编程人员临时起用一些名称。共有10个局部符号，可在整个程序中重复使用。这些符号
; 名使用名称‘0’、‘1’、...、‘9’来引用。为了定义一个局部符号，需把标号写成‘N：’形式（其中N
; 表示一个数字）。为了引用先前最近定义的这个符号，需要写成‘Nb’。为了引用一个局部标号的
; 下一个定义，需要写成‘Nf’，这里N是10个前向引用之一。上面‘b’表示“向后（backwards）”，
; ‘f’表示“向前（forwards）”。在汇编程序的某一处，我们最大可以向后/向前引用10个标号
; （最远第10个）。

        xor     eax, eax
.1:     inc     eax
        mov     [0x000000], eax         ; check that A20 really IS enabled
        cmp     [0x100000], eax         ; loop forever if it isn't
        je      .1

; NOTE!486 should set bit 16, to check for write-protect in supervisor
; mode. Then it would be unnecessary with the "verify_area()"-calls.
; 486 users probably want to set the NE (#5) bit also, so as to use
; int 16 for math errors.
; 
; 上面原注释中提到的 486 CPU 中 CR0 控制寄存器的位16是写保护标志WP(Write-Protect),
; 用于禁止超级用户级的程序向一般用户只读页面中进行写操作。该标志主要用于操作系统在创建
; 新进程时实现写时复制（copy-on-write）方法。
; 下面这段程序（43-65）用于检查数数协处理器芯片是否存在。方法是修改控制寄存器CR0，在
; 假设存在协处理器的情况下执行一个协处理器指令，如果出错的话则说明协处理器芯片不存在，
; 需要设置CR0中的协处理器仿真位EM（位2），并复位协处理器存在标志位MP（位1）。
        
        mov     eax, cr0                ; check math chip
        and     eax, 0x80000011         ; Save PG,PE,ET
; "orl $0x10020, %eax" here for 486 might be good
        or      eax, 2                  ; set MP
        mov     cr0, eax
        call    check_x87
        jmp     after_page_tables       ; 跳转到135行

; We depend on ET to be correct. This checks for 287/387.
; 我们依赖于ET标志的正确性来检测287/387存在与否。

; 下面fninit和fstsw是数学协处理器（80287/80387）的指令。
; fninit 向协处理器发出初始化命令，它会把协处理器置于一个未受以前操作影响的已知状态，设置
; 其控制字为默认值、清除状态字和所有浮点栈式寄存器。非等待形式的这条指令（fninit）还会让
; 协处理器终止执行当前正在执行的任何先前的算术操作。fstsw指令取协处理器的状态字。如果系
; 统中存在协处理器的话，那么在执行了fninit指令后其状态字低字节肯定为0。

check_x87:
        fninit                          ; 向协处理器发出初始化命令。
        fstsw   ax                      ; 取协处理器状态字到ax寄存器中。
        cmp     al, 0                   ; 初始化后状态字应该为0，否则说明协处理器不存在。
        je      .1                      ; no coprocessor: have to set bits
        mov     eax, cr0                ; 如果存在则向前跳转到标号1处，否则改写cr0。
        xor     eax, 6                  ; reset MP, set EM
        mov     cr0, eax
        ret

; .align 是一汇编指示符。其含义是指存储边界对齐调整。“2”表示把随后的代码或数据的偏移位置
; 调整到地址值最后2比特位为零的位置（2^2），即按4字节方式对齐内存地址。不过现在 GNU as
; 直接是写出对齐的值而非2的次方值了。使用该指示符的目的是为了提高32位CPU访问内存中代码
; 或数据的速度和效率。
; 下面的两字节值是80287协处理器指令fsetpm的机器码。其作用是把80287设置为保护模式。
; 80387无需该指令，并且会把该指令看作是空操作。

; .lignn 2
align 4
; 1:    .byte 0xDB, 0xE4                ; fsetpm for 287, ignored by 387
.1:     fsetpm                          ; 287 协处理器码
        ret

;
; setup_idt
;
; set up a idt with 256 entries pointing to 
; ignore_int, interrupt gates. It then loads
; idt. Everything that wants to install itself
; in the idt-table may do so themselves. Interrupts
; are enabled elsewhere, when we can be relatively
; sure everything is ok. This routine will be over-
; written by the page tables.

; 中断描述符表中的项虽然也是8字节组成，但其格式与全局表中的不同，被称为门描述符
; (Gate Descriptor)。它的0-1，6-7字节是偏移量，2-3字节是选择符，4-5字节是一些标志。
; 这段代码首先在 edx、eax中组合设置出8字节默认的中断描述符值，然后在idt表每一项中
; 都放置该描述符，共256项。eax含有描述符低4字节，edx含有高4字节。内核在随后的初始
; 化过程中会替换安装那些真正实用的中断描述符项。

setup_idt:
        lea     edx, [ignore_int]       ; 将ignore_int的有效地址（偏移值）值 -> edx寄存器
        mov     eax, 0x00080000         ; 将选择符0x0008置入eax的高16位中。
        mov     dx, ax                  ; selector = 0x0008 = cs
                                        ; 偏移值的低16位置入eax的低16位中。此时eax含有
                                        ; 门描述符低4字节的值。
        mov     dx, 0x8E00              ; interrupt gate - dpl=0,present
                                        ; 此时edx含有门描述符高4字节的值。
        lea     edi, [_idt]             ; _idt是中断描述符的地址。
        mov     ecx, 256
rp_sidt:
        mov     [edi], eax              ; 将哑中断门描述符存入表中。
        mov     [edi+4], edx            ; eax内容放到 edi+4 所指向内存位置处。
        add     edi, 8                  ; edi指向表中下一项。
        dec     ecx
        jne     rp_sidt
        lidt    [idt_descr]             ; 加载中断描述符表寄存器值。
        ret

; 
; setup_gdt
;
; This routines sets up a new gdt and loads it.
; Only two entries are currently buit, the same
; ones that were build in init.s. The routine
; is VERY complicated at two whole lines, so this
; rather long comment is certainly needed :-).
; This routine will beoverwritten by the page tables.
setup_gdt:
        lgdt    [gdt_descr]             ; 加载全局描述符表寄存器
        ret

; I put the kernel page tables right after the page directory,
; using 4 of them to span 16 Mb of physical memory. People with
; more than 16MB will havt to expand this.
; Linus将内核的内存表直接放在页目录之后，使用了4个表来寻址16MB的物理内存。
; 如果你有多玩16MB的内存，就需要在这里进行扩充修改。

; 每个页表长度为4KB（一页内存页面），而每个页表项需要4个字节，因此一个页表共可以存放
; 1024个表项。如果一个页表项寻址4KB的地址空间，则一个页表就可以寻址4MB的物理内存。
; 页表项的格式为：项的前0-11位存放一些标志，例如是否在内存中（P位0）、读写许可（R/W位1）、
; 普通用户不是超级用户使用（U/S位2）、是否修改过（是否脏了）（D位6）等；表项的位12-31是
; 页框地址，用于指出一页内存的物理起始地址。

times 0x1000-($-$$) db 0
pg0:

times 0x2000-($-$$) db 0
pg1:

times 0x3000-($-$$) db 0
pg2:

times 0x4000-($-$$) db 0
pg3:

times 0x5000-($-$$) db 0        ; 定义下面的内存数据块从0x5000处开始
;
; tmp_floppy_area is used by the ploppy-driver when DMA cannot
; reach to a buffer-block. It needs to be aligned, so that it isn't
; on a 64KB border.
; 当DMA（直接存储器访问）不能访问缓冲块时，下面的tmp_floppy_area内存块
; 就可以供轮船驱动程序使用。其地址需要对齐调整，这样就不会跨越64KB边界。
_tmp_floppy_area:
        times 1024 db 0         ; 共保留1024项，每项1字节，填充数值0.

; 下面这几个入栈操作用于为跳转到init/main.c中的 main() 函数作准备工作。第139行上的指令
; 在栈中压入了返回地址（标号L6），而第140行则压入了main()函数代码的地址。当head.s最后
; 在第218行执行ret指令时就会弹出main()函数的地址，并把控制权转移到 init/main.c程序中。
; 参见第3章中有关C函数调用机制的说明。
; 前面3个入栈0值分别表示main函数的参数envp、argv指针和argc，但main()没有用到。
; 139行的入栈操作是模拟调用main程序时将返回地址入栈的操作，操心如果main.c程序
; 真的退出时，就会返回到这里的标号L6外继续执行下去，也即死循环。140行将main.c的地址
; 压入堆栈，这样，在设置分布处理（setup_paging）结束后执行‘ret’返回指令时就会将main.c
; 程序的地址弹出堆栈，并去执行main.c程序了。
after_page_tables:
        push    0                       ; These are the parameters to main :-)
        push    0                       ; 这些是调用main程序的参数（指init/main.c）。
        push    0                       ; 其中的‘$’符号表示这是一个立即操作数。
        push    L6                      ; return address for main, if it decides to.
        push    _start                  ; '_main'是编译程序时对main的内部表示方法。
        jmp     setup_paging
L6:
        jmp     L6                      ; main should never return here, but
                                        ; just in case, we know what happens.

; This is the default interrupt "handler" :-)
int_msg:
        db "Unknown interrupt", 0x0a, 0x0d

align 4                         ; .align 2
ignore_int:     
        push    eax
        push    ecx
        push    edx
        push    ds                      ; 这里请注意！！ds，es，fs，gs等虽然是16位的寄存器，但入栈后
        push    es                      ; 仍然会以32位的形式入栈，也即需要占用4个字节的堆栈空间。
        push    fs                      
        mov     eax, 0x10               ; 置段选择符（使ds，es，fs指向gdt表中的数据段）。
        mov     ds, ax
        mov     es, ax
        mov     fs, ax
        push    int_msg                 ; 把调用printk函数的参数指针（地址）入栈。注意！若int_msg
        call    printk                  ; 前不加‘$’,则表示把int_msg符号处的长字（‘Unkn’）入栈。
        pop     eax                     ; 该函数在/kernel/printk.c中。‘_printk’是printk编译后模块中
        pop     fs                      ; 内部表示法
        pop     es
        pop     ds
        pop     edx
        pop     ecx
        pop     eax
        iret                            ; 中断返回（把中断调用时压入栈的CPU标志寄存器（32位）值也弹出）。


; 
; Setup_paging
; 
; This routine sets up paging by setting the page bit
; in cr0. The page tables are set up, identity-mapping
; the first 16MB. The pager assumes that no illegal
; addresses are produced (ie >4Mb on a 4Mb machine).
;
; NOTE! Although all physical memory should be identity
; mapped by this routine, only the kernel page functions
; use the >1Mb addresses directly. All "normal" functions
; use just the lower 1Mb, or the local data space, which
; will be mapped to some other place - mm keeps track of
; that.
; for those with more memory than 16 Mb - tough luck. I've
; not got it, why sould you :-) The source is here. Change
; it. (Seriously - it shouldn't be too difficult. Mostly
; change some constants etc. I left it at 16Mb, as my machne
; even cannot be extended past that (ok, but it was cheap :-)
; I've tried to show which constants to change by having
; some kind of marker at them (search for "16Mb"), but I
; won't guarantee that's all :-( )
; 上面英文注释第2段的含义是指在机器物理内存中大于 1MB的内存空间要被用于主内存区。
; 主内存空间mhmm模块管理。它涉及到页面映射操作。内核中所有其他函数就是这里指的一般
; (普通)函数。若要使用主内存区的页面，就需要使用get_free_page()等函数获取。因为主内
; 存区中内存页面是共享资源，必须有程序进行统一管理以避免资源争用和竞争。
; 
; 在内存物理地址0x0处开始存放1页页目录表和4页页表。页目录是系统所有进程公用的，而
; 这里的4页页表则属于内核专用，它们一一映射线性地址起始16MB空间范围到物理内存上。对于
; 新建的进程，系统会在主内存区为其申请页面存放页表。另外，1页内存长度是4096字节。

align 4                                         ; 按4字节方式对齐内存地址边界。
setup_paging:                                   ; 首先对5页内存（1页目录 + 4页页表）清零
        mov     ecx, 1024*5                     ; 5 pages - pg_dir+4 page tables
        xor     eax, eax
        xor     edi, edi                        ; pg_dir is at 0x000
        cld                                     ; 页目录从0x000地址开始。
        rep stosd                               ; eax内容存到es:edi所指内存位置处，且edi增4。

; 下面4句设置面目录表中的项。因为我们（内核）共有4个页表，所以只需设置4项。
; 页目录项的结构与页表中项的结构一样，4个字节为1项。参见上面113行下的说明。
; 例如“$pg0+7”表示：0x00001007,是页目录表中的第1项。
; 则第1个页表所在的地址 = 0x00001007 & 0xfffff000 = 0x1000;
; 第1个页表的属性标志 = 0x00001007 & 0x00000fff = 0x07, 表示该页存在、用户可读写。
        mov     dword [_pg_dir], pg0+7          ; set present bit/user r/w
        mov     dword [_pg_dir+4], pg1+7
        mov     dword [_pg_dir+8], pg2+7
        mov     dword [_pg_dir+12], pg3+7

; 下面6行填写4个页表中所有项的内容，共有：4（页表）*1024（项/页表）=4096项（0-0xfff），
; 也即能映射物理内存 4096*4KB = 16MB。
; 每项的内容是：当前项所映射的物理内存地址 + 该页的标志（这里均为7）。
; 填写使用的方法是从最后一个页表的最后一项开始按倒退顺序填写。每一个页表中最后一项在表中
; 的位置是 1023*4 = 4092。因此最后一页的最后一项的位置就是$pg3+4092。

        mov     edi, pg3+4092                   ; edi->最后一页的最后一项
        mov     eax, 0xfff007                   ; 16Mb - 4096 + 7 (r/w user,p)  
                                                ; 最后1项对应物理内存页面的地址是0xfff000
                                                ; 加上属性标志7，即为0xfff007
        std                                     ; 方向位置位，edi值递减（4个字节）。
.1:     stosd                                   ; fill pages backwards- more efficient :-)
        sub     eax, 0x1000                     ; 每填写好一项，物理地址值减0x1000。
        jge     .1                              ; 如果小于0则说明全填写好了。
; 现在设置页目录表基址寄存器cr3，指向页目录表。cr3中保存的是页目录表的物理地址，然后
; 再设置启动使用分页处理（cr0的PG标志，位31）
        xor     eax, eax                        ; pg_dir is at 0x0000   ; 页目录表在0x000处
        mov     cr3, eax                        ; cr3 - page directory start
        mov     eax, cr0
        or      eax, 0x80000000                 ; 添上PG标志
        mov     cr0, eax                        ; set paging (PG) bit
        ret                                     ; this also flushes prefetch-queue

; 在改变分页处理标志后再使用转移指令刷新刷新预取指令队列。这里用的是返回指令ret。
; 该返回指令的另一个作用是将140行压入堆栈中的main程序的地址弹出，并跳转到/init/main.c
; 程序去运行。本程序到此就真正结束了。

align 4                                         ; 按4字节对齐内存地址边界。
dw 0                                            ; 这里先空出2字节，这样224行上的长字是4字节对齐的

; 下面是加载中断描述符表寄存器idtr的指令lidt要求的6字节操作数。前2字节是idt表的限长，
; 后4字节是idt表在线性地址空间中的32位基地址。
idt_descr:
        dw      256*8-1                         ; idt contains 256 entires ; 共256项，限长=长度-1。
        dd      _idt
align 4
dw 0

; 下面加载全局描述符表寄存器gddtr的指令lgdt要求的6字节操作数。前2字节是gdt表的限长，
; 后4字节是gdt表的线性基地址。这里全局表长度设置为2KB字节（0x7ff即可），因为每8字节
; 组成一个描述符项，所以表中共可有256项。符号_gdt是全局表在本程序中的偏移位置，见234行。
gdt_descr:
        dw      256*8-1                         ; so does gdt (note that that's any
        dd      _gdt                            ; magic number, but it works for me :^)

align 8                                         ; 按8（2^3）字节方式对齐内存地址边界。
_idt:   times 256 dq 0                          ; idt is uninitialized  ; 256项，每项8字节，填0.

; 全局描述符表。其前4项分别是：空项（不用）、代码段描述符、数据段描述符、系统调用段描述符，
; 其中系统调用段描述符并没有派用处，Linus当时可能曾想把系统调用代码放在这个独立段中。
; 后面还预留了252项的空间，用于放置亲创建任务的局部描述符（LDT）和对应的任务段TSS
; 的描述符。
; (0-nul, 1-cs, 2-ds, 3-syscall, 4-TSS0, 5-LDT0, 6-TSS1, 7LDT1, 8-TSS2 etc...)

_gdt:   dq      0x0000000000000000              ; NULL descriptor
        dq      0x00c09a0000000fff              ; 16Mb          ; 0x08,内核代码段最大长度16MB。
        dq      0x00c0920000000fff              ; 16Mb          ; 0x10,内核数据段最大长度16MB。
        dq      0x0000000000000000              ; TEMPORARY - don't use
        times 252 dq 0                          ; space for LDT's and TSS's etc ; 预留空间
```

init/main.c
-----------
boot/head.s最后会进入init/main.c中的main函数运行,其中还引用了c代码中的stack_start,printk。为了通过编译进行测试，我们在init/main.c中定义main函数，printk函数和变量stack_start。链接时head.s的代码应该在输出文件开始的地方，但是现在的链接器会把main函数的代码放在最前面。一番尝试后发现只要将main()改个名字就没问题了，所以将main()改成了_start()。

```c
void printk()
{

}

void _start(void)
{
	int a = 0x55aa;
	int b = 0xaa55;

	while(1)
	{
		a = a^b;
		b = a^b;
		a = a^b;
	}
}


#define PAGE_SIZE 4096

long user_stack [ PAGE_SIZE>>2 ] ;

struct {
	long * a;
	short b;
} stack_start = { & user_stack [PAGE_SIZE>>2] , 0x10 };

```

编译&运行
-------

为了在Mac中也能编译和链接，需要安装x86_64-elf-binutils和x86_64-elf-gcc
```sh
brew install x86_64-elf-binutils
brew install x86_64-elf-gcc
```

Makefile编译链接制作启动软盘：
```make
AS	=nasm
LD	=x86_64-elf-ld
LDFLAGS	=-m elf_i386 -Ttext 0 -e startup_32
CC	=x86_64-elf-gcc -m32 -march=i386
CFLAGS	=-Wall -fomit-frame-pointer

build-system:
	nasm -f elf32 -o head.o linux-0.12/head.nasm
	$(CC) $(CFLAGS) -nostdinc -Iinclude -Wno-main -c -o main.o linux-0.12/init/main.c
	$(LD) $(LDFLAGS) head.o main.o  -o system
	x86_64-elf-objcopy -O binary -R .note -R .comment system kernel
	nasm linux-0.12/bootsect.nasm -o bootsect.bin
	nasm linux-0.12/setup.nasm -o setup.bin
	dd if=bootsect.bin of=Image bs=512 count=1 conv=notrunc
	dd if=setup.bin of=Image bs=512 seek=1 conv=notrunc
	dd if=kernel of=Image bs=512 seek=5 conv=notrunc
	bochs -q -unlock
```

使用`x86_64-elf-objdump -d system`可以反汇编查看system的机器码：
```
system:     file format elf32-i386


Disassembly of section .text:

00000000 <_pg_dir>:
       0:	b8 10 00 00 00       	mov    $0x10,%eax
       5:	8e d8                	mov    %eax,%ds
       7:	8e c0                	mov    %eax,%es
       9:	8e e0                	mov    %eax,%fs
       b:	8e e8                	mov    %eax,%gs
       d:	0f b2 25 e8 74 00 00 	lss    0x74e8,%esp
      14:	e8 5a 00 00 00       	call   73 <setup_idt>
      19:	e8 85 00 00 00       	call   a3 <setup_gdt>
      1e:	b8 10 00 00 00       	mov    $0x10,%eax
      23:	8e d8                	mov    %eax,%ds
      25:	8e c0                	mov    %eax,%es
      27:	8e e0                	mov    %eax,%fs
      29:	8e e8                	mov    %eax,%gs
      2b:	0f b2 25 e8 74 00 00 	lss    0x74e8,%esp
      32:	31 c0                	xor    %eax,%eax

00000034 <startup_32.1>:
      34:	40                   	inc    %eax
      35:	a3 00 00 00 00       	mov    %eax,0x0
      3a:	39 05 00 00 10 00    	cmp    %eax,0x100000
      40:	74 f2                	je     34 <startup_32.1>
      ......
```