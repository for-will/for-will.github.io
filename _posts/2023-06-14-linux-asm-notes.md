---
layout: post
title: "Linux-0.12内核中的汇编笔记"
subtitle: ""
date: 2023-06-14
author: "will"
tags: [linux, asm, nasm]
---

LDS, LES, LFS, LGS, LSS: Load Far Pointer
-----------------------------------------
```nasm
LDS reg16,mem                 ; o16 C5 /r            [8086] 
LDS reg32,mem                 ; o32 C5 /r            [8086]
LES reg16,mem                 ; o16 C4 /r            [8086] 
LES reg32,mem                 ; o32 C4 /r            [8086]
LFS reg16,mem                 ; o16 0F B4 /r         [386] 
LFS reg32,mem                 ; o32 0F B4 /r         [386]
LGS reg16,mem                 ; o16 0F B5 /r         [386] 
LGS reg32,mem                 ; o32 0F B5 /r         [386]
LSS reg16,mem                 ; o16 0F B2 /r         [386] 
LSS reg32,mem                 ; o32 0F B2 /r         [386]
```

These instructions load an entire far pointer (16 or 32 bits of offset, plus 16 bits of segment) out of memory in one go. LDS, for example, loads 16 or 32 bits from the given memory address into the given register (depending on the size of the register), then loads the next 16 bits from memory into DS. LES, LFS, LGS and LSS work in the same way but use the other segment registers.

**代码：**
```nasm
;; NASM
push 0
pop fs
mov bx, 0x78
lgs si, [fs:bx]     ; 从内存0x00078处读取2字节数据到si，之后的2字节数据到gs
```
对应内核中86as的代码: `linux/boot/bootsect.S:87`
```nasm
push	#0
pop	fs
mov	bx,#0x78		! fs:bx is parameter table address
seg fs
lgs	si,(bx)			! gs:si is source
```

BIOS 中断 0x1E
------------
BIOS 设置的中断 0x1E 实际上并不是一个中断，其对应中断向量的地方被放置了软驱参数表的地址。该中断向量位于内存 0x1E * 4 = 0x78 处。
```as86
push	#0
pop	fs
mov	bx,#0x78		! fs:bx is parameter table address
seg fs
lgs	si,(bx)			! gs:si is source

mov	di,dx			! es:di is destination
mov	cx,#6			! copy 12 bytes
cld

rep
seg gs
movw

mov	di,dx
movb	4(di),*18		! patch sector count
```
这段代码首先从内存 0x0000:0x0078 处复制原软驱参数表到 0x9000:0xfef4 处，然后修改表中偏移 4 处的每磁道最大扇区数为 18。表长 12 字节。

**NASM代码**
```nasm
push 0
pop fs
mov bx, 0x78                    ; fs:bx is parameter table address

lgs si, [fs:bx]                 ; gs:si is source

mov di, dx                      ; es:di is destination ; dx=0xfef4
mov cx, 6                       ; copy 12 bytes
cld

rep gs movsw                    ; copy word from [gs:si] to [es:di]

mov di, dx
mov byte [di+4], 18
```

`movsw`默认从内存地址`[ds:si]`处复制两个字节数据到内存地址`[es:di]`处,如果要从内存`[gs:si]`处复制数据到`[es:di]`,则加上前缀`gs`。`rep gs movsw`表示从内存`[gs:si]`处复制`cx*2`字节数据到内存`[es:di]`处。参考[The Netwide Assembler: NASM](!http://www.cburch.com/csbsju/cs/350/docs/nasm/nasmdoca.html#section-A.105)

**调试**：`gs=0xf000`,`si=0xefde`,用`xp /12hx 0xfefde`查看`gs:si`处的内存，将数据复制到`0x7c0:0xfef4`处，其实就是栈的上面，用`print-stack`命令就可以查看，或者用命令`xp /12bx 0x17af4`。可以看到`0x17af8`处的值已经是0x12,即18。

```
<bochs:29> xp /12hx 0xfefde 
[bochs]:
0x00000000000fefde <bogus+       0>:	0x02af	0x0225	0x1b12	0x6cff	0x0ff6	0x4f08	0x0400	0x0000
0x00000000000fefee <bogus+      16>:	0x0000	0x0000	0x0000	0x0000
<bochs:30> print-stack
Stack address size 2
 | STACK 0x17af4 [0x02af] (<unknown>)
 | STACK 0x17af6 [0x0225] (<unknown>)
 | STACK 0x17af8 [0x1b12] (<unknown>)
 | STACK 0x17afa [0x6cff] (<unknown>)
 | STACK 0x17afc [0x0ff6] (<unknown>)
 | STACK 0x17afe [0x4f08] (<unknown>)
 | STACK 0x17b00 [0x0000] (<unknown>)
 | STACK 0x17b02 [0x0000] (<unknown>)
 | STACK 0x17b04 [0x0000] (<unknown>)
 | STACK 0x17b06 [0x0000] (<unknown>)
 | STACK 0x17b08 [0x0000] (<unknown>)
 | STACK 0x17b0a [0x0000] (<unknown>)
 | STACK 0x17b0c [0x0000] (<unknown>)
 | STACK 0x17b0e [0x0000] (<unknown>)
 | STACK 0x17b10 [0x0000] (<unknown>)
 | STACK 0x17b12 [0x0000] (<unknown>)
```

文档
----
1. [The Netwide Assembler: NASM -- Section A.92: LDS, LES, LFS, LGS, LSS: Load Far Pointer](http://www.cburch.com/csbsju/cs/350/docs/nasm/nasmdoca.html#section-A.92)