;
;	Command & Conquer Red Alert(tm)
;	Copyright 2025 Electronic Arts Inc.
;
;	This program is free software: you can redistribute it and/or modify
;	it under the terms of the GNU General Public License as published by
;	the Free Software Foundation, either version 3 of the License, or
;	(at your option) any later version.
;
;	This program is distributed in the hope that it will be useful,
;	but WITHOUT ANY WARRANTY; without even the implied warranty of
;	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;	GNU General Public License for more details.
;
;	You should have received a copy of the GNU General Public License
;	along with this program.  If not, see <http://www.gnu.org/licenses/>.
;

;***************************************************************************
;**   C O N F I D E N T I A L --- W E S T W O O D   A S S O C I A T E S   **
;***************************************************************************
;*                                                                         *
;*                 Project Name : Westwood 32 bit Library                  *
;*                                                                         *
;*                    File Name : REMAP.ASM                                *
;*                                                                         *
;*                   Programmer : Phil W. Gorrow                           *
;*                                                                         *
;*                   Start Date : July 1, 1994                             *
;*                                                                         *
;*                  Last Update : July 1, 1994   [PWG]                     *
;*                                                                         *
;*-------------------------------------------------------------------------*
;* Functions:                                                              *
;* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *


IDEAL
P386
MODEL USE32 FLAT

INCLUDE "mcgaprim.inc"
INCLUDE ".\gbuffer.inc"


CODESEG

	PROC	MCGA_Remap C NEAR
	USES	eax,ebx,ecx,edx,esi,edi

	;*===================================================================
	;* Define the arguements that our function takes.
	;*===================================================================
	ARG	this	:DWORD
	ARG	x0_pixel:DWORD
	ARG	y0_pixel:DWORD
	ARG	width	:DWORD
	ARG	height	:DWORD
	ARG	remap	:DWORD

	;*===================================================================
	; Define some locals so that we can handle things quickly
	;*===================================================================
	local	x1_pixel  : DWORD
	local	y1_pixel  : DWORD
	local	win_width : dword
	local	counter_x : dword


	cmp	[ remap ] , 0
	jz	??real_out

; Clip Source Rectangle against source Window boundaries.
	mov  	esi , [ this ]	    ; get ptr to src
	xor 	ecx , ecx
	xor 	edx , edx
	mov	edi , [ (VideoViewPort esi) . VIVPWidth ]  ; get width into register
	mov	ebx , [ x0_pixel ]
	mov	eax , [ x0_pixel ]
	add	ebx , [ width ]
	shld	ecx , eax , 1
	mov	[ x1_pixel ] , ebx
	inc	edi
	shld	edx , ebx , 1
	sub	eax , edi
	sub	ebx , edi
	shld	ecx , eax , 1
	shld	edx , ebx , 1

	mov	edi,[ ( VideoViewPort esi) . VIVPHeight ] ; get height into register
	mov	ebx , [ y0_pixel ]
	mov	eax , [ y0_pixel ]
	add	ebx , [ height ]
	shld	ecx , eax , 1
	mov	[ y1_pixel ] , ebx
	inc	edi
	shld	edx , ebx , 1
	sub	eax , edi
	sub	ebx , edi
	shld	ecx , eax , 1
	shld	edx , ebx , 1

	xor	cl , 5
	xor	dl , 5
	mov	al , cl
	test	dl , cl
	jnz	??real_out
	or	al , dl
	jz	??do_remap

	test	cl , 1000b
	jz	??scr_left_ok
	mov	[ x0_pixel ] , 0

??scr_left_ok:
	test	cl , 0010b
	jz	??scr_bottom_ok
	mov	[ y0_pixel ] , 0

??scr_bottom_ok:
	test	dl , 0100b
	jz	??scr_right_ok
	mov	eax , [ (VideoViewPort esi) . VIVPWidth ]  ; get width into register
	mov	[ x1_pixel ] , eax
??scr_right_ok:
	test	dl , 0001b
	jz	??do_remap
	mov	eax , [ (VideoViewPort esi) . VIVPHeight ]  ; get width into register
	mov	[ y1_pixel ] , eax


??do_remap:
       cld	
       mov	edi , [ (VideoViewPort esi) . VIVPOffset ]
       mov	eax , [ (VideoViewPort esi) . VIVPXAdd ]
       mov	ebx , [ x1_pixel ]
       add	eax , [ (VideoViewPort esi) . VIVPWidth ]
       mov	esi , eax
       mul	[ y0_pixel ]
       add	edi , [ x0_pixel ]
       sub	ebx , [ x0_pixel ]
       jle	??real_out
       add	edi , eax
       sub	esi , ebx

       mov	ecx , [ y1_pixel ]
       sub	ecx , [ y0_pixel ]
       jle	??real_out
       mov	eax , [ remap ]
       mov	[ counter_x ] , ebx
       xor	edx , edx

??outer_loop:
       mov	ebx , [ counter_x ]
??inner_loop:
       mov	dl , [ edi ]
       mov	dl , [ eax + edx ]	
       mov	[ edi ] , dl
       inc	edi
       dec	ebx
       jnz	??inner_loop
       add	edi , esi
       dec	ecx
       jnz	??outer_loop




??real_out:
	ret

	ENDP	MCGA_Remap

	END
