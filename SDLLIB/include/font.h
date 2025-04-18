/*
**	Command & Conquer Red Alert(tm)
**	Copyright 2025 Electronic Arts Inc.
**
**	This program is free software: you can redistribute it and/or modify
**	it under the terms of the GNU General Public License as published by
**	the Free Software Foundation, either version 3 of the License, or
**	(at your option) any later version.
**
**	This program is distributed in the hope that it will be useful,
**	but WITHOUT ANY WARRANTY; without even the implied warranty of
**	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**	GNU General Public License for more details.
**
**	You should have received a copy of the GNU General Public License
**	along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

/***************************************************************************
 **     C O N F I D E N T I A L --- W E S T W O O D   S T U D I O S       **
 ***************************************************************************
 *                                                                         *
 *                 Project Name : Font and text print 32 bit library       *
 *                                                                         *
 *                    File Name : FONT.H                                   *
 *                                                                         *
 *                   Programmer : Scott K. Bowen                           *
 *                                                                         *
 *                   Start Date : June 27, 1994                            *
 *                                                                         *
 *                  Last Update : June 29, 1994   [SKB]                    *
 *                                                                         *
 *-------------------------------------------------------------------------*
 * Functions:                                                              *
 *   VVPC::Text_Print -- Text print into a virtual viewport.               *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef FONT_H
#define FONT_H

#include "gbuffer.h"


//////////////////////////////////////// Defines //////////////////////////////////////////

// defines for font header, offsets to block offsets

#define FONTINFOBLOCK			4
#define FONTOFFSETBLOCK			6
#define FONTWIDTHBLOCK			8
#define FONTDATABLOCK			10
#define FONTHEIGHTBLOCK			12

// defines for font info block

#define FONTINFOMAXHEIGHT		4
#define FONTINFOMAXWIDTH		5

//////////////////////////////////////// Prototypes //////////////////////////////////////////

/*=========================================================================*/
/* The following prototypes are for the file: SET_FONT.CPP						*/
/*=========================================================================*/

void  * Set_Font(void const *fontptr);

/*=========================================================================*/
/* The following prototypes are for the file: FONT.CPP							*/
/*=========================================================================*/

int Char_Pixel_Width(char chr);
unsigned int String_Pixel_Width(char const *string);

/*=========================================================================*/
/* The following prototypes are for the file: TEXTPRNT.ASM	  					*/
/*=========================================================================*/

#ifdef __cplusplus
extern "C" {
#endif


void Set_Font_Palette_Range(void const *palette, int start_idx, int end_idx);

#ifdef TD
void *Get_Font_Palette_Ptr();
#endif

#ifdef __cplusplus
}
#endif

/*=========================================================================*/





//////////////////////////////////////// External varables ///////////////////////////////////////
extern "C" int FontXSpacing;
extern "C" int FontYSpacing;
extern char FontWidth ;
extern char FontHeight;
extern char *FontWidthBlockPtr;


extern "C" void const *FontPtr;
extern uint8_t ColorXlat[];




#endif // FONT_H
