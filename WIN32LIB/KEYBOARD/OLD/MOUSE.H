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

/*********************************************************************************************** 
 *                                                                                             * 
 *                 Project Name : Westwood 32 Bit Library                                      * 
 *                                                                                             * 
 *                    File Name : MOUSE.H                                                      * 
 *                                                                                             * 
 *                   Programmer : Philip W. Gorrow                                             * 
 *                                                                                             * 
 *                   Start Date : 12/12/95                                                     * 
 *                                                                                             * 
 *                  Last Update : December 12, 1995 [PWG]                                      * 
 *                                                                                             * 
 *---------------------------------------------------------------------------------------------* 
 * Functions:                                                                                  * 
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef WW_MOUSE_H
#define WW_MOUSE_H

#include <gbuffer.h>

class WWMouseClass {
	public:
		WWMouseClass(GraphicBufferClass *scr, int mouse_max_width, int mouse_max_height);
		~WWMouseClass();
		void *Set_Cursor(int xhotspot, int yhotspot, void *cursor);
		void Process_Mouse(void);
		void Hide_Mouse(void);
		void Show_Mouse(void);
		void Conditional_Hide_Mouse(int x1, int y1, int x2, int y2);
		void Conditional_Show_Mouse(void);
		int Get_Mouse_State(void);
		int Get_Mouse_X(void);
		int Get_Mouse_Y(void);
		void Get_Mouse_XY(int &x, int &y);
		//
		// The following two routines can be used to render the mouse onto a graphicbuffer
		// other than the hidpage.
		//
		void Draw_Mouse(GraphicBufferClass *scr);
		void Erase_Mouse(GraphicBufferClass *scr, int forced = FALSE);

	private:
		enum 	{
			CONDHIDE		= 1,
			CONDHIDDEN 	= 2,
		};
		void Low_Hide_Mouse(void);
		void Low_Show_Mouse(int x, int y);

		char						*MouseCursor;	// pointer to the mouse cursor in memory
		int						MouseXHot;		// X hot spot of the current mouse cursor
		int						MouseYHot;		// Y hot spot of the current mouse cursor
		int						CursorWidth;	// width of the mouse cursor in pixels
		int						CursorHeight;	// height of the mouse cursor in pixels

		char						*MouseBuffer;	// pointer to background buffer in memory
		int						MouseBuffX;		// pixel x mouse buffer was preserved at
		int						MouseBuffY;		// pixel y mouse buffer was preserved at
		int						MaxWidth;		// maximum width of mouse background buffer
		int						MaxHeight;		// maximum height of mouse background buffer

		int						MouseCXLeft;	// left x pos if conditional hide mouse in effect
		int						MouseCYUpper;	// upper y pos if conditional hide mouse in effect
		int						MouseCXRight;	// right x pos if conditional hide mouse in effect
		int						MouseCYLower;	// lower y pos if conditional hide mouse in effect
		char						MCFlags;			// conditional hide mouse flags
		char						MCCount;			// nesting count for conditional hide mouse

		GraphicBufferClass	*Screen;			// pointer to the surface mouse was init'd with
		char *					PrevCursor;		// pointer to previous cursor shape
		int						MouseUpdate;
		int						State;

		char						*EraseBuffer;	// Buffer which holds background to restore to hidden page
		int						EraseBuffX;		// X position of the hidden page background
		int						EraseBuffY;		// Y position of the hidden page background
		int						EraseBuffHotX;	// X position of the hidden page background
		int						EraseBuffHotY;	// Y position of the hidden page background

		int						EraseFlags;		// Records whether mutex has been released

		HANDLE					MutexObject;	// Handle the Mutex Object created when mouse is
		unsigned					TimerHandle;

};

extern "C" {
	 void __cdecl Mouse_Shadow_Buffer(void *thisptr, GraphicBufferClass *srcdst, void *buffer, int x, int y, int hotx, int hoty, int store);
	 void __cdecl Draw_Mouse(void *thisptr, GraphicBufferClass *srcdst, int x, int y);
	 void __cdecl *ASM_Set_Mouse_Cursor(void * thisptr, int hotspotx, int hotspoty, VOID *cursor);
};

void Hide_Mouse(void);
void Show_Mouse(void);
void Conditional_Hide_Mouse(int x1, int y1, int x2, int y2);
void Conditional_Show_Mouse(void);
int Get_Mouse_State(void);
void *Set_Mouse_Cursor(int hotx, int hoty, void *cursor);
int Get_Mouse_X(void);
int Get_Mouse_Y(void);

#endif
