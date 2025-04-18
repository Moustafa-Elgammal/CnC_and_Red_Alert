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
 *                 Project Name : Westwood 32 bit Library                                      * 
 *                                                                                             * 
 *                    File Name : MOUSE.CPP                                                    * 
 *                                                                                             * 
 *                   Programmer : Philip W. Gorrow                                             * 
 *                                                                                             * 
 *                   Start Date : 12/12/95                                                     * 
 *                                                                                             * 
 *                  Last Update : December 12, 1995 [PWG]                                      * 
 *                                                                                             * 
 *---------------------------------------------------------------------------------------------* 
 * Functions:                                                                                  * 
 *   WWMouseClass::WWMouseClass -- Constructor for the Mouse Class                                 * 
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include "mouse.h"
#include <mmsystem.h>

static WWMouseClass *_Mouse=NULL;
void CALLBACK Process_Mouse( UINT event_id, UINT res1 , DWORD user, DWORD  res2, DWORD  res3 );

/*********************************************************************************************** 
 * MOUSECLASS::MOUSECLASS -- Constructor for the Mouse Class                                   * 
 *                                                                                             * 
 * INPUT:		GraphicBufferClass * screen - pointer to screen mouse is created for				  *                                                                                      * 
 *                                                                                             * 
 * OUTPUT:     none                                                                            * 
 *                                                                                             * 
 * HISTORY:                                                                                    * 
 *   12/12/1995 PWG : Created.                                                                 * 
 *=============================================================================================*/
WWMouseClass::WWMouseClass(GraphicBufferClass *scr, int mouse_max_width, int mouse_max_height)
{
	MouseCursor 	= new char[mouse_max_width * mouse_max_height];
	MouseXHot		= 0;
	MouseYHot		= 0;
	CursorWidth		= 0;
	CursorHeight	= 0;

	MouseBuffer		= new char[mouse_max_width * mouse_max_height];
	MouseBuffX		= -1;
	MouseBuffY  	= -1;
	MaxWidth			= mouse_max_width;
	MaxHeight		= mouse_max_height;

	MouseCXLeft		= 0;
	MouseCYUpper	= 0;
	MouseCXRight	= 0;
	MouseCYLower	= 0;
	MCFlags			= 0;
	MCCount			= 0;

	Screen			= scr;
	PrevCursor		= NULL;
	MouseUpdate		= 0;
	State				= 1;  
	timeBeginPeriod ( 1000/ 60);

	MutexObject		= CreateMutex(NULL, FALSE, "WWLIB32MOUSEMUTEX");
	//
	// Install the timer callback event handler
	//

	EraseBuffer		= new char[mouse_max_width * mouse_max_height];
	EraseBuffX		= -1;
	EraseBuffY  	= -1;
	EraseBuffHotX	= -1;
	EraseBuffHotY	= -1;
	EraseFlags		= FALSE;

	_Mouse			= this;
	TimerHandle = timeSetEvent( 1000/60 , 1 , ::Process_Mouse, 0 , TIME_PERIODIC);
}

WWMouseClass::~WWMouseClass()
{
	WaitForSingleObject(MutexObject, INFINITE);
	MouseUpdate++;

	if (MouseCursor) delete[] MouseCursor;
	if (MouseBuffer) delete[] MouseBuffer;
	if (TimerHandle) {
		timeKillEvent(TimerHandle);
	}
	timeEndPeriod (1);
	ReleaseMutex(MutexObject);
}

void WWMouseClass::Process_Mouse(void)
{
	POINT 	pt;					// define a structure to hold current cursor pos

	//
	// If the mouse is currently hidden or it has not been installed, then we 
	// have no need to redraw the mouse.  
	//
	if (!_Mouse || State > 0 || MouseUpdate || EraseFlags)
		return;

	//
	// Wait until we have exclusive access to our data
	//
	WaitForSingleObject(MutexObject, INFINITE);
	
	//
	// Get the mouse's current real cursor position
	//
	GetCursorPos(&pt);			// get the current cursor position
	//
	// If the mouse has moved then we are responsible to redraw the mouse
	//
	if (pt.x != MouseBuffX || pt.y != MouseBuffY) {
		if (!EraseFlags) {
			//
			// If we can't lock the surface we need to draw to, we cannot update
			// the mouse.
			//
			if (Screen->Lock()) 	{
				//
				// Erase the old mouse by dumping the mouses shadow buff
				//   to the screen (if its position had ever been recorded).
				//
				Low_Hide_Mouse();

				//
				// Verify that the mouse has not gone into a conditional hiden area
				// If it has, mark it as being in one.
				//
				if (MCFlags & CONDHIDE && pt.x >= MouseCXLeft && pt.x <= MouseCXRight && pt.y >= MouseCYUpper && pt.y <= MouseCYLower) {
					MCFlags |= CONDHIDDEN;
				}

				//
				// Show the mouse if we are allowed to.
				//
				if (!(MCFlags & CONDHIDDEN)) 	{
					Low_Show_Mouse(pt.x, pt.y);
				}
				//
				// Finally unlock the destination surface as we have sucessfully
				// updated the mouse.
				//
				Screen->Unlock();
			}
		} else {
			if (MCFlags & CONDHIDE && pt.x >= MouseCXLeft && pt.x <= MouseCXRight && pt.y >= MouseCYUpper && pt.y <= MouseCYLower) {
				MCFlags |= CONDHIDDEN;
				State++;
			}
		}
	}
	ReleaseMutex(MutexObject);
}

void *WWMouseClass::Set_Cursor(int xhotspot, int yhotspot, void *cursor) 
{
	//
	// If the pointer to the cursor we got is invalid, or its the same as the
	// currently set cursor then just return.
	if (!cursor || cursor == PrevCursor) 
		return(cursor);

	//
	// Wait until we have exclusive access to our data
	//
	WaitForSingleObject(MutexObject, INFINITE);
	MouseUpdate++;
	//
	// Since we are updating the mouse we need to hide the cursor so we
	// do not get some sort of weird transformation.
	//
	Hide_Mouse();
	//
	// Now convert the shape to a mouse cursor with the given hotspots and
	// set it as our current mouse.
	//
	void *retval = ASM_Set_Mouse_Cursor(this, xhotspot, yhotspot, cursor);
	//
	// Show the mouse which will force it to appear with the new shape we
	// have assigned.
	//
	Show_Mouse();
	//
	// We are done updating the mouse cursor so on to bigger and better things.
	//
	MouseUpdate--;
	//
	// Inform Windows that we are done using the exclusive data we requested.
	//
	ReleaseMutex(MutexObject);
	//
	// Return the previous mouse cursor which as conveniantly passed back by
	// Asm_Set_Mouse_Cursor.
	//
	return(retval);
}	

void WWMouseClass::Low_Hide_Mouse()
{
	if (!State) {
		while (!Screen->Lock()) {}
		if (MouseBuffX != -1 || MouseBuffY != -1) {
			Mouse_Shadow_Buffer(this, Screen, MouseBuffer, MouseBuffX, MouseBuffY, MouseXHot, MouseYHot, 0);
		}
		MouseBuffX = -1;
		MouseBuffY = -1;
		Screen->Unlock();
	}
	State++;
}
void WWMouseClass::Hide_Mouse()
{
	//
	// Wait until we have exclusive access to our data
	//
	WaitForSingleObject(MutexObject, INFINITE);
	MouseUpdate++;
	Low_Hide_Mouse();
	MouseUpdate--;
	ReleaseMutex(MutexObject);
}


void WWMouseClass::Low_Show_Mouse(int x, int y)
{
	//
	// If the mouse is already visible then just ignore the problem.
	//
	if (State == 0) return;
	//
	// Make the mouse a little bit more visible
	//
	State--;

	//
	//	If the mouse is completely visible then draw it at its current
	// position.
	//
	if (!State)	{
		//
		// Try to lock the screen til we sucessfully get a lock.
		//
		while (!Screen->Lock()) {}
		//
		// Save off the area behind the mouse.
		//
		Mouse_Shadow_Buffer(this, Screen, MouseBuffer, x, y, MouseXHot, MouseYHot, 1);
		//
		// Draw the mouse in its new location
		//
		::Draw_Mouse(this, Screen, x, y);
		//
		// Save off the positions that we saved the buffer from
		//
		MouseBuffX = x;
		MouseBuffY = y;
		//
		// Unlock the screen and lets get moving.
		//
		Screen->Unlock();		
	}
}

void WWMouseClass::Show_Mouse()
{
	POINT	pt;
	GetCursorPos(&pt);

	//
	// Wait until we have exclusive access to our data
	//
	WaitForSingleObject(MutexObject, INFINITE);
	MouseUpdate++;
	Low_Show_Mouse(pt.x, pt.y);
	MouseUpdate--;
	ReleaseMutex(MutexObject);
}

void WWMouseClass::Conditional_Hide_Mouse(int x1, int y1, int x2, int y2)
{
	POINT	pt;

	//
	// Wait until we have exclusive access to our data
	//
	WaitForSingleObject(MutexObject, INFINITE);
	MouseUpdate++;

	//
	// First of all, adjust all the coordinates so that they handle
	// the fact that the hotspot is not necessarily the upper left
	// corner of the mouse.
	//
	x1 -= (CursorWidth - MouseXHot);
	x1  = MAX(0, x1);
	y1 -= (CursorHeight - MouseYHot);
	y1  = MAX(0, y1);
	x2  += MouseXHot;
	x2  = MIN(x2, Screen->Get_Width());
	y2  += MouseYHot;
	y2  = MIN(y2, Screen->Get_Height());

	// The mouse could be in one of four conditions.
	// 1) The mouse is visible and no conditional hide has been specified.
	// 	(perform normal region checking with possible hide)
	// 2) The mouse is hidden and no conditional hide as been specified.
	// 	(record region and do nothing)
	// 3) The mouse is visible and a conditional region has been specified
	// 	(expand region and perform check with possible hide).
	// 4) The mouse is already hidden by a previous conditional.
	// 	(expand region and do nothing)
	//
	// First: Set or expand the region according to the specified parameters
	if (!MCCount) {
		MouseCXLeft		= x1;
		MouseCYUpper	= y1;
		MouseCXRight	= x2;
		MouseCYLower	= y2;
	} else {
		MouseCXLeft		= MIN(x1, MouseCXLeft);
		MouseCYUpper	= MIN(y1, MouseCYUpper);
		MouseCXRight	= MAX(x2, MouseCXRight);
		MouseCYLower	= MAX(y2, MouseCYLower);
	}
	//
	// If the mouse isn't already hidden, then check its location against
	// the hiding region and hide if necessary.
	//
	if (!(MCFlags & CONDHIDDEN)) {
		GetCursorPos(&pt);
		if (pt.x >= MouseCXLeft && pt.x <= MouseCXRight && pt.y >= MouseCYUpper && pt.y <= MouseCYLower) {
			Hide_Mouse();
			MCFlags |= CONDHIDDEN;
		}
	}
	//
	// Record the fact that a conditional hide was called and then exit
	//
	//
	MCFlags |= CONDHIDE;
	MCCount++;
	MouseUpdate--;
	ReleaseMutex(MutexObject);

}
void WWMouseClass::Conditional_Show_Mouse(void)
{
	//
	// Wait until we have exclusive access to our data
	//
	WaitForSingleObject(MutexObject, INFINITE);
	MouseUpdate++;

	//
	// if there are any nested hides then dec the count
	//
	if (MCCount) {
		MCCount--;
		//
		// If the mouse is now not hidden and it had actually been
		// hidden before then display it.
		//
		if (!MCCount) {
			if (MCFlags & CONDHIDDEN) {
				Show_Mouse();
			}
			MCFlags = 0;
		}
	}

	MouseUpdate--;
	ReleaseMutex(MutexObject);
}


void WWMouseClass::Draw_Mouse(GraphicBufferClass *scr)
{
	POINT pt;

	if (State != 0) return;
	//
	// Wait until we have exclusive access to our data
	//
	WaitForSingleObject(MutexObject, INFINITE);
	MouseUpdate++;
	//
	//	Get the position that the mouse is currently located at
	//
	GetCursorPos(&pt);
	if (MCFlags & CONDHIDE && pt.x >= MouseCXLeft && pt.x <= MouseCXRight && pt.y >= MouseCYUpper && pt.y <= MouseCYLower) {
		Hide_Mouse();
		MCFlags |= CONDHIDDEN;
	} else {
		//
		// If the mouse is already visible then just ignore the problem.
		//
		EraseFlags = TRUE;

		//
		// Try to lock the screen til we sucessfully get a lock.
		//
		while (!scr->Lock()) {}
		//
		// Save off the area behind the mouse into two different buffers, one
		// which will be used to restore the mouse and the other which will
		// be used to restore the hidden surface when we get a chance.
		//
		Mouse_Shadow_Buffer(this, scr, EraseBuffer, pt.x, pt.y, MouseXHot, MouseYHot, 1);
		memcpy(MouseBuffer, EraseBuffer, MaxWidth * MaxHeight);
		//
		// Draw the mouse in its new location
		//
		::Draw_Mouse(this, scr, pt.x, pt.y);
		//
		// Save off the positions that we saved the buffer from
		//
		EraseBuffX 		= pt.x;
		MouseBuffX 		= pt.x;
		EraseBuffY 		= pt.y;
		MouseBuffY 		= pt.y;
		EraseBuffHotX	= MouseXHot;
		EraseBuffHotY	= MouseYHot;
		//
		// Unlock the screen and lets get moving.
		//
		scr->Unlock();
	}

	MouseUpdate--;
	ReleaseMutex(MutexObject);
}

void WWMouseClass::Erase_Mouse(GraphicBufferClass *scr, int forced)
{
//	if (!EraseFlags) return;
//	if (State != 0) return;

	//
	// If we are forcing the redraw of a mouse we already managed to
	// restore then just get outta here.
	//
	if (forced && EraseBuffX == -1 && EraseBuffY == -1) return;

	//
	// If we don't own the Mutex Object, then we need to because we may
	// update the mouse.
	//
	WaitForSingleObject(MutexObject, INFINITE);
	MouseUpdate++;

	//
	// If this is not a forced call, only update the mouse is we can legally
	//	lock the buffer.
	//
	if (!forced) {
#if(0)
		if (scr->Lock()) {
			//
			// If the surface has not already been restore then restore it and erase the 
			// restoration coordinates so we don't accidentally do it twice.
			//
			if (EraseBuffX != -1 || EraseBuffY != -1) {
				Mouse_Shadow_Buffer(this, scr, EraseBuffer, EraseBuffX, EraseBuffY, 0);
				EraseBuffX = -1;
				EraseBuffY = -1;
			}
			//
			// We are done writing to the buffer so unlock it.
			//
			scr->Unlock();
		}
#endif
	} else	{
		//
		// If this is a forced call then wait til we are allowed to update the buffer
		//
		while (!scr->Lock()) {}
		//
		// If the surface has not already been restore then restore it and erase the 
		// restoration coordinates so we don't accidentally do it twice.
		//
		if (EraseBuffX != -1 || EraseBuffY != -1) {
			Mouse_Shadow_Buffer(this, scr, EraseBuffer, EraseBuffX, EraseBuffY, EraseBuffHotX, EraseBuffHotY, 0);
		}
		//
		// We are done writing to the buffer so unlock it.
		//
		scr->Unlock();
	}
	MouseUpdate--;
	ReleaseMutex(MutexObject);
  	EraseFlags = FALSE;
}

int WWMouseClass::Get_Mouse_State(void)
{
	return(State);
}
/***********************************************************************************************
 * WWKeyboardClass::Get_Mouse_X -- Returns the mouses current x position in pixels             *
 *                                                                                             *
 * INPUT:		none                                                                            *
 *                                                                                             *
 * OUTPUT:     int		- returns the mouses current x position in pixels                      *
 *                                                                                             *
 * HISTORY:                                                                                    *
 *   10/17/1995 PWG : Created.                                                                 *
 *=============================================================================================*/
int WWMouseClass::Get_Mouse_X(void)
{
	POINT 	pt;
	GetCursorPos(&pt);
	return(pt.x);
}


/***********************************************************************************************
 * WWKeyboardClass::Get_Mouse_Y -- returns the mouses current y position in pixels             *
 *                                                                                             *
 * INPUT:		none                                                                            *
 *                                                                                             *
 * OUTPUT:     int		- returns the mouses current y position in pixels                      *
 *                                                                                             *
 * HISTORY:                                                                                    *
 *   10/17/1995 PWG : Created.                                                                 *
 *=============================================================================================*/
int WWMouseClass::Get_Mouse_Y(void)
{
	POINT 	pt;
	GetCursorPos(&pt);
	return(pt.y);
}

/***********************************************************************************************
 * WWKeyboardClass::Get_Mouse_XY -- Returns the mouses x,y position via reference vars         *
 *                                                                                             *
 * INPUT:		int &x		- variable to return the mouses x position in pixels                *
 *					int &y		- variable to return the mouses y position in pixels					  *
 *                                                                                             *
 * OUTPUT:     none - output is via reference variables                                        *
 *                                                                                             *
 * HISTORY:                                                                                    *
 *   10/17/1995 PWG : Created.                                                                 *
 *=============================================================================================*/
void WWMouseClass::Get_Mouse_XY(int &x, int &y)
{
	POINT 	pt;

	GetCursorPos(&pt);
	x = pt.x;
	y = pt.y;
}

#pragma off(unreferenced)
void CALLBACK Process_Mouse( UINT event_id, UINT res1 , DWORD user, DWORD  res2, DWORD  res3 )
{
	if (_Mouse) {
		_Mouse->Process_Mouse();
	}
}
#pragma on(unreferenced)

void Hide_Mouse(void)
{
	if (!_Mouse) return;
	_Mouse->Hide_Mouse();
}
	
void Show_Mouse(void)
{
	if (!_Mouse) return;
	_Mouse->Show_Mouse();
}

void Conditional_Hide_Mouse(int x1, int y1, int x2, int y2)
{
	if (!_Mouse) return;
	_Mouse->Conditional_Hide_Mouse(x1, y1, x2, y2);
}

void Conditional_Show_Mouse(void)
{
	if (!_Mouse) return;
	_Mouse->Conditional_Show_Mouse();
}

int Get_Mouse_State(void)
{
	if (!_Mouse) return(0);
	return(_Mouse->Get_Mouse_State());
}

void *Set_Mouse_Cursor(int hotx, int hoty, void *cursor)
{
	if (!_Mouse) return(0);
	return(_Mouse->Set_Cursor(hotx,hoty,cursor));
}

int Get_Mouse_X(void)
{
	if (!_Mouse) return(0);
	return(_Mouse->Get_Mouse_X());
}

int Get_Mouse_Y(void)
{
	if (!_Mouse) return(0);
	return(_Mouse->Get_Mouse_Y());
}
