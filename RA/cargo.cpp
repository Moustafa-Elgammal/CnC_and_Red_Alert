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

/* $Header: /CounterStrike/CARGO.CPP 1     3/03/97 10:24a Joe_bostic $ */
/***********************************************************************************************
 ***              C O N F I D E N T I A L  ---  W E S T W O O D  S T U D I O S               ***
 ***********************************************************************************************
 *                                                                                             *
 *                 Project Name : Command & Conquer                                            *
 *                                                                                             *
 *                    File Name : CARGO.CPP                                                    *
 *                                                                                             *
 *                   Programmer : Joe L. Bostic                                                *
 *                                                                                             *
 *                   Start Date : April 23, 1994                                               *
 *                                                                                             *
 *                  Last Update : 10/31/94 [JLB]                                               *
 *                                                                                             *
 *---------------------------------------------------------------------------------------------*
 * Functions:                                                                                  *
 *   CargoClass::Attach -- Add unit to cargo hold.                                             *
 *   CargoClass::Attached_Object -- Determine attached unit pointer.                           *
 *   CargoClass::Debug_Dump -- Displays the cargo value to the monochrome screen.              *
 *   CargoClass::Detach_Object -- Removes a unit from the cargo hold.                          *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include	"function.h"


#ifdef CHEAT_KEYS
/***********************************************************************************************
 * CargoClass::Debug_Dump -- Displays the cargo value to the monochrome screen.                *
 *                                                                                             *
 *    This routine is used to dump the current cargo value to the monochrome monitor.          *
 *                                                                                             *
 * INPUT:   none                                                                               *
 *                                                                                             *
 * OUTPUT:  none                                                                               *
 *                                                                                             *
 * WARNINGS:   none                                                                            *
 *                                                                                             *
 * HISTORY:                                                                                    *
 *   06/02/1994 JLB : Created.                                                                 *
 *=============================================================================================*/
void CargoClass::Debug_Dump(MonoClass * mono) const
{
	if (How_Many()) {
		mono->Set_Cursor(63, 3);
		mono->Printf("(%d)%04X", How_Many(), Attached_Object());
	}
}
#endif


/***********************************************************************************************
 * CargoClass::Attach -- Add unit to cargo hold.                                               *
 *                                                                                             *
 *    This routine will add the specified unit to the cargo hold. The                          *
 *    unit will chain to any existing units in the hold. The chaining is                       *
 *    in a LIFO order.                                                                         *
 *                                                                                             *
 * INPUT:   object-- Pointer to the object to attach to the cargo hold.                        *
 *                                                                                             *
 * OUTPUT:  none                                                                               *
 *                                                                                             *
 * WARNINGS:   none                                                                            *
 *                                                                                             *
 * HISTORY:                                                                                    *
 *   04/23/1994 JLB : Created.                                                                 *
 *   10/31/94   JLB : Handles chained objects.                                                 *
 *=============================================================================================*/
void CargoClass::Attach(FootClass * object)
{
	/*
	**	If there is no object, then no action is necessary.
	*/
	if (object == NULL) return;

	object->Limbo();

	/*
	**	Attach any existing cargo hold object to the end of the list as indicated by the
	**	object pointer passed into this routine. This is necessary because several objects may
	**	be attached at one time or several objects may be attached as a result of several calls
	**	to this routine. Either case must be handled properly.
	*/
	ObjectClass * o = object->Next;
	while (o != NULL) {
		if (o->Next == (void*)NULL) break;
		o = o->Next;
	}
	if (o != NULL) {
		o->Next = CargoHold;
	} else {
		object->Next = CargoHold;
	}

	/*
	**	Finally, assign the object pointer as the first object attached to this cargo hold.
	*/
	CargoHold = object;
	Quantity = 0;
	object = CargoHold;
	while (object != NULL) {
		Quantity++;
		object = (FootClass *)(ObjectClass *)object->Next;
	}
}


/***********************************************************************************************
 * CargoClass::Detach_Object -- Removes a unit from the cargo hold.                            *
 *                                                                                             *
 *    This routine will take a unit from the cargo hold and extract it.                        *
 *    The unit extracted is the last unit added to the hold. If there                          *
 *    is no unit in the hold or the occupant is not a unit, then NULL is                       *
 *    returned.                                                                                *
 *                                                                                             *
 * INPUT:   none                                                                               *
 *                                                                                             *
 * OUTPUT:  Returns with a pointer to the unit that has been extracted.                        *
 *                                                                                             *
 * WARNINGS:   none                                                                            *
 *                                                                                             *
 * HISTORY:                                                                                    *
 *   04/23/1994 JLB : Created.                                                                 *
 *   06/07/1994 JLB : Handles generic object types.                                            *
 *=============================================================================================*/
FootClass * CargoClass::Detach_Object(void)
{
	TechnoClass * unit = Attached_Object();

	if (unit != NULL) {
		CargoHold = (FootClass *)(ObjectClass *)unit->Next;
		unit->Next = 0;
		Quantity--;
	}
	return((FootClass *)unit);
}


/***********************************************************************************************
 * CargoClass::Attached_Object -- Determine attached unit pointer.                             *
 *                                                                                             *
 *    This routine will return with a pointer to the attached unit if one                      *
 *    is present. One would need to know this if this is a transport                           *
 *    unit and it needs to unload.                                                             *
 *                                                                                             *
 * INPUT:   none                                                                               *
 *                                                                                             *
 * OUTPUT:  Returns a pointer to the attached unit. If there is no                             *
 *          attached unit, then return NULL.                                                   *
 *                                                                                             *
 * WARNINGS:   none                                                                            *
 *                                                                                             *
 * HISTORY:                                                                                    *
 *   09/07/1992 JLB : Created.                                                                 *
 *   06/07/1994 JLB : Handles generic object types.                                            *
 *=============================================================================================*/
FootClass * CargoClass::Attached_Object(void) const
{
	if (Is_Something_Attached()) {
		return(CargoHold);
	}
	return(NULL);
}


