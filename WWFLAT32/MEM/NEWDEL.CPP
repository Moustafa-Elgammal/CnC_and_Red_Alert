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
 *                 Project Name : Memory system.                           *
 *                                                                         *
 *                    File Name : NEWDEL.CPP                               *
 *                                                                         *
 *                   Programmer : Scott K. Bowen                           *
 *                                                                         *
 *                   Start Date : June 21, 1994                            *
 *                                                                         *
 *                  Last Update : July 17, 1995   [PWG]                    *
 *                                                                         *
 *-------------------------------------------------------------------------*
 * Functions:                                                              *
 *   operator NEW -- Overides the global new function.                     *
 *   operator delete -- Overides the global delete function.               *
 *   operator NEW[] -- Overides the array version of new.                  *
 *   operator delete[] -- Overides the array version of delete[]           *
 *    --                                                                   *
 *   OPERATOR NEW -- New opperator which takes a MEM_FLAG                  *
 *   OPERATOR NEW[] -- Global NEW[] which takes MEM_FLAG                   *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include "wwmem.h"


/*=========================================================================*/
/* The following PRIVATE functions are in this file:                       */
/*=========================================================================*/

/*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =*/


/***************************************************************************
 * OPERATOR NEW -- Overides the global new function.                       *
 *                                                                         *
 * INPUT:                                                                  *
 *                                                                         *
 * OUTPUT:                                                                 *
 *                                                                         *
 * WARNINGS:                                                               *
 *                                                                         *
 * HISTORY:                                                                *
 *   06/21/1994 SKB : Created.                                             *
 *=========================================================================*/
void * operator new(size_t size) 
{
	return (Alloc((unsigned long) size, MEM_NEW));
}


/***************************************************************************
 * OPERATOR NEW[] -- Overides the array version of new.                    *
 *                                                                         *
 *                                                                         *
 *                                                                         *
 * INPUT:                                                                  *
 *                                                                         *
 * OUTPUT:                                                                 *
 *                                                                         *
 * WARNINGS:                                                               *
 *                                                                         *
 * HISTORY:                                                                *
 *   06/21/1994 SKB : Created.                                             *
 *=========================================================================*/
void * operator new[](size_t size) 
{
	return (Alloc((unsigned long) size, MEM_NEW));
}
 
/***************************************************************************
 * OPERATOR NEW -- New opperator which takes a MEM_FLAG                    *
 *                                                                         *
 *                                                                         *
 *                                                                         *
 * INPUT:                                                                  *
 *                                                                         *
 * OUTPUT:                                                                 *
 *                                                                         *
 * WARNINGS:                                                               *
 *                                                                         *
 * HISTORY:                                                                *
 *   07/17/1995 PWG : Created.                                             *
 *=========================================================================*/
void * operator new(size_t size, MemoryFlagType flag) 
{
	return(Alloc(size, (MemoryFlagType)(flag|MEM_NEW)));
}

/***************************************************************************
 * OPERATOR NEW[] -- Global NEW[] which takes MEM_FLAG                     *
 *                                                                         *
 * INPUT:                                                                  *
 *                                                                         *
 * OUTPUT:                                                                 *
 *                                                                         *
 * WARNINGS:                                                               *
 *                                                                         *
 * HISTORY:                                                                *
 *   07/17/1995 PWG : Created.                                             *
 *=========================================================================*/
void * operator new[] (size_t size, MemoryFlagType flag)
{
	return(Alloc(size, (MemoryFlagType)(flag|MEM_NEW)));
}

/***************************************************************************
 * OPERATOR DELETE -- Overides the global delete function.                 *
 *                                                                         *
 *                                                                         *
 *                                                                         *
 * INPUT:                                                                  *
 *                                                                         *
 * OUTPUT:                                                                 *
 *                                                                         *
 * WARNINGS:                                                               *
 *                                                                         *
 * HISTORY:                                                                *
 *   06/21/1994 SKB : Created.                                             *
 *=========================================================================*/
void operator delete(void *ptr) 
{
	Free(ptr);
}

/***************************************************************************
 * OPERATOR DELETE[] -- Overides the array version of delete[]           	*
 *                                                                         *
 *                                                                         *
 *                                                                         *
 * INPUT:                                                                  *
 *                                                                         *
 * OUTPUT:                                                                 *
 *                                                                         *
 * WARNINGS:                                                               *
 *                                                                         *
 * HISTORY:                                                                *
 *   10/20/1994 SKB : Created.                                             *
 *=========================================================================*/
void operator delete[](void *ptr) 
{
	Free(ptr);
}



