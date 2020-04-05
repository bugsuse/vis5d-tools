/* tokenize.h */
/* Vis5D version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990 - 2000 Bill Hibbard, Johan Kellum, Brian Paul,
Dave Santek, and Andre Battaiola.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/


/*
 * Divide an input string into a sequence of tokens similar to how argc and
 * argv pass arguments to main().
 */


#ifndef TOKENIZE_H
#define TOKENIZE_H



/*
 * Divide a line of input into tokens separated by whitespace.
 * Input:  str - the input string
 * Output:  ntokens - number of tokens found
 * Return:  address of array of pointers to token strings
 *
 * Example:
 *    char **tokenv;
 *    int tokenc;
 *    tokenv = tokenize( "this is a test.", &tokenc );
 * Then:
 *    tokenv[0] = "this"
 *    tokenv[1] = "is"
 *    tokenv[2] = "a"
 *    tokenv[3] = "test."
 */
extern char **tokenize( char *str, int *ntokens );


/*
 * Free a vector of tokens as returned by tokenize().
 * Input:  tokens - the address of the array of pointers to token strings
 */
extern void free_tokens( char **tokens );


#endif
