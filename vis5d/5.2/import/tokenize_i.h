/* tokenize.h */


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
