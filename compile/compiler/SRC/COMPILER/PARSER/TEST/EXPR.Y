/*@A (C) 1992 Allen I. Holub                                                */
   /* This is the occs input file for the expression compiler in Appendix E
    */



%term  ID 	/*  a string of lower-case characters	*/
%term  NUM	/*  a number				*/

%left  PLUS 	/*  +					*/
%left  STAR	/*  *					*/
%left  LP RP	/*  (  )				*/

%{
#include <stdio.h>
#include <ctype.h>
#include <malloc.h>
#include <tools/debug.h>	/* For P() definition	*/

extern char *yytext;      	 /* Lexeme, In yylex()			 */
extern int  yyparse P((void)); 	 /* Parser, Generated by occs in yyout.c */

char *new_name	P(( void    ));  /* declared at bottom of this file */
void free_name	P(( char *s ));

typedef char	     *stype;	   /* Value stack */
#define YYSTYPE      stype

#define YYMAXDEPTH   64
#define YYMAXERR     10
#define YYVERBOSE
%}

%%
/* A small expression grammar that recognizes numbers, names, addition (+),
 * multiplication (*), and parentheses. Expressions associate left to right
 * unless parentheses force it to go otherwise. * is higher precedence than +.
 * Note that an underscore is appended to identifiers so that they won't be
 * confused with rvalues.
 */

s	: e ;

e	: e PLUS e   { yycode("%s += %s\n", $1, $3); free_name( $3 ); }
	| e STAR e   { yycode("%s *= %s\n", $1, $3); free_name( $3 ); }
	| LP e RP    { $$ = $2;	    				     }
  	| NUM	     { yycode("%s = %s\n",  $$ = new_name(), yytext ); }
	| ID 	     { yycode("%s = _%s\n", $$ = new_name(), yytext ); }
	;
%%
/*----------------------------------------------------------------------*/
#ifdef __TURBOC__
#pragma argsused
#endif

char	*yypstk( val, symbol)
void	*val;
char	*symbol;			/* not used */
{
    /* Yypstk is used by the debugging routines. It is passed a pointer to a
     * value-stack item and should return a string representing that item. In
     * this case, all it has to do is dereference one level of indirection.
     */

    return *(char **)val ? *(char **)val : "<empty>" ;
}
/*----------------------------------------------------------------------*/
char  *Names[] = { "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7" };
char  **Namep  = Names;

char	*new_name()
{
    /* Return a temporary-variable name by popping one off the name stack.  */

    if( Namep >= &Names[ sizeof(Names)/sizeof(*Names) ] )
    {
	yyerror("Expression too complex\n");
	exit( 1 );
    }

    return( *Namep++ );
}

void free_name(s)
char	*s;
{			/* Free up a previously allocated name */
    *--Namep = s;
}
/*----------------------------------------------------------------------*/
#ifdef __TURBOC__
#pragma argsused
#endif

void yy_init_occs( tos )
void *tos;		/* not used here */
{
    /* Generate declarations for the rvalues */

    yycode("public word t0, t1, t2, t3;\n");
    yycode("public word t4, t5, t6, t7;\n");
}

void main( argc, argv )
int  argc;
char **argv;
{
    /* Open the input file, using yy_get_args() if we're debugging or
     * ii_newfile() if not.
     */

#ifdef YYDEBUG
    yy_get_args( argc, argv );
#else
    if( argc < 2 )
    {
	fprintf( stderr, "Need file name\n");
	exit(1);
    }
    else if( ii_newfile(argv[1]) < 0 )
    {
	fprintf( stderr, "Can't open %s\n", argv[1] );
	exit(2);
    }
#endif
    yyparse();
    exit( 0 );
}
