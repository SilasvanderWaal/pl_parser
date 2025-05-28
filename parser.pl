/******************************************************************************/
/* Prolog Lab 2 example - Grammar test bed                                    */
/******************************************************************************/
:- initialization(testall).

parser(Tokens, Res) :-
   (program(Tokens, Res), Res = [ ], write('Parse OK!'));
    write('Parse Fail!').

/******************************************************************************/
/* Lexer logic                                                                */
/******************************************************************************/
lexer([ ], [ ]).
lexer([H|T], [F|S]) :-match(H, F), lexer(T,S).

/******************************************************************************/
/* Mapping logic                                                              */
/******************************************************************************/
match(L, F) :- L = 'program', F is 256.
match(L, F) :- L = 'input', F is 257.
match(L, F) :- L = 'output', F is 258.
match(L, F) :- L = 'var', F is 259.
match(L, F) :- L = 'integer', F is 260.
match(L, F) :- L = 'begin', F is 261.
match(L, F) :- L = 'end', F is 262.
match(L, F) :- L = 'real', F is 264.
match(L, F) :- L = 'boolean', F is 263.

match(L, F) :- L = '(' , F is 40.
match(L, F) :- L = ')' , F is 41.
match(L, F) :- L = ',' , F is 44.
match(L, F) :- L = '.' , F is 46.
match(L, F) :- L = ':' , F is 58.
match(L, F) :- L = ';' , F is 59.
match(L, F) :- L = '*' , F is 42.
match(L, F) :- L = '+' , F is 43.
match(L, F) :- L = ':=' , F is 271.
match(L, F) :- L = -1, F is 275.  %EOF

match(L, F) :- 
        name(L, [T|S]), char_type(T, csymf), match_string(S), F is 270.
match(L, F) :-
    name(L, [T|S]), char_type(T, digit), match_digit(S), F is 272.
match(_, F) :- F is 273.    %undefined symbol

match_string([ ]).
match_string([H|T]) :- char_type(H, csym), match_string(T).

match_digit([ ]).
match_digit([H|T]) :- char_type(H, digit), match_digit(T).


/******************************************************************************/
/* Grammar Rules in Definite Clause Grammar form                              */
/* This the set of productions, P, for this grammar                           */
/* This is a slightly modified from of the Pascal Grammar for Lab 2 Prolog    */
/******************************************************************************/

program       --> prog_head, var_part, stat_part.

/******************************************************************************/
/* Program Header                                                             */
/******************************************************************************/
prog_head     --> [256], [270], [40], [257], [44], [258], [41], [59].

/******************************************************************************/
/* Var_part                                                                   */
/******************************************************************************/
var_part             --> [259], var_dec_list.
var_dec_list         --> var_dec | var_dec, var_dec_list.
var_dec              --> id_list , [58], type, [59].
id_list              --> [270], [44], id_list | [270]. 
type                  --> [260] | [263] | [264].

/******************************************************************************/
/* Stat part                                                                  */
/******************************************************************************/
stat_part            -->  [261], stat_list, [262], [46].
stat_list            --> stat | stat, [59], stat_list.
stat                 --> assign_stat.
assign_stat          --> [270], [271], expr.
expr                 --> term | term, [43], expr.
term                 --> factor | factor, [42], term. 
factor              --> [40], expr, [41] | operand.
operand             --> [270] | [272].

/******************************************************************************/
/* Testing the system: this may be done stepwise in Prolog                    */
/* below are some examples of a "bottom-up" approach - start with simple      */
/* tests and buid up until a whole program can be tested                      */
/******************************************************************************/
/* Stat part                                                                  */
/******************************************************************************/
/*  op(['+'], []).                                                            */
/*  op(['-'], []).                                                            */
/*  op(['*'], []).                                                            */
/*  op(['/'], []).                                                            */
/*  addop(['+'], []).                                                         */
/*  addop(['-'], []).                                                         */
/*  mulop(['*'], []).                                                         */
/*  mulop(['/'], []).                                                         */
/*  factor([a], []).                                                          */
/*  factor(['(', a, ')'], []).                                                */
/*  term([a], []).                                                            */
/*  term([a, '*', a], []).                                                    */
/*  expr([a], []).                                                            */
/*  expr([a, '*', a], []).                                                    */
/*  assign_stat([a, assign, b], []).                                          */
/*  assign_stat([a, assign, b, '*', c], []).                                  */
/*  stat([a, assign, b], []).                                                 */
/*  stat([a, assign, b, '*', c], []).                                         */
/*  stat_list([a, assign, b], []).                                            */
/*  stat_list([a, assign, b, '*', c], []).                                    */
/*  stat_list([a, assign, b, ';', a, assign, c], []).                         */
/*  stat_list([a, assign, b, '*', c, ';', a, assign, b, '*', c], []).         */
/*  stat_part([begin, a, assign, b, '*', c, end, '.'], []).                   */
/******************************************************************************/
/* Var part                                                                   */
/******************************************************************************/
/* typ([integer], []).                                                        */
/* typ([real], []).                                                           */
/* typ([boolean], []).                                                        */
/* id([a], []).                                                               */
/* id([b], []).                                                               */
/* id([c], []).                                                               */
/* id_list([a], []).                                                          */
/* id_list([a, ',', b], []).                                                  */
/* id_list([a, ',', b, ',', c], []).                                          */
/* var_dec([a, ':', integer], []).                                            */
/* var_dec_list([a, ':', integer], []).                                       */
/* var_dec_list([a, ':', integer, b, ':', real], []).                         */
/* var_part([var, a, ':', integer], []).                                      */
/******************************************************************************/
/* Program header                                                             */
/******************************************************************************/
/* prog_head([program, c, '(', input, ',', output, ')', ';'], []).            */
/******************************************************************************/

/******************************************************************************/
/* Whole program                                                              */
/******************************************************************************/
/* program([program, c, '(', input, ',', output, ')', ';',                    */
/*          var, a,    ':', integer, ';',                                     */
/*               b, ',', c, ':', real,    ';',                                */
/*          begin,                                                            */
/*             a, assign, b, '*', c, ';',                                     */  
/*             a, assign, b, '+', c,                                          */
/*          end, '.'], []).                                                   */
/******************************************************************************/

/******************************************************************************/
/* Define the above tests                                                     */
/******************************************************************************/

testph :- prog_head([program, c, '(', input, ',', output, ')', ';'], []).
testpr :-   program([program, c, '(', input, ',', output, ')', ';'], []).

testall :- list_files_in_directory('testfiles', Files), sort(Files, SortedFiles),
parseFiles(SortedFiles), halt.
    
parseFiles([ ]).
parseFiles([H|T]) :-
    write('Testing '), write(H), nl,
    read_in(H,L), write(L), nl,
    lexer(L, Tokens), write(Tokens), nl,
    parser(Tokens, _), nl,
    write(H), write('end of parse'), nl, nl,
    parseFiles(T).

list_files_in_directory(Dir, Files) :-
    directory_files(Dir, AllFiles),
    exclude(is_dot_file, AllFiles, VisibleFiles),
    maplist({Dir}/[File, Path]>>directory_file_path(Dir, File, Path), VisibleFiles, Files).

is_dot_file('.').
is_dot_file('..').


/******************************************************************************/
/* End of program                                                             */
/******************************************************************************/

/******************************************************************************/
/* CMREADER FILE  */
/******************************************************************************/

/******************************************************************************/
/* From Programming in Prolog (4th Ed.) Clocksin & Mellish, Springer (1994)   */
/* Chapter 5, pp 101-103 (DFR (140421) modified for input from a file)        */
/******************************************************************************/

read_in(File,[W|Ws]) :- see(File), get0(C), 
                        readword(C, W, C1), restsent(W, C1, Ws), nl, seen.

/******************************************************************************/
/* Given a word and the character after it, read in the rest of the sentence  */
/******************************************************************************/

restsent(W, _, [])         :- W = -1.                /* added EOF handling */
restsent(W, _, [])         :- lastword(W).
restsent(_, C, [W1 | Ws ]) :- readword(C, W1, C1), restsent(W1, C1, Ws).

/******************************************************************************/
/* Read in a single word, given an initial character,                         */
/* and remembering what character came after the word (NB!)                   */
/******************************************************************************/
readword(C, W, C2) :- C = 58, get0(C1), readwordaux(C, W, C1, C2).
readword(C, W, _)  :- C = -1, W = C.                    /* added EOF handling */
readword(C, W, C1) :- single_character( C ), name(W, [C]), get0(C1).
readword(C, W, C2) :-
   in_word(C, NewC ),
   get0(C1),
   restword(C1, Cs, C2),
   name(W, [NewC|Cs]).

readword(_, W, C2) :- get0(C1), readword(C1, W, C2).

readwordaux(C, W, C1, C2) :- C1 = 61, name(W, [C, C1]), get0(C2).
readwordaux(C, W, C1, C2) :- C1 \= 61, name(W, [C]), C1 = C2.

restword(C, [NewC|Cs], C2) :-
   in_word(C, NewC),
   get0(C1),
   restword(C1, Cs, C2).

restword(C, [ ], C).

/******************************************************************************/
/* These characters form words on their own                                   */
/******************************************************************************/

single_character(40).                  /* ( */
single_character(41).                  /* ) */
single_character(42).                  /* + */
single_character(43).                  /* * */
single_character(44).                  /* , */
single_character(59).                  /* ; */
single_character(58).                  /* : */
single_character(61).                  /* = */
single_character(46).                  /* . */
single_character(45).                  /* - */

/******************************************************************************/
/* These characters can appear within a word.                                 */
/* The second in_word clause converts character to lower case                 */
/******************************************************************************/

in_word(C, C) :- C>96, C<123.             /* a b ... z */
in_word(C, L) :- C>64, C<91, L is C+32.   /* A B ... Z */
in_word(C, C) :- C>47, C<58.              /* 1 2 ... 9 */

/******************************************************************************/
/* These words terminate a sentence                                           */
/******************************************************************************/

lastword('.').

/******************************************************************************/
/* added for demonstration purposes 140421, updated 150301                    */
/* testa  - file input (characters + Pascal program)                          */
/* testb  - file input as testa + output to file                              */
/* ttrace - file input + switch on tracing (check this carefully)             */
/******************************************************************************/

testa   :- testread(['cmreader.txt', 'testok1.pas']).
testb   :- tell('cmreader.out'), testread(['cmreader.txt', 'testok1.pas']), told.

ttrace  :- trace, testread(['cmreader.txt']), notrace, nodebug.

testread([]).
testread([H|T]) :- nl, write('Testing C&M Reader, input file: '), write(H), nl,
                   read_in(H,L), write(L), nl,
                   nl, write(' end of C&M Reader test'), nl,
                   testread(T).

/******************************************************************************/
/* end of program                                                             */
/******************************************************************************/
