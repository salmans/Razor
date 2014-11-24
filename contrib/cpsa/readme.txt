CPSA analysis to Razor converter

Converts the output of a CPSA run into a formula in Razor syntax.
Requires SWI-Prolog.

Suppose the CPSA source is or.scm.  To generate the shape analysis
sentences using Make.hs from the CPSA documents directory, type:

$ echo 'sas "or"' | ghci Make.hs
$ swipl
?- [razor].
?- razor('or_sas.text', 'or_sas.raz').
?- halt.
$

To handle the subsort relations of the message algebra, add the
following axioms.

   akey(t) => mesg(t);
   skey(t) => mesg(t);
   name(t) => mesg(t);
   text(t) => mesg(t);
   name(t) => mesg(t);

The precedes axioms are:

   sprec(x, y) => prec(x, y);
   prec(x, y) & prec(y, z) => prec(x, y);
