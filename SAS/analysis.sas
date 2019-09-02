/* Inputting Data */


/* Manual Input */
/* datalines is interchangeable with cards */
/* $ needed to indicate categorical var */
	input CatVar $ NumVar Catvar2 $ Numvar2;
	datalines;
	Catvar NumVar Catvar2 Numvar2
	;
 
/* Input from space-delimited .txt */
/* Use import wizard to avoid needing to manually name columns */
data df;
	infile '\\path.txt' firstobs=1 dlm=' ';
	input Var1 Var2;

/* Set library */
/* Save dataframes to this library and access via explorer */
libname lib '\\path';

/* Capturing results & closing output */
ods rtf file='\\path\results.rtf';
ods rt close;

/* Tables */

/* Print lines w/ conditions */
proc print data=df;
	var Var1 Var2;
	where Var1 > 40; 
 	where Var2 = 'condition';
run;


/* Frequency metrics */
/* Given that Var1 and Var2 are categorical */
proc freq data=df;
	tables Var1;
	tables Var1*Var2 / chisq;
run;