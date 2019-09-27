/* ---------------- Setup: Data Handling ---------------- */

/* Manual import */
/* datalines is interchangeable with cards */
/* $ needed to indicate categorical var */
input Catvar $ Numvar Catvar2 $ Numvar2;
datalines;
Catvar Numvar Catvar2 Numvar2
;
 
/* Input from delimited .txt/.csv */
/* Use import wizard to avoid needing to manually name columns */
data df;
	infile '\\path.txt' firstobs=1 dlm=' ';
	input var1 var2;

/* Set library (wd) */
/* Save dataframes to this library and access via explorer */
libname lib '\\path';

/* Capturing results & closing output */
ods rtf file='\\path\results.rtf';
ods rt close;

/* ---------------- Tables ---------------- */

/* Conditionally output vars */
proc print data=df;
	var var1 var2;
	where var1 > 40; 
 	where var2 = 'condition';
run;


/* Frequency tables */
/* Given that var1 and var2 are categorical */
proc freq data=df;
	tables var1;
	tables var1*var2 / chisq;
run;


/* Statistical testing */

/* t testing */

/* manual critical value given: probability & degrees of freedom */
data tvalue;
    input probability df;
    critval=abs(tinv(probability,df));
    cards;
    0.025 23
    ;
run;
proc print data = tvalue;
run;


/* mean interval */
proc reg data = df;
    model var1 = var2 / clm alpha = 0.10;
run;     


/* individual prediction interval */
proc reg data = df;
    model  out = pred /cli alpha = 0.10;
run;


/* correlation - normal */
PROC CORR DATA = df pearson;
    VAR var1 var2;
run;

/* corrleation - pearsons */
PROC CORR DATA = df spearman;
    VAR var1 var2;
run;

/* f testing */

/* manual assess critical value */
data FValue;
    input prob ndf ddf;
    critval = (Finv(prob,ndf,ddf));
    cards;
    prob1 ndfd1 ddf1
    ;
run;



/* --------- Diagnostics --------- */

/* make model, create resid */
PROC REG DATA = df;
    MODEL var1 = var2 / clb;
    plot out*pred p.*pred / overlay;
    output out=new P=Pred R=Resid;
run;
quit;

/* create semistudentized resid using MSE from model */
data new;
set new;
semistud=resid/48.8233;
run;

/* normality of semistudentized resid */

proc univariate plot data = new normal;
    var resid semistud;
    histogram;
    qqplot /normal(mu=est sigma=est color=red l=2) square;
run;


/* assess linearity */
symbol1 v=circle l=32 c = black;
symbol2 v=star l=32 c = red interpol = join;
title "scatter plot predicted vs. X overlay";
PROC GPLOT DATA=NEW;
    PLOT out*pred Pred*pred/ OVERLAY;
RUN;
quit;


/* nonparametric regression */
title "prediction diagnostic plots for data";
proc reg data = df plots = prediction(X=pred_var, smooth);
    model out_var=pred_var/clb clm;
    output out = regout1 p=pred r=resid
lclm=lowerbnd uclm=upperbnd;
run;




/* shape of semistudentized resid */

title "plot of semistudentized predicted values
versus residual";
proc gplot data = new;
    plot semistud*pred / vref=0 lvref=2;
run;


/* Tests */

/* Normality - shapiro */







/* TRANSFORMATIONS */


/* Box-Cox w/ untransformed x */
proc transreg data = df ss2 details;
	model boxcox(var1)=identity(var2);
run;


/* Box-Cox w/ transformed x */
/* square root: sqrt() */
proc transreg data = df ss2 details;
	model boxcox(var1)=log(var2);
run;




