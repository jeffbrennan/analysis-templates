/* Visualizations */

/* ---------------- Exploratory plotting ---------------- */


/* Normality assessments */
proc univariate data=df;
    var var1;
    histogram / normal;
    qqplot / normal;
run;


/* ---------------- Linear modeling ---------------- */
proc reg data=df;
    var out pred;
    model out = pred / clb;
  predictor  plot out*pred p.*pred / overlay;
    output out=NEW p=PRED R=Resid;
run;


/* scatter plot w/ confidence bands */
proc reg data = df plots = fitplot(nocli);
    model var1 = var2 / clb;
run;



/* nonparametric - LOWESS */
/* saves output statistics into new df */
proc loess data = df;
    model out_var=pred_var / smooth = 0.5;
    ods output outputstatistics = lowessout;
run;

/* regression & lowess */
proc sgscatter data = df;
    plot resid*pred /loess=(smooth=0.75);
run;

/* add mean confidence limits to loess */

proc sgscatter data = df;
	plot workhrs*lotsize /reg=(clm cli) loess=(smooth=0.5);
run;


/* Multivariable */

/* 3D scatter plot */

data df;
	infile "data.txt";
     input var1 var2 var3;
	 label var1="var 1 description"
	 	dispinc = "var2 descritpion"
	 	sales="var3 description";
run;

options helpport= 63640;
ods html file = "out.html";

/*produce an interactive plot we can spin*/
title "3d plot to spin of Dwaine Data";
proc g3d data = df;
	goptions device=ACTiveX;
	title "3d scatterplot";
	scatter var1*var2 = var3/shape ='pyramid' caxis=black noneedle; 
RUN;
quit;
goptions reset;

ods html file = "out.html" close;



/* diagnostics */

/* scatterplot matrix */
proc sgscatter data = df;
	title "Scatterplot matrix for df";
	matrix var1 var2 var3;
run;


/* correlation matrix */
/*look at the correlation matrix of the data*/
proc corr data = df spearman;
	var var1 var2 var3;
run;
