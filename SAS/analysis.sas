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



/* --------- Regression Diagnostics --------- */

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

/* Heteroschedasticity - bp */
proc model data =plutonium3;
	alpha=b0+b1*plutact; /*this is the linear model for proc model to match our model of interest*/
	fit alpha PARMS=(b0 b1)/ breusch=(plutact);  /*the PARMS statement lets sas know what parameters we are interested in estimating by least squares.*/
run;

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




/* ------------- Multiple Regression -------------- */



/* Manually obtain interval estimate for one parameter (b) */
/* Parent df is matrix */
/* x_col is integer representing position of desired predictor in 
   df
 */
start sumofsquares;
    SSTO=t(y)*y-(1/n)*t(y)*j(n,n,1)*y;
    SSE=t(y)*y-t(b)*t(x)*y;
    MSE=SSE/(n-p);
    varb=MSE*inv(t(x)*x);
finish sumofsquares;
start inference;
    tstat=abs(b[x_col]/sqrt(varb[x_col, x_col])); 
    tpvalue=2*(1-probt(tstat,n-p));
    bintervalub=b[3]+tinv(0.975,n-p)*sqrt(varb[3,3]);
    bintervallb=b[3]-tinv(0.975,n-p)*sqrt(varb[3,3]);
    finish inference;



/* Multivar diagnostics */

start outtodata;
    studres=(1/sqrt(mse))*e; /*compute the
semistudentized residuals*/
    sqresid=e##2; /*# is the element wise power
operator, this line squares each element in e*/
    matout=y||x[,2:p]||yhat||e||studres||sqresid;
    create fattrimidreg from matout [colname =
    {"Response" "Pred1" "Pred2" "preds" "resids" "studres"
    "sqresid"}];
    append from matout;
    finish outtodata;



/* ------------ Matrices -------- */

/* Simple ops */
proc iml;
start matrixops; /*start a module in iml called Mytranspose*/
	A={2 5, 10 3, 9 4}; /*define matrix A, comma separates rows, spaces for columns*/
	B ={1 3, 4 6, 9 0};
    AT=t(A);
    A_INV  = inv(A);
    A_DET = det(A);
    A_RANK = round(trace(ginv(A)*A));

    ID_4 = I(4)
    C = A+B;
    D = A-B;
    E = A * 4 /* scalar times matrix*/
    F = A * B /* matrix times matrix*/

	print A, AT, C, D;/*print the matrices A, AT, C, and CT. NOTE for printing: Comma prints out the matrices separately, no comma puts them in one table.*/
finish matrixops;
run matrixops;
quit;

/* Regression w/ matrices */

/* read data into matrix */
proc iml;
    start datasetup;
        use df;
        read all var {'var1'} into y 
        read all var {'var2'} into xmat
        n = nrow(xmat)
        x=j(n,1,1)||xmat; /*add a column of 1's to the left side of x the command j(a,b,c) fills a matrix of a rows, b columns with the value c)*/
        p=ncol(x); /* set p = number of columns of design matrix, or total number of beta coefficients*/
    finish datasetup;
        
    start estimateb;
        xpx= t(x)*x; /*compute X'X*/
        xpy= t(x)*y; /*compute X'Y*/
        xpinv=inv(xpx); /*compute the inverse of X'x*/
        b=inv(xpx)*xpy; /*compute b vector*/
    finish estimateb;

    start predicted;
        yhat=x*b;
        e=y-yhat;
        hat=x*inv(xpx)*t(x);
    finish predicted;

    start sumofsquares; /*this module computes each of the sum of squares using the non-quadratic forms of the matrix equations (SSR is left for you to do on your own)*/
		SSTO=t(y)*y-(1/n)*t(y)*j(n,n,1)*y; /*computes total sum of squares eqn 5.83*/
		SSE=t(y)*y-t(b)*t(x)*y; /*computes Error sum of squares eqn 5.84a */
		MSE=SSE/(n-p);/*compute mean squared error recall p is the number of columns of the design matrix (including X0) */
		varb=MSE*inv(t(x)*x); /*compute the variance-covariance matrix of of b*/
		xh={1,65}; /* vector of the new value for prediciton of the mean*/
		meanpredvar=t(xh)*varb*xh; /*compute variance for predicting mean response at Xh*/
	finish sumofsquares;

	start inference;
		tstat =j(p,1,-1);/*start a matrix tstat and tpvalue to hold below computations*/
		tpvalue=j(p,1,-1);
		binterval=j(p,2,0);
		do i = 1 to p;
			tstat[i]=(b[i]/sqrt(varb[i,i]));/*compute the t-statistic*/
		                           /*b[3] is the coefficient for midarm circumference varb[3,3] is the variance term for midarm circumference*/
     		tpvalue[i]=2*(1-probt(abs(tstat[i]),n-p)); /*compute the pvalue for the tstatistic, with n-p df. we multiply by 2 for two tail test.  due to symmmetry we take the absolute value for simplicity of finding the pvalue*/
			binterval[i,1]=b[i]-tinv(0.975,n-p)*sqrt(varb[i,i]); /*lower bound of interval for the coefficients, put on left column of binterval*/
			binterval[i,2]=b[i]+tinv(0.975,n-p)*sqrt(varb[i,i]); /*upper bound of interval for the coeffficients, put in right column of binterval*/
		end;
	finish inference;


	start computeF;
		SSR=t(b)*t(x)*y-(1/n)*t(y)*j(n,n,1)*y; /*compute SSR*/
		MSR=SSR/(p-1);/*compute the regression mean squares. For SLR p=2, but holds for multivariate regression */
		Fstat=MSR/MSE; /*compute the ANOVA F statsitic for the model*/
		Fpvalue = 1-probF(Fstat,p-1,n-p); /*compue the pvalue for the anova F statistic*/
	finish computeF;

    start partialcorr;
		Rsq=SSR/SSTO;
		adjRsq=1-((n-1)/(n-p))*(SSE/SSTO);
	finish partialcorr;


	start outtodata;
		studres=(1/sqrt(mse))*e; /*compute the semi-studentized residuals*/
		sqresid=e##2; /*## is the element wise power operator, this line squares each element in e*/	
						/*not the same as t(e)*e or e*t(e). one is a scalar one is a matrix*/
		matout=y||x[,2:p]||yhat||e||studres||sqresid;
		create Dwainereg from matout [colname = {"var1" "var2" "var3" "preds" "resids" "studres" "sqresid"}]; /*create a dataset in sas with the varnames in {}*/ 
			append from matout;
	finish outtodata;
	

    run datasetup;
    run estimateb;
    run predicted;
    run sumofsquares;
    run outtodata;
    print b varb MSE rootMSE SSE;
    run computef;
    run inference;
    run partialcorr;
    print fstat fpvalue, b varb tstat tpvalue binterval, Rsq adjRsq, MSE rootMSE; 
run;
quit;

	print y X b yhat e,bintervallb bintervalub, SSTO, SSE MSE; /*using spaces prints in one table, commas give it a separate table*/
	run;/*this run statement executes the print above*/
quit; 

