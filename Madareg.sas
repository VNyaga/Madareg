/*************************************************************************************************		  										
**																								**
**																								**
** TITLE:		Diagnostic Test Accuracy Meta-analysis 	 										**
**																								**
**																								**
** AUTHOR:		Victoria Nyaga																	**
**																								**
** ADAPTED FROM: Petra Macaskill and Yemisi Takwoingi											**
**																								**
**																								**
** DATE CREATED:	10/05/17																	**
** VERSION: 		1.0.0																		**
** DATE MODIFIED:																				**
** MODIFICATION:																				**
**																								**
**																								**
**																								**
** PURPOSE: 	To automate fitting of uni\bi-variate model for 								**
**				meta-analysis of diagnostic accuracy studies using PROC NLMIXED.				**
**				Explanatory variables (covariates) can be added to the models to 				**
**				produce separate effects on the summary measures of test accuracy.				**
**				Also, distributional assumptions of the random effects can be checked and 		**
**				predicted values of sensitivity and specificity, based on empirical Bayes 		**
**				estimates of the random effects, can be obtained for each study in the 			**
**				meta-analysis. The output from the analysis is presented in a Word document.	**  
**	  								 															**
** PARAMETERS:																					**
**   		%madareg(																			**
**				dtfile='text'	The path and name of the Excel or Stata file to import 			**
**								e.g. 'C:\Documents\DTA\Revman Test Data.xls'. 					**
**								The file extension (.xl, .csv or .dta) must be included. 		**
**				import=y/n		If = n, a data set must be provided with the dsname= option. 	**
**								The default is y.												**
**				dsname=data set	The input data set if no data import is required.				**
**				tech=quanew/newrap/trureg/nrridg/dbldog/congra/nmsimp							**
**								There are several optimization techniques available with 		**
**								Proc NLMIXED. No algorithm for optimizing general nonlinear 	**
**								functions exists that always finds the global optimum for 		**
**								a general nonlinear minimization problem in a reasonable 		**
**								amount of time. This parameter enables the user to select a 	**
**								technique as they would do if they were running NLMIXED 		**
**								directly. The default is tech=QUANEW. 							**
**								information and algorithm descriptions, see the SAS user 		**
**								documentation for NLMIXED.										**
**				ident=y/n		A potential problem with numerical maximization of the 			**
**								likelihood function is identifiability of model parameters. 	**
**								When this occurs, the likelihood will equal its maximum 		**
**								value at a set of parameter values instead of at a single 		**
**								point. To detect if there is a problem, you could try 			**
**								different initial values of the parameters and check for 		**
**								changes in parameter estimates or by examining the Hessian 		**
**								matrix at convergence.											**
**								If ident=y the Hessian matrix after optimization is 			**
**								produced and the eigenvalues of the Hessian are calculated 		**
**								(with values saved in _madareg_a_eigenvals_ / 					**
**								_madareg_cv_eigenvals_). At a true minimum, the eigenvalues 	**
**								will all be positive, i.e., positive definite. The default 		**
**								is y. The starting Hessian matrix is also produced because 		**
**								Proc NLMIXED option START is always used by madareg to 			**
**								output the gradient at the starting values.						**
**				tp=variable		The number of true positives. The default variable name is 		**
**								tp so that RevMan users or those who have named their 			**
**								variables accordingly do not need to specify this input 		**
**								parameter.														**
**				fp=variable		The number of false positives. The default variable name is	fp.	**																
**				fn=variable		The number of false negatives. The default variable name is fn.	**															
**				tn=variable		The number of true negatives. The default variable name is tn.	**
**				subject=variable																**	
**								This determines when new realizations of the random effects 	**
**								are assumed to occur. Proc NLMIXED assumes that a new 			**
**								realization occurs whenever the subject= variable changes 		**
**								from the previous observation, so the input data set is 		**
**								clustered according to this variable. The default variable 		**
**								name is study_id (as named in the RevMan 5 data export file)	**
**				cialpha=numeric	Specifies the alpha level for computing z statistics and 		**
**								confidence limits. The default is 0.05.							**
**				byvar=variable	This enables multiple analyses, i.e., consecutive calls 		**
**								to Proc NLMIXED for each test or group of studies in the 		**
**								data file. This may also be used to produce separate models 	**
**								using subsets of the data (subgroup analyses as in 				**
**								traditional meta-analysis) but be aware this is not 			**
**								recommended because you cannot formally test for a 				**
**								difference. A better approach is to use all the data and 		**
**								include the variable as a covariate in the model.				**
**				covariate(s)= variable(s)														**
**								Specifies a covariate(s) for inclusion in the model				**
**								(meta-regression). The covariates are separated by space.  	    **
**				cvref(s)='text'/numeric															**
**								This specifies the reference level of the covariate. If it 		**
**								is not specified, the reference level is selected based on 		**
**								the sort order. Sorting is done in ascending order by 			**
**								default and for descending specify sortcv=d.					**
**				sortcv=d/a		The sort order for the covariate. sortcv=d specifies 			**
**								descending order and a specifies ascending. The default is 		**
**								to sort in ascending order.										**
**				cvtype =cat/con	Type of covariate. Options are cat for categorical or con 		**
**								for continuous. If the parameter is not specified, the 			**
**								covariate is assumed to be categorical.							**
**				cveffect=se/sp/sesp																**
**								For the bivariate model, se specifies that the effect be 		**
**								assessed only on sensitivity while sp on specificity and 		**
**								sesp specifies effect on both sensitivity and specificity. 		**
**								Default is sesp.												**
**				cvsummorder(s) =stat/level														**
**								Specifies the ordering of items in the table of summary 		**
**								estimates for a model with covariate. If level is specified,	**
**								items are listed in the table according to covariate level. 	**
**								If stat is specified, items are listed according to summary 	**
**								statistic such that all levels of the covariate are grouped 	**
**								together for each statistic. The default is stat.				**
**				formatlr=y/n	For formatting the log likelihood difference and p-value 		**
**								obtained for the likelihood ratio test. If =y, then -2logL 		**
**								difference is formatted to 3 decimal places if it is greater	**
**								than or equal to 0.001 otherwise the exact value is 			**
**								reported. The p-value is formatted to 3 d.p. if less than or	**
**								equal to 0.001 and as <0.001 if less than 0.001. The default	**
**								is y.															**
**				mtitle=text		Title of the meta-analysis that is placed in the Word 			**
**								document. Default is 'Meta-analysis of diagnostic test 			**
**								accuracy studies'.												**
**								NOTE: no quotation marks allowed unlike some of the other 		**
**								text options.													**
**				tbpe=data set	Use parameters and starting values stored in the named 			**
**								table. The data set can be in either a narrow or wide form. 	**
**								The narrow-form data set contains the variables PARAMETER 		**
**								and ESTIMATE, with parameters and values listed as distinct 	**
**								observations. The wide-form data set has the parameters 		**
**								themselves as variables, and each observation provides a 		**
**								different set of starting values. 								**
**								Note: In this version of madareg, the data set should only 		**
**								contain the 5 basic parameters the bivariate model (msens, 		**
**								mspec, s2usens, s2uspec, covsesp). If there is a covariate, 	**
**								the starting values for additional parameters can be 			**
**								specified using cspa1 - cspa5, cset1 - cset5 and/or 			**
**								cpb1 - cpb5.													**
**				p1 - p5			These are the basic parameters and their starting values. 		**
**								There are five such parameters for the model. You can 		    **
**								either specify a single number e.g. p1= 2.5 or you can use 		**
**								the TO and BY keywords to specify a number list for a grid 		**
**								search e.g. p1 = -2 to 2 by 0.5. If you specify a grid of 		**
**								points, the objective function value at each grid point is 		**
**								calculated and the best (feasible) grid point is chosen as 		**
**								an initial point for the optimization process.					**
**								p1 = mean logit sensitivity, p2 = mean logit specificity, 		**
**								p3= variance of logit sensitivity, p4 = variance of logit 		**
**								specificity, p5 = covariance of logit sensitivity and 			**
**								specificity. The default values are:							**
**								p1= -2 to 4 by 1												**
**								p2= -2 to 4 by 1												**
**								p3= 0 to 1 by 0.2												**
**								p4= 0 to 1 by 0.2												**
**								p5= -1 to 1 by 0.2												**
**				cspa1 - cspa5	These specify starting 											**
**								values for additional specificity 								**
**								parameters e.g. cspa1 = 0 to 2 by 1. cspa1 - cspa5 indicates	**
**								a maximum of 5 parameters, i.e., a covariate with 6 levels. 	**
**								The default is 0 for any of the 5 parameters, i.e., cspa1=0,	**
**								cspa2 = 0 for a covariate with 3 levels.						**
**				cset1 - cset5	Starting values for sensitivity 								**
**								parameters. The maximum is 5, i.e., a covariate with 6 			**
**								levels,. The default is 0 for any of the 5 parameters, i.e.,	**
**								cset1 = 0, cset2 = 0 for a covariate with 3 levels.				**
**				randeffs=y/n	Produce table of empirical Bayes estimates of the random 		**
**								effects if = y. The default is n.								**
**				relax=y/n		Assume correlation is zero.The default is n.					**
**				predict=y/n		If =y, predictions are obtained using the estimated model, 		**
**								parameter estimates and empirical Bayes estimates of the 		**
**								random effects. Standard errors of prediction are computed 		**
**								using the delta method and the predicted values of logitp 		**
**								(stored in data sets prefixed with _logitp_ and _logitp_cv_)	**
**								are transformed to obtain predictions of sensitivity and 		**
**								specificity (stored in data sets prefixed with _predsesp_ 		**
**								and _predsesp_cv_). The default is n.							**
**				checkmod=y/n	If =y, produce histograms and normal probability plots of 		**
**								the empirical Bayes estimates of the random effects to 			**
**								check assumption of normality. The default is n.				**
**				debug=y/n		Debugging tool. If =y, displays the SAS statements that are 	**
**								generated by macro execution. The default is n.					**
**				logfile='text'	Path and file name to save the contents of the SAS log. 		**
**								Must add the .log extension. Contents of the log file are 		**
**								scanned and any errors found are stored in _madareg_errors, 	**
**								warnings in _madareg_warnings, and model failure messages 		**
**								generated by madareg in _madareg_modfail. The data set for 		**
**								the log contents is _madareg_log.								**
**				outfile='text'	Path and filename to save the contents of the SAS output 		**
**								window. The file name must have the .lst extension. This is 	**
**								especially useful if the analysis is expected to run for 		**
**								awhile because the output window will fill up and user input	**
**								is required before SAS can proceed. However, this is not 		**
**								the case if the output is being saved to a file.				**
**				keepds=all/some/log/none														**
**								Selectively keeps the data sets produced as output from the 	**
**								analyses. Option some is the default. With this option, 		**
**								data sets containing data from the Excel file are kept, 		**
**								including any data sets generated from the log file if a 		**
**								log file was specified. For the option log, only the data 		**
**								sets generated from the log file are kept. If option none is	**
**								specified, all data sets prefixed with _madareg_ are 			**
**								deleted. Option all keeps all output data sets from NLMIXED 	**
**								as well as two summary ones for covariate summary and 			**
**								relative measures of test accuracy. Data sets for 				**
**								predictions, random effects, the Hessian matrix and 			**
**								eigenvalues are also kept with options all and some if 			**
**								parameters have been specified for them.						**
**								madareg output data sets										**
**								All data from Excel file =_madareg_meta							**
**								Unique values of the BY variable = _madareg_variablename		**
**								Data set for level i of the BY variable = _madareg_dsi			**
**								Unique values of the covariate = _madareg_variablename			**
**								Predicted logitp for model without covariate=_madareg_logitp_i	**
**								Predicted logitp for model with covariate=_madareg_cv*_logitp_i	**
**								Predicted sensitivities and specificities for 					**
**								model without covariate = _madareg_predict_i					**
**								Predicted sensitivities and specificities for model with 		**
**								covariate = _madareg_cv*_predict_i								**
**								Relative estimates of accuracy measures for 					**
**								covariate = _madareg_cv*_relsummary_i							**
**								Summary estimates of accuracy measures for covariate = 			**
**								_madareg_cv*_statsummary_i										**
**								Eigenvalues for model without covariate =_madareg_a_eigenvals_	**
**								Eigenvalues for model with covariate =_madareg_cv*_eigenvals_	**
**								SAS NLMIXED output data sets are prefixed by madareg as follows:**
**								Model without covariate											**
**								Starting values =_madareg_a_sv_								    **
**								Parameters=_madareg_a_parms_									**
**								Parameter estimates=_madareg_a_pe_								**
**								Fit statistics=_madareg_a_fit_									**
**								Additional estimates=_madareg_a_addest_							**
**								Covariance matrix of additional estimates =_madareg_a_covaddest_**
**								Convergence status=_madareg_a_convgstat_						**
**								Final Hessian matrix=_madareg_a_hessian_						**
**								Model with covariate											**
**								Starting values = _madareg_cv*_sv_								**
**								Parameters=_madareg_cv*_parms_									**
**								Parameter estimates=_madareg_cv*_pe_							**
**								Fit statistics=_madareg_cv*_fit_								**
**								Additional estimates=_madareg_cv*_addest_						**
**								Covariance matrix of additional estimates=_madareg_cv*_covaddest_**
**								Convergence status=_madareg_cv*_convgstat_						**
**								Contrasts=_madareg_cv*_contrasts_								**
**								Final Hessian matrix=_madareg_cv*_hessian_						**
**								For the bivariate model there are 2 additonal tables, 			**
**								madareg_cv*_covparmest_ and madareg_cv*_covparmest_, for the 	**
**								covariance marix of parameter estimates. 						**
**				info=y/n		If =y, include details of some of the input parameters 			**
**								specified for the macro. The default is y.						**
**				incbasic=y/n	If = n then the output for the model with no covariate is 		**
**								suppressed. This may be useful where the model with no covariate**
**								has already been investigated and the parameters are no longer 	**
**								of interest for extraction to RevMan or in test comparisons 	**
**								where the covariate is test type. The default is y.				**
**				rfile='text'	Path and name of the Word document to save the result of the 	**
**								analyses. The file name must have the .rtf extension 			**
**								(rich text file).												**
**				model = RE/FE   Random or fixed effects model. If there are two studies, then 	**
**								the fixed-effect model is fitted.								**
**				graph = Y/N		Include the forest plot or not. Default is Y.					**
**				gtitle ='text' 		Title of the forest plot. 									**
**								Default is														**
**								'Forest plot of sensitivity and specificity and 95% CI'			**
**				cltype= AC/W/CP	How to compute the confidence intervals for the individual 		**
**								studies. AC is Agresti-coull, W is for Wilson/Score and 		**
**								CP (default) for Clopper-Pearson, the exact interval.			**
**				pmin =			A number in the interval [0,1] to indicate the minimum x-axis	**
**								tick-value.														**
**				pmax = 			A number in the interval [0,1] to indicate the maximum x-axis	**
**								tick-value.														**
**				gwidth = 		Number to indicate width of the forest plot, default is 800.	**
**				gheight = 		Number to indicate height of the forest plot, default is 800.	**
**				gdpi =  		Number to indicate the resolution of the forest plot, 			**
*								default is 250.													**
**				dp = 			Decimal points to be displayed on the forest plot. 				**
**								Default is 2.													**
**																								**
**	Example: %madareg(dtfile= 'C:\Data.xls', covariate=age_group,								**
**                    tech = newrap ,															**
**							rfile ="c:\Test\hsroc test data output.rtf");						**
**																								**
*************************************************************************************************/
	/* Declare global macro variables which will be used in some of the macros below */
	%global madareg nlevels bylevels estmuAcv estmuBcv estsenscv estspeccv estDOR 
		estRDOR estRelSens estRelSpec estLRpos estLRneg estAlpha estTheta 
		estBeta estMuAlevels estMuBlevels pms cvparamlist logitp contrasta 
		contrastt contrastb contrastse contrastsp dsprefixpe dsprefixae 
		dsprefixcov sub typem model;
		
%macro madareg(
	dtfile = , 
	import = ,
	dbmstype = ,
	dsname = , 
	tech = quanew, 
	ident = ,
	tp = TP, 
	fp = FP, 
	fn = FN, 
	tn = TN, 
	subject = study_id, 
	cialpha = 0.05, 
	byvar = , 
	covariate = , 
	cvref = , 
	sortcv = , 
	cvtype = , 
	cveffect = , 
	cvsummorder = , 
	formatlr = , 
	mtitle = 'Meta-analysis of diagnostic accuracy studies', 
	tbpe =, 
	p1=, p2=, p3=, p4=, p5=, 
	cspa1=, cspa2=, cspa3=, cspa4=, cspa5=, 
	cset1=, cset2=, cset3=, cset4=, cset5=, 
	randeffs=, 
	predict=, 
	checkmod=, 
	debug=, 
	logfile=, 
	outfile=, 
	keepds=, 
	relax= N,
	info=, 
	incbasic=, 
	rfile=,
	graph = ,
	gtitle = 'Forest plot of sensitivity and specificity and 95% CI',
	cltype=,
	pmin=,
	pmax =,
	gwidth=800,
	gheight=800,
	gdpi = 250,
	dp=2,
	model = RE);

	%put %str(*******************************************************);
	%put %str(*    		                                            *);
	%put %str(* 	META-ANALYSIS OF DIAGNOSTIC ACCURACY STUDIES 	*);
	%put %str(*											            *);
	%put %str(*******************************************************);
		
	%if &covariate~=%str() %then %do;
		%global ncovariates covariates;
		%let covariates = &covariate;
		%let ncovariates = %sysfunc(countw(&covariate));

		%do c = 1 %to &ncovariates;
			%global covariate&c cvtype&c nlevels&c;
			
			%let covariate&c = %sysfunc(scanq(&covariate, &c));
			%let cvtype&c = %sysfunc(scanq(&cvtype, &c));

			%global sortcv&c;
			%if sortcv ~= %str() %then 				
				%let sortcv&c = %sysfunc(scanq(&sortcv, &c));
			%else
				%let sortcv&c = ;

			%global cvref&c;
			%if &cvref ~= %str() %then 				
				%let cvref&c = %sysfunc(scanq(&cvref, &c));
			%else
				%let cvref&c =;

			%global cvsummorder&c;
			%if &cvsummorder ~= %str() %then 			
				%let cvsummorder&c = %sysfunc(scanq(&cvsummorder, &c));
			%else 
				%let cvsummorder&c = ;
			
			%global cveffect&c;
			%if &cveffect ~= %str() %then
				%let cveffect&c = %sysfunc(scanq(&cveffect, &c));
			%else 
				%let cveffect&c = ;
		%end;
	%end;

	
	/* initialise macro variable as zero
	use this macro variable to track execution of macros within madareg.
	return 0 if unsuccessful otherwise 1*/
	%let madareg=0;

	/* prefixes for data sets generated from analysis with covariate(s) */

	%let sub=_ds;

	/* If importing data, check if the external data file exists 
	if it exists check the last 4 characters for file extensions .xls or .csv  or .dta
	and set the dbms type */
	%if %upcase(&import)~= N and &dtfile ~= %str() %then %do;
		%if %sysfunc(fileexist(&dtfile)) ~=1 %then %do;
			%put %str(FILE ERROR: File &dtfile not found! Please check the path, file name and extension.);
			%put %str(Execution of madareg terminated.);
			%return;
		%end;
		%else %do;
			%let len=%sysfunc(lengthn(&dtfile));
			%let ext=%sysfunc(substr(&dtfile,&len-4,4));
			%if &ext ~= %str() %then %do;
				%if %upcase("&ext")=".XLS" %then %let dbmstype=excel; 
				%else %if %upcase("&ext")=".CSV" %then %let dbmstype=csv;
				%else %if %upcase("&ext")=".DTA" %then %let dbmstype=dta;
				%else %do;
					%put %str(FILE ERROR: &dtfile extension must be .csv, .xls or .dta. Data import failed!);
					%put %str(Execution of madareg terminated.);
					%return;
				%end;
			%end;
			%else %do;
				%put %str(FILE ERROR: Failed to get the file extension! Please check file name and extension (.csv or .xls depending on the file type).);
				%put %str(Execution of madareg terminated.);
				%return;
			%end;
		%end;
	%end;
	%else %if %upcase(&import)~=N and &dtfile = %str() %then %do;
		%put %str(INPUT ERROR: Excel file (.csv or .xls) not specified.);
		%put %str(If data import is required a file name must be specified otherwise specify option IMPORT=N and provide a data set (DSNAME=).);
		%put %str(Execution of madareg terminated.);
		%return;
	%end;

	/* data set name is required if no import */
	%if %upcase(&import)= N and &dsname = %str() %then %do;
		%put %str(INPUT ERROR: A data set must be specified for option DSNAME if data import from Excel is not required.);
		%put %str(Execution of madareg terminated.);
		%return;
	%end;
	/* if data set is specified check that it exists */
	%else %if &dsname ~= %str() %then %do;
		%if not %sysfunc(exist(&dsname)) %then %do;
			%put %str(DATA SET ERROR: The data set &dsname does not exist.);
			%put %str(Check that you have specified the correct data set name and library.);
			%put %str(Execution of madareg terminated.);
			%return;
		%end;
	%end;

	/* If a log file name is specified, send output that would normally go to the Log Window 
	to the file and if the file already exists replace it.
	Also, check the extension is correctly specified (.log) */
	%if &logfile ~= %str() %then %do;
		%let len=%sysfunc(lengthn(&logfile));
		%let ext=%sysfunc(substr(&logfile,&len-4,4));
		%if %upcase("&ext")=".LOG" %then %do;
			filename logfile &logfile; 		
			proc printto log = logfile new;
		%end;
		%else %do;
			%put %str(FILE ERROR: &logfile extension must be .log!);
			%put %str(Execution of madareg terminated.);
			%return;			
		%end; 
	%end;

	/* If an output file name is specified, send output that would normally go to the Output Window 
	to the file and if the file already exists replace it.
	Also, check the extension is correctly specified (.lst) */
	%if &outfile ~= %str() %then %do;
		%let len=%sysfunc(lengthn(&outfile));
		%let ext=%sysfunc(substr(&outfile,&len-4,4));
		%if %upcase("&ext")=".LST" %then %do;
			filename outfile &outfile; 		
			proc printto print = outfile new;
		%end;
		%else %do;
			%put %str(FILE ERROR: &outfile extension must be .lst!);
			%put %str(Execution of madareg terminated.);
			%return;			
		%end; 
	%end;

	%if &byvar~=%str() %then %do;
		/* need to check that the name of the data set _madareg_&byvar
		will not exceed 32, the maximum length for a sas sata set name*/
		%let bylen= %sysfunc(lengthn(&byvar));
		%let dslen= %eval(&bylen + 9);
		%if &dslen > 32 %then %do;
			%put %str(madareg ERROR: The length (&dslen) of the name of the madareg generated data set, _madareg_&byvar, exceeds 32 which is the maximum for a SAS data set name!);
			%put %str(Please shorten the name of the BY= variable.);
			%put %str(Execution of madareg terminated.);
			%return;	
		%end;
		%else %if %sysfunc(exist(_madareg_&byvar)) %then %do;
			data _null_;
	   			set _madareg_&byvar nobs = numobs; 
				call symput('numobs',numobs);
			run;
			%let bylevels=&numobs;
		%end;
		%else %let bylevels=0;
	%end;
	%else %let bylevels=1;

	%if &covariate ~= %str() %then %do;
		%do c=1 %to &ncovariates;
			/* need to check that the name of the data set _madareg_&covariate&c
			will not exceed 32, the maximum length for a sas sata set name*/
			%let cvlen= %sysfunc(lengthn(&covariate&c));
			%let dslen= %eval(&cvlen + 9);
			%if &dslen > 32 %then %do;
				%put %str(madareg ERROR: The length (&dslen) of the name of the madareg generated data set, _madareg_&covariate&c, exceeds 32 which is the maximum for a SAS data set name!);
				%put %str(Please shorten the name of the COVARIATE = &covariate&c.);
				%put %str(Execution of madareg terminated.);
				%return;	
			%end;
			%else %if %sysfunc(exist(_madareg_&covariate&c)) %then %do;
				data _madareg_&covariate&c;
		   			set _madareg_&covariate&c nobs=num; 
					call symput('num',num);
				run;
				%let nlevels&c=%eval(&num-1);	
			%end;
			%else %let nlevels&c=0;
		%end;
	%end;

	/* clear output data sets from previous analysis if any */	
	%keepds(mode=none);
	run;

	/* check if macro successfully executed */ 
	%if &madareg=0 %then %goto continueprog;

	/* if user wishes to debug macro execution, turn on
	  macro debug system options */
	%if %upcase(&debug)=Y %then %do;
		options mprint;
	%end;

	/* Import data from Excel file and create appropriate SAS data set */	
	%setupdata(dtfile = &dtfile, 
		import = &import, 
		dsname = &dsname,
		dbmstype = &dbmstype,
		tp = &tp,fp = &fp, fn = &fn, tn = &tn, 
		byvar = &byvar, 
		covariate = &covariate);
	run;
	
	/*Define the type of model*/
	%if %upcase(&model) = FE %then
		%let relax = Y;
		
	%if &relax = N %then 
		%let typem = Bivariate;
	%else
		%let typem = Univariate;

	/* check if macro successfully executed */
	%if &madareg=0 %then %goto continueprog;

	/* output Hessian matrix at convergence if ident=y*/
	%if %upcase(&ident)= N %then %let hessout= ;
   	%else %if %upcase(&ident) ~=N %then %let hessout=hess;

	/* resume madareg execution here if any of the other macros
	failed to execute correctly, i.e, returned madareg = 0. */
	%continueprog:

	/* Fit the BRMA */

	%metab(subject=&subject, 
			cialpha=&cialpha, 
			tech=&tech, 
			hessout=&hessout,
			byvar=&byvar, 
			covariate=&covariate, 
			cvtype=&cvtype, 
			cveffect=&cveffect,  
			mtitle=&mtitle, 
			tbpe=&tbpe, 
			p1=&p1, p2=&p2, p3=&p3, p4=&p4, p5=&p5, 
			cset1=&cset1, cset2=&cset2, cset3=&cset3, cset4=&cset4, cset5=&cset5, 
			cspa1=&cspa1, cspa2=&cspa2,	cspa3=&cspa3, cspa4=&cspa4, cspa5=&cspa5,
			randeffs=&randeffs, 
			predict=&predict, 
			checkmod=&checkmod, 
			relax=&relax,
			model = &model);
	run;


	/* check if macro successfully executed */
	%if &madareg=0 %then %goto continueprog;

	/* parameter identifiability check using the Hessian matrix*/
	%if %upcase(&ident) ~= N %then %do;

		%do k = 1 %to &bylevels;

			data _madareg_a_hess_&k (drop= Row Parameter);
				set _madareg_a_hessian_&k;
			run;

			proc iml;
				use _madareg_a_hess_&k; /* input data set */
				read all into mx; /* read all observations into matrix mx */
				_m_eg = eigval(mx); /* compute eigenvalues */
				/* create a SAS data set from the resulting matrix */
				create _madareg_a_eigenvals_&k from _m_eg[colname='Eigenvalue']; 
   				append from _m_eg;
			quit;

			proc datasets nodetails nolist;
				delete _madareg_a_hess_&k;
			run;
			quit;
			%if &covariate ~=%str() %then %do;
				%if &ncovariates > 1 %then 
					%let d = %eval(&ncovariates + 1);
				%else 
					%let d = %eval(&ncovariates);

				%do c = 1 %to &d;
					data %sysfunc(cats(_madareg_hess_cv&c, _&k)) (drop= Row Parameter);
						set %sysfunc(cats(_madareg_cv&c, _hessian_&k));
					run;

					proc iml;
						use %sysfunc(cats(_madareg_hess_cv&c, _&k));
						read all into mxcv;
						_m_egcv = eigval(mxcv); 
						/* create a SAS data set from the resulting matrix */
						create %sysfunc(cats(_madareg_eigenvals_cv&c, _&k)) from _m_egcv[colname='Eigenvalue']; 
	   					append from _m_egcv;
					quit;

					proc datasets nodetails nolist;
						delete %sysfunc(cats(_madareg_hess_cv&c, _&k));
					run;
					quit;
				%end;
			%end;
		%end;
	%end;
		/* output results of the analysis to Word */
	%printtodoc(
		covariate=&covariate, 
		cvtype=&cvtype, 
		cveffect=&cveffect, 
		formatlr=&formatlr, 
		byvar=&byvar, 
		mtitle=&mtitle, 
		info=&info, cialpha=&cialpha,
		subject=&subject, 
		predict=&predict, 
		dtfile=&dtfile, 
		checkmod=&checkmod, 
		incbasic=&incbasic, 
		rfile=&rfile,
		relax = &relax,
		graph = &graph,
		cltype = &cltype,
		gtitle = &gtitle,
		pmin= &pmin,
		pmax = &pmax,
		gwidth = &gwidth,
		gheight = &gheight,
		gdpi = &gdpi,
		dp = &dp,
		model = &model
		);
	run;
	/* check if macro successfully executed */
	%if &madareg=0 %then %goto continueprog;

	%if %upcase(&keepds) ~= ALL %then %do;
		%keepds(mode=&keepds, 
			covariate=&covariate, 
			byvar=&byvar, 
			randeffs=&randeffs, 
			checkmod=&checkmod);
		run;
	%end;


	/* if debugging options were switched on earlier, turn off
	  macro debug system options at the end*/
	%if %upcase(&debug)=Y %then %do;
		options nomprint;
	%end;

	/* Create tables of errors, warnings and model failure messages 
	based on the log output sent to file */
	%if &logfile ~= %str() %then %do;	

		/*change the destination for the log back to the SAS Log Window */
		proc printto log = log;
		run;

		/* read full log into _tb_log, any errors into _tb_errors, any warnings into _tb_warnings 
		and any programmer generated model failure messages to _tb_modfail. */ 
		data _madareg_log(keep=logline) _madareg_errors _madareg_warnings _madareg_modfail; 
			infile logfile missover; 
	      	length logline $256 code macroname $20 tbloglineref 4; 
	      	retain code macroname; 
	      	input; 
	      	if index(_infile_,'0D'x) then logline=scan(_infile_,1,'0D'x); 
	      	else                          logline=_infile_; 
	      	logline = translate(logline,' ','%'); 
			tbloglineref=_n_;
	      	if index(logline,'MPRINT(') then macroname=scan(logline,2,'()'); 
	      	if index(logline,':') then code=scan(logline,1,':'); 
	      	else if substr(logline,1,5) ne ' ' then code=scan(logline,1,' '); 
	      	output _madareg_log; 
	      	if index(code,'ERROR') =1 and logline ne ' ' then output _madareg_errors; 
			else if index(code,'WARNING') =1 and logline ne ' ' then output _madareg_warnings;
			else if index(code,'MODEL FAILURE') =1 and logline ne ' ' then output _madareg_modfail; 
	   	run; 		
	%end;

	/*change the destination for the output back to the SAS Output Window */
	%if &outfile ~= %str() %then %do;
		proc printto print = print;
		run;
	%end;

	%if &madareg = 0 %then %do;
		%put %str(Error encountered. Execution of madareg terminated.);
		%return;
	%end; 

%mend;


/********************************************************************
*																	*
* 						DELETE DATA SETS							*							
*																	*
********************************************************************/

%macro keepds(mode=, 
	covariate=, 
	byvar=, 
	randeffs=, 
	checkmod=);

	/* set global variable madareg to 0 to begin with */
	%let madareg=0;

	/* don't wish to keep any of the generated data sets */
	%if %upcase(&mode) = NONE %then %do;
		proc datasets nodetails nolist;
			delete _madareg_: ;
		run;
		quit;
		%let madareg = 1;
		%return;
	%end;

	/* wish to keep only data sets generated from the log file */
	%if %upcase(&mode) = LOG %then %do;
		proc datasets nodetails nolist;
			delete _madareg_meta 
			delete _madareg_&byvar
			delete _madareg_&covariate
			delete _madareg_a_: 
			delete _madareg_cv_: 
			delete _madareg_ds:;
		run;
		quit;
		%let madareg = 1;
		%return;
	%end;

	%do k=1 %to &bylevels;

		%if %upcase(&randeffs) ~= Y  %then %do;
			%if %sysfunc(exist(_madareg_a_randeffs_&k)) %then %do;
				proc datasets memtype=data nodetails nolist; 
					delete _madareg_a_randeffs_&k; 
				run;
				quit;
			%end;
		%end;

		%if %upcase(&predict) ~= Y  %then %do;
			%if %sysfunc(exist(_madareg_a_logitp_&k)) %then %do;
				proc datasets memtype=data nodetails nolist; 
					delete _madareg_a_logitp_&k
							_madareg_a_predict_&k; 
				run;
				quit;
			%end;
		%end;
		
		%if %sysfunc(exist(_madareg_a_addest_&k)) and %upcase(&mode) ~= ALL %then %do;
			proc datasets memtype=data nodetails nolist; 
				delete _madareg_a_addest_&k
						_madareg_a_covaddest_&k; 
			run;
			quit;
		%end;


		%if %sysfunc(exist(_madareg_a_covparmest_&k)) and %upcase(&mode) ~= ALL %then %do;
			proc datasets memtype=data nodetails nolist; 
				delete _madareg_a_covparmest_&k; 
			run;
			quit;
		%end;		

		%if %sysfunc(exist(_madareg_a_convgstat_&k)) and %upcase(&mode) ~= ALL %then %do;
			proc datasets memtype=data nodetails nolist; 
				delete 	_madareg_a_convgstat_&k
						_madareg_a_fit_&k
						_madareg_a_parms_&k
						_madareg_a_pe_&k
						_madareg_a_sv_&k; 
			run;
			quit;
		%end;	
		
		
		%if &covariate ~=%str() %then %do;
		
			%if &ncovariates > 1 %then 
				%let n = %eval(&ncovariates + 1);
			%else
				%let n = 1;
				
			%do t=1 %to &n;
					%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&t, _addest_&k)))) and %upcase(&mode) ~= ALL %then %do;
					proc datasets memtype=data nodetails nolist; 
						delete %sysfunc(cats(_madareg_cv&t, _addest_&k))
								%sysfunc(cats(_madareg_cv&t, _covaddest_&k))
								%sysfunc(cats(_madareg_cv&t, _contrasts_&k))
								%sysfunc(cats(_madareg_cv&t, _statsummary_&k))
								%sysfunc(cats(_madareg_cv&t, _relsummary_&k));
					run;
					quit;
				%end;

				%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&t, _covparmest_&k)))) and %upcase(&mode) ~= ALL %then %do;
					proc datasets memtype=data nodetails nolist; 
						delete %sysfunc(cats(_madareg_cv&t, _covparmest_&k)); 
					run;
					quit;
				%end;

				%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&t, _convgstat_&k)))) and %upcase(&mode) ~= ALL %then %do;
					proc datasets memtype=data nodetails nolist; 
						delete %sysfunc(cats(_madareg_cv&t, _convgstat_&k))
								%sysfunc(cats(_madareg_cv&t, _fit_&k
								%sysfunc(cats(_madareg_cv&t, _parms_&k))
								%sysfunc(cats(_madareg_cv&t, _pe_&k))
								%sysfunc(cats(_madareg_cv&t, _sv_&k)); 
					run;
					quit;
				%end;

				%if %upcase(&randeffs) ~= Y  %then %do;
					%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&t, _randeffs_&k)))) %then %do;
						proc datasets memtype=data nodetails nolist; 
							delete %sysfunc(cats(_madareg_cv&t, _randeffs_&k; 
						run;
						quit;
					%end;
				%end;

				%if %upcase(&predict) ~= Y %then %do;
					%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&t, _logitp_&k)))) %then %do;
						proc datasets memtype=data nodetails nolist; 
							delete %sysfunc(cats(_madareg_cv&t, _logitp_&k))
									%sysfunc(cats(_madareg_cv&t, _predict_&k)); 
						run;
						quit;
					%end;
				%end;

			%end;			
		%end;
		
	%end;

	quit;

	/* if macro execution is not terminated prematurely due to an error of 
	some sort then set global variable madareg to 1, i.e., successful execution so far */
	%let madareg=1;

%mend;



/********************************************************************
*																	*
* 		IMPORT DATA	FROM EXCEL FILE	AND MODIFY FOR ANALYSIS			*			
*																	*
********************************************************************/

%macro setupdata(dtfile=, 
	import=, 
	dsname=, 
	tp=, fp=, fn=, tn=, 
	dbmstype=, 
	byvar=, 
	covariate=);

	/* set global variable madareg to 0 to begin with */
	%let madareg=0;

	/* if import is not NO and we have an Excel file, then import data */
	%if %upcase(&import) ~=N and &dtfile ~= %str() %then %do;
		%put %str(*******************************************************);
		%put %str(IMPORT DATA FROM %upcase(&dbmstype) FILE AND MODIFY FOR ANALYSIS);	
		%put %str(*******************************************************);
		
		%put %str(-----------------------------------------------------------------------------);
		%put %str(DATA STEP 1: Import data from %upcase(&dtfile));
		%put %str(-----------------------------------------------------------------------------);

		/* run proc import to import data from excel/csv file or Stata data file*/	
		%if  %upcase(&dbmstype) = DTA %then %do;	
			proc import 	
				out= _madareg_meta		/* Name of the SAS data to create */
				datafile= &dtfile 		/* Name of Stata file to import */
				dbms=&dbmstype replace;
			run;
		%end;
		%else %do;
			proc import 	
				out= _madareg_meta		/* Name of the SAS data to create */
				datafile= &dtfile 		/* Name of Excel file to import */
				dbms=&dbmstype replace;
				getnames=yes; 			/* Generate variable names from the first row of data in the Excel file */
				guessingrows = max;
			run;
		%end;

		%if &syserr ~=0 %then %do;
			%put %str(ERROR: Failed to import data from %upcase(&dtfile). If the file is open, please close and rerun the macro.);
			%put %str(Execution of madareg terminated.);
			%return;
		%end;
		%else %let dsname=_madareg_meta;
	%end;
			
	%put -------------------------------------;
	%put DATA STEP 2: Modify data for analysis;
	%put -------------------------------------;

	/* Modify the data set in order to create 2 records 
	for each study, one for sensitivity and the other for specificity */
	%let numobs=0;
	
	data &dsname;
		set &dsname nobs=numobs;
		call symput('numobs', numobs);
		if &tp=0 or &fp=0 or &fn=0 or &tn=0 then do;
			zctp=&tp+0.5;
			zcfp=&fp+0.5;
			zcfn=&fn+0.5;
			zctn=&tn+0.5;
		end;
		else do;
			zctp=&tp;
			zcfp=&fp;
			zcfn=&fn;
			zctn=&tn;
		end;
		sensit=zctp/(zctp+zcfn);
		specit=zctn/(zctn+zctp);
		logit_sens=log(sensit/(1-sensit));
		var_logit_sens=1/(sensit*(1-sensit)*(zctp+zcfn));
		logit_spec=log(specit/(1-specit));
		var_logit_spec=1/(specit*(1-specit)*(zctn+zcfp));
	run;
	
	data _madareg_tbmeta;
		set &dsname;
		n=&tp+&fn; 					/* Number with disease, sensitivity */
		sens=1; spec=0; true=&tp; 	/* Set this up for bivariate model */
		logit=logit_sens;
		var_logit=var_logit_sens;
		rec+1;
		output;
		n=&tn+&fp; 					/* Number without disease, specificity */
		sens=0; spec=1; true=&tn; 	/* Set this up for bivariate model */
		logit=logit_spec;
		var_logit=var_logit_spec;
		rec+1;
		output;
	run;
	

	
	%if &numobs < 3 %then %do;
		%let model = FE;
		%let relax = Y;
	%end;
	
	/* Create dummy variables for the specified categorical covariate(s) */
	%if &covariate~=%str() %then %do;
		%do c = 1 %to &ncovariates;
			%if %upcase(&&cvtype&c)~= CON %then %do;
				%put %str(Create dummy variables for covariate %upcase(&&covariate&c));

				/* A: 
				Get unique values of the covariate into the data set tbmeta_covariatename
				Sort covariate - A for ascending, D for descending */
				%if %upcase(&&sortcv&c) = D %then %let sortdesc&c = descending;
				%else %let sortdesc&c = ;
				proc sort data=_madareg_tbmeta out= _madareg_&&covariate&c nodupkey; 
					by &&sortdesc&c &&covariate&c;
				run;

				/* B: 
				The macro variable nlevels will contain the number of levels of 
				the covariate. If a reference level is given for the covariate 
				then set cvlevels value to 0 otherwise use only the sort order to 
				generate all values.*/
				%if &&cvref&c=%str() %then %do;
					data _madareg_&&covariate&c;
						set _madareg_&&covariate&c end = maxn;			
						cvlevels&c =_n_-1;
						if maxn then call symput('nlevels', put(cvlevels&c, 3.)); 
					run;
					%let nlevels&c = &nlevels;
				%end;
				%else %do;

					/* check the given reference level exists in the data set */
					%let numobs=0;

					data _tmp_;
						set _madareg_&&covariate&c;			
						where &&covariate&c="&&cvref&c"; 
					run;
					data _null_;
						set _tmp_ nobs=numobs;
						call symput('numobs',numobs);
					run;

					%if %sysfunc(exist(_tmp_)) %then %do;			
						proc datasets memtype=data nodetails nolist; 
							delete _tmp_; 
						run;
						quit;						
					%end;

					%if &numobs > 0 %then %do;
						data _madareg_&&covariate&c;
							set _madareg_&&covariate&c;
							if &&covariate&c="&&cvref&c" then _cv_reference_&c=1;
							else _cv_reference_&c = 0;
						run;

						proc sort data=_madareg_&&covariate&c;
							by descending _cv_reference_&c;
						run;

						data _madareg_&&covariate&c;
							set _madareg_&&covariate&c end=maxn;
							if _cv_reference_&c =1 then cvlevels&c=0;
							else cvlevels&c =_n_-1;
							if maxn then call symput("nlevels&c", put(cvlevels&c, 3.)); 
						run;
					%end;
					%else %do;
						%put %str(ERROR: The reference level &&cvref&c specified for option CVREF does not exist in the data set.);
						%let madareg=0;
						%return;
					%end;
				%end;

				/* C:
				Finally create the dummy variables for the covariate 
				Sort each data set then merge them and generate values 
				for each dummy variable */
				proc sort data = _madareg_tbmeta;
					by &&sortdesc&c &&covariate&c;
				run;

				proc sort data = _madareg_&&covariate&c;
					by &&sortdesc&c &&covariate&c;
				run;

				data _madareg_tbmeta;
					merge _madareg_tbmeta _madareg_&&covariate&c;
					by &&sortdesc&c &&covariate&c;
				run;

				%do I = 0 %to &&nlevels&c;
					%let cv=%str(cv&c&I);
					data _madareg_tbmeta;
						set _madareg_tbmeta;
						if cvlevels&c = &I then &cv = 1;
						else &cv = 0;
					run;
				%end;
			%end;
			%else %if %upcase(&&cvtype&c) = CON %then %do;
				/* For continuous covariate, treat as a categorical variable with 2 levels 
				for the purpose of producing statements */
				%let nlevels&c = 1;
				data _madareg_tbmeta;
					set _madareg_tbmeta;
					cv&c = &&covariate&c;
				run;
			%end;
		%end;
	%end;

	/* For multiple/subgroup analyses using subsets of the data, 
	separate models are required so create separate data sets. 
	The BY statement of PROC NLMIXED could have been used instead 
	thereby eliminating the need to do this. However, doing 
	it this way means in future we can have separate starting values 
	and/or covariate for each analysis which would not otherwise be  
	possible. */
	%if &byvar ~=%str() %then %do;
		%put ----------------------------------------------;
		%put DATA STEP 3: Modify data for multiple analyses;
		%put ----------------------------------------------;

		/* A: 
		Create a duplicate of tbmeta as tbmeta_by */
		data tbmeta_by;
   			set _madareg_tbmeta;			
		run;

		/* B: 
		Get unique values of the by variable */
		proc sort data=tbmeta_by out= _madareg_&byvar nodupkey; 
			by &byvar;
		run;

		/* C: 
		The macro variable bylevels will contain the number of levels of 
		the by variable */
		data _madareg_&byvar;
   			set _madareg_&byvar end = maxn;			
			bylevels =_n_;
			if maxn then call symput('bylevels', put(bylevels, 3.)); 
		run;

		/* D: 
		Create a data set for each level of the by variable */
		proc sort data = tbmeta_by;
			by &byvar;
		run;

		proc sort data = _madareg_&byvar;
			by &byvar;
		run;

		data tbmeta_by;
   			merge tbmeta_by _madareg_&byvar;
   			by &byvar;
   		run;

		%do I = 1 %to &bylevels;
			data _madareg_ds&I;
				set tbmeta_by;
				where bylevels = &I;
			run;
		%end;
	%end;
	/* if no BY variable set bylevels as 1 so that only a set of 
	analyses is run. Also create a copy of the _madareg_tbmeta data set 
	as _madareg_ds1. Doing this just so that it fits in with the loop 
	for running the analyses */
	%else %if &byvar=%str() %then %do;
		%let bylevels = 1;
		data _madareg_ds1;
			set _madareg_tbmeta;
		run;
	%end;

	%if %sysfunc(exist(_madareg_tbmeta)) %then %do;
		proc datasets memtype=data nodetails nolist; 
			delete _madareg_tbmeta; 
		run;
		quit;
	%end;

	%if %sysfunc(exist(tbmeta_by)) %then %do;
		proc datasets memtype=data nodetails nolist; 
			delete tbmeta_by; 
		run;
		quit;
	%end;

	quit;

	/* if macro execution is not terminated prematurely due to an error of 
	some sort then set global variable madareg to 1, i.e., successful execution so far */
	%let madareg=1;
	
%mend;

/********************************************************************
*																	*
* 				UNI\BI-VARIATE ANALYSIS WITH NO COVARIATE			*
*																	*
********************************************************************/


%macro bivarnocv(data=, 
	tech=, 
	hessout=, 
	cialpha=, 
	subject=, 
	outreffs=, 
	pms=, 
	byname=, 
	modelnum=, 
	predict=,
	relax = ,
	model = );

	/* set global variable madareg to 0 to begin with */
	%let madareg=0;

	%if %upcase(&predict) = Y %then %let predictlogit= %str(predict logitp out=_madareg_a_logitp_&modelnum);
	%else %let predictlogit= ;

	%if &bylevels > 1 %then %let
		proctitle = %str("&typem analysis &modelnum without covariate - &byname");
	%else %let proctitle = %str("&typem analysis without covariate - &byname");

	/*ODS OUTPUT */
	ods output StartingValues=_madareg_a_sv_&modelnum
		Parameters=_madareg_a_parms_&modelnum
		ParameterEstimates=_madareg_a_pe_&modelnum
		FitStatistics=_madareg_a_fit_&modelnum
		AdditionalEstimates=_madareg_a_addest_&modelnum
		CovMatAddEst=_madareg_a_covaddest_&modelnum
		CovMatParmEst=_madareg_a_covparmest_&modelnum
		ConvergenceStatus=_madareg_a_convgstat_&modelnum;

	%if &hessout ~=%str() %then %do;
		ods output Hessian=_madareg_a_hessian_&modelnum;
	%end;
	
	%if %upcase(&relax) = N %then %do;
		%let covsesp = covsesp;
		
		/*Estimate correlation of expected 	logit(sensitivity)and logit(specificity)*/
		%let estCorr = estimate "Corr(logits)" &covsesp/(SQRT(exp(es2usens))*(SQRT(exp(s2uspec))));
	%end;
	%else %if %upcase(&relax) = Y or %upcase(&model) = 	FE %then %do;
		%let covsesp = 0;
		%let estCorr = ;
	%end;
	
	%if &model = RE %then %do;
		%let sse = %str(s2usens = exp(es2usens)); 
		%let ssp = %str(s2uspec = exp(es2uspec));
		%let logitp = %str(logitp = (msens + usens)*sens + (mspec + uspec)*spec);
		%let randomstatement = %str(random usens uspec ~ normal([0,0],[s2usens, &covsesp, s2uspec]) subject=&subject &outreffs);
		
		/* convert to HSROC parameterisation */
		/* theta is the threshold parameter, lambda is the accuracy parameter*/				
		/* beta is the shape parameter*/
		%let estsse = estimate 's2usens' exp(es2usens);
		%let estssp = estimate 's2uspec' exp(es2uspec);
		%let estlambda = %str(estimate 'Lambda' (exp(es2uspec)/exp(es2usens))**0.25 * msens  + (exp(es2usens)/exp(es2uspec))**0.25 * mspec) ;
		%let esttheta = %str(estimate 'Theta' 0.5* ( (exp(es2uspec)/exp(es2usens))**0.25 * msens - (exp(es2usens)/exp(es2uspec))**0.25 * mspec )) ;
		%let estbeta = %str(estimate 'beta' log( sqrt(exp(es2uspec)/exp(es2usens)) ) );
		%let estvaraccu = %str(estimate 'Var(accuracy)'   2*( sqrt(exp(es2usens)*exp(es2uspec)) + &covsesp) );
		%let estvarthresh = %str(estimate 'Var(threshold)' 0.5*( sqrt(exp(es2usens)*exp(es2uspec)) - &covsesp) );
	%end;
	%else %do;
		%let sse = ; 
		%let ssp = ;
		%let logitp = %str(logitp = msens*sens + mspec*spec);
		%let randomstatement =;
		%let estsse = ;
		%let estssp = ;
		%let estlambda = ;
		%let esttheta =  ;
		%let estbeta = ;
		%let estvaraccu =  ;
		%let estvarthresh =  ;
	%end;
	
	proc nlmixed data=&data cov ecov df=1000 start alpha=&cialpha &hessout tech=&tech;
		title &proctitle;
		parms &pms /Best=5;/*initial values for analysis*/
		&sse;
		&ssp;
		&logitp;
		p = exp(logitp)/(1+exp(logitp));
		model true ~ binomial(n,p);
		&randomstatement;

		&predictlogit;
		&estsse ;
		&estssp ;
		&estCorr;			

		/* estimates of average operating point */
		/*estimate 'sensitivity' exp(msens)/(1+exp(msens));
		estimate 'specificity' exp(mspec)/(1+exp(mspec));*/
		estimate 'logDOR' log((exp(msens)/(1+exp(msens)))/(1-(exp(msens)/(1+exp(msens))))/
				((1-(exp(mspec)/(1+exp(mspec))))/(exp(mspec)/(1+exp(mspec)))));
		estimate 'logLR+' log((exp(msens)/(1+exp(msens)))/(1-(exp(mspec)/(1+exp(mspec)))));
		estimate 'logLR-' log((1-(exp(msens)/(1+exp(msens))))/(exp(mspec)/(1+exp(mspec))));

		&estlambda;
		&esttheta;
		&estbeta;
		&estvaraccu;
		&estvarthresh;
	run;
	quit;

	/* if macro execution is not terminated prematurely due to an error of 
	some sort then set global variable madareg to 1, i.e., successful execution so far */
	%let madareg=1;

%mend;

/********************************************************************
*																	*
* 		CREATE STRING OF PARAMETERS AND THEIR STARTING VALUES 		*
*		FOR UNI/BI - VARIATE MODEL FROM A PARAMETER ESTIMATES TABLE			*							
*																	*
********************************************************************/

%macro createbpars (tbpe=, 
					reffs=, 
					relax=,
					model = );

	/* set global variable madareg to 0 to begin with */
	%let madareg=0;

	%if %sysfunc(exist(&tbpe)) %then %do;

		/* if we do not have parameter estimates for the random effects then do this */
		%if %upcase(&reffs) = N %then %do;
			data _null_;
			   	set &tbpe;
				select (Parameter);
						when ('msens')
							do;
							call symput('msens', put(Estimate, 5.2));
							end;
						when ('mspec')
							do;
							call symput('mspec', put(Estimate, 5.2));
							end;
						otherwise
							do;	
							end;
						end;			
			run;

			%let num = 5; /* number of steps */

			%if &relax ~= Y %then 
				%let randeffsvar= %str(es2usens=-1 to 1 by 0.5 es2uspec=-1 to 1 by 0.5 covsesp = -1 to 1 by 0.5);
			%else
				%let randeffsvar= %str(es2usens=0 to 1 by 0.5 es2uspec=-1 to 1 by 0.5);

			%if &model = FE %then 
				%let randeffsvar = ;

		%end;

		/* we have already fitted random effects model and now fitting
		model with covariate so use the parameter values from the model without 
		covariate as starting values using a grid search of these values within
		the range +/- 1 */
		%else %if %upcase(&reffs) = Y %then %do;
			data _null_;
			   	set &tbpe;
				select (Parameter);
						when ('msens')
							do;
							call symput('msens', put(Estimate, 5.2));
							end;
						when ('mspec')
							do;
							call symput('mspec', put(Estimate, 5.2));
							end;
						when ('es2usens')
							do;
							call symput('es2usens', put(Estimate, 5.3));
							end;
						when ('es2uspec')
							do;
							call symput('es2uspec', put(Estimate, 5.3));
							end;
						when ('covsesp')
							do;
							call symput('covsesp', put(Estimate, 5.3));
							end;
						otherwise
							do;	
							end;
						end;
			run;

			%let num=2; /* using this number of steps so nlmixed doesn't take forever */

			/* create range of values for the variance terms 0 to +1 and 
			covariance value to +1. For some reason covariance term does not like 
			range from -1 */
			%if &model = RE %then %do;
				%let ubs2usens = %sysevalf(&es2usens+1);
				%let s2usensbynum = %sysevalf(&ubs2usens/&num);
				%let es2usenssv = %str(es2usens = 0 to &ubs2usens by &s2usensbynum); /* must remember to check this range */
				
				%let ubs2uspec = %sysevalf(&es2uspec+1);
				%let s2uspecbynum = %sysevalf(&ubs2uspec/&num);
				%let es2uspecsv = %str(es2uspec = 0 to &ubs2uspec by &s2uspecbynum); /* must remember to check this range */

				%if &relax ~= Y %then %do;
					%let ubcovsesp = %sysevalf(&covsesp+1);
					%let covsespbynum = %sysevalf(&ubcovsesp/&num);
					%let covsespsv = %str(covsesp =  0 to &ubcovsesp by &covsespbynum); /* must remember to check this range */
					%let randeffsvar = %str(&es2usenssv &es2uspecsv &covsespsv);
					%end;
				%else
					%let randeffsvar = %str(&es2usenssv &es2uspecsv);
			%end;	
			%else
				%let randeffsvar = ;
		%end;

		/* create range of values (lower bound is -1 of the value and upper bound
		is +1 of the value) for mean logit sensitivity and specificity.
		Use number of steps	depending on whether or not we have the random effects */
		%let lbmsens = %sysevalf(&msens-1);
		%let ubmsens = %sysevalf(&msens+1);
		%let msensbynum= %sysevalf((&ubmsens-&lbmsens)/&num); 
		%let msenssv= %str(msens = &lbmsens to &ubmsens by &msensbynum);

		%let lbmspec = %sysevalf(&mspec-1);
		%let ubmspec = %sysevalf(&mspec+1);		
		%let mspecbynum= %sysevalf((&ubmspec-&lbmspec)/&num); 
		%let mspecsv = %str(mspec = &lbmspec to &ubmspec by &mspecbynum);

		%let pms = %str(&msenssv &mspecsv) &randeffsvar;

	%end;

	%else %put The dataset &tbpe does not exist;

	/* if macro execution is not terminated prematurely due to an error of 
	some sort then set global variable madareg to 1, i.e., successful execution so far */
	%let madareg=1;

%mend;


/********************************************************************
*																	*
* 		CREATE PARAMETER AND ESTIMATE STATEMENTS 					*
*		FOR A COVARIATE	FOR THE BIVARIATE MODEL						*
*																	*
********************************************************************/

%macro cvparsbivar(covariate=, 
	cvtype = , 
	cveffect = ,
	cset1=, cset2=, cset3=, cset4=, cset5=,
	cspa1=, cspa2=, cspa3=, cspa4=, cspa5=,
	model=);

	/* set global variable madareg to 0 to begin with */
	%let madareg=0;

	/* A:
	INITIALISE MACRO  VARIABLES */
	/* initialise these macro variables as null or with the appropriate 
	estimate statement for the reference category if categorical covariate
	otherwise if continuous nlevels= 1 */

	%let csenslist0 = ;
	%let cspeclist0 = ;
	%let plist0 = ;
	%let secv0 = ;
	%let spcv0 = ;
	%let estmuA0= ;
	%let estmuB0= ;
	
	%let lnSens0 = log( exp(msens)/(1+exp(msens)))%str(;);
	%let lnSpec0 = log(exp(mspec)/(1+exp(mspec)))%str(;);
	%let lnDOR0 = log((exp(msens)/(1+exp(msens)))/(1-(exp(msens)/(1+exp(msens))))/
				((1-(exp(mspec)/(1+exp(mspec))))/(exp(mspec)/(1+exp(mspec)))))%str(;);
	%let estDOR0 = estimate 'logDOR_0' &lnDOR0;
	%let estLRp0 = estimate 'logLR+_0' log((exp(msens)/(1+exp(msens)))/(1-(exp(mspec)/(1+exp(mspec)))))%str(;);
	%let estLRn0 = estimate 'logLR-_0' log((1-(exp(msens)/(1+exp(msens))))/(exp(mspec)/(1+exp(mspec))))%str(;);
	%let estAlphaCvL0 = ;
	%let estThetaCvL0 = ;
	%let estMuACvL0 = ;
	%let estMuBCvL0 = ;
	%let estRDOR0 = ;
	%let estRelSens0 = ;
	%let estRelSpec0 = ;


	/* B:
	CREATE PARAMETERS AND ESTIMATE STATEMENTS AND REGRESSION EQUATION*/
	/* For each level of the covariate (except the reference level) create parameters for 
	msens and/or mspec */
	%let M = 1;
	%do q = 1 %to %sysfunc(countw(&covariate));

		%if %eval(%sysfunc(countw(&covariate))) = 1 %then
			%let p = %sysfunc(findw(&covariates, &covariate, ' ', E));
		%else 
			%let p = &q;

		%do I = 1 %to &&nlevels&p;

			%let j = %eval(&M - 1);
		
			/* msens */
			%let msens = %str(msens_cv&p&I);
			%if %superq(cset&I) = %str() %then %let psens=&msens=0; /* simply use zero as starting value for
																	the additional parameter if none is given */
			%else %let psens=&msens=%superq(cset&I);
		
			/* mspec */
			%let mspec=%str(mspec_cv&p&I);
			%if %superq(cspa&I)=%str() %then %let pspec=&mspec=0; /* simply use zero as starting value for
																	the additional parameter if none is given */
			%else %let pspec=&mspec=%superq(cspa&I) ;
		
			/*create string of parameters with their starting values 
			also create lists for contrasts statements*/ 
			%if %upcase(&&cveffect&p)=SESP or &&cveffect&p=%str() %then %do; /*effect on the 2 parameters */
				%let plist= &psens &pspec;
				%let csens = &msens;
				%let cspec = &mspec;
			%end;
			%else %if %upcase(&&cveffect&p)=SE %then %do; /* covariate effect on sensitivity only*/
				%let plist=&psens;
				%let csens = &msens;
				%let mspec=0; 
				%let cspec= ;
			%end;
			%else %if %upcase(&&cveffect&p)=SP %then %do; /* covariate effect on specificity only*/
				%let plist=&pspec;
				%let cspec = &mspec;
				%let msens=0; 
				%let csens= ;
			%end;
			
			%let %str(plist&M)= %superq(plist&j) &plist; /* build string of parameters and their 
														 starting values */		
			%if (%eval(&I <= &&nlevels&p) and %eval(&q ~= %sysfunc(countw(&covariate)))) or (%eval(&I ~= &&nlevels&p) and %eval(%sysfunc(countw(&covariate)) = 1)) %then %do;
				%let %str(csenslist&M)= %superq(csenslist&j) msens+&csens,; /* for use with contrast statement build string 
														of parameters without their starting values */
				%let %str(cspeclist&M)= %superq(cspeclist&j) mspec+&cspec,;
			%end;
			%else  %do;
				%let %str(csenslist&M)= %superq(csenslist&j) msens+&csens;
				%let %str(cspeclist&M)= %superq(cspeclist&j) mspec+&cspec;
			%end;
			
			/* Now to create estimate statements for the additional parameters */
			%if %upcase(&&cvtype&p)=CON %then %do;
				%let trueposORTitle = %str(True positive logOR 1);
				%let truenegORTitle = %str(True negative logOR 1);
				%let RDORTitle = %str(logRDOR 1);
				%let RSensTitle = %str(logRSE 1);
				%let RSpecTitle = %str(logRSP 1);		
			%end;
			%else %do;
				%let trueposORTitle = %str(True positive logOR &&covariate&p: &I vs 0);
				%let truenegORTitle = %str(True negative logOR &&covariate&p: &I vs 0);
				%let RDORTitle = %str(logRDOR &&covariate&p: &I vs 0);
				%let RSensTitle = %str(logRSE &&covariate&p: &I vs 0);
				%let RSpecTitle = %str(logRSP &&covariate&p: &I vs 0);
			%end;
			%let ElogitSeTitle = %str(msens_&p&I);
			%let ElogitSpTitle = %str(mspec_&p&I);
			%let DORTitle = %str(logDOR_&p&I);
			%let LRposTitle = %str(logLR+_&p&I);
			%let LRnegTitle = %str(logLR-_&p&I);
			%let alphaTitle = %str(Lambda_&p&I);
			%let thetaTitle = %str(Theta_&p&I);	
			%let betatitle = %str(beta_&p&I);
			%let compse = %str(Pooled test for sensitivity);
			%let compsp = %str(Pooled test for specificity);
			%let estA = estimate "&trueposORTitle" &msens;
			%let estB = estimate "&truenegORTitle" &mspec;

			/* estimates of average operating points and relative measures */
			/* The statements preceded by log could be simplified but leaving them so that it is 
			obvious to anyone inspecting or modifying the macro what the function is and that the 
			estimates have been obtained on the log scale */

			%let estlnD = log(((exp(msens+&msens)/(1+exp(msens+&msens)))/(1-(exp(msens+&msens)/(1+exp(msens+&msens)))))
						/((1-(exp(mspec+&mspec)/(1+exp(mspec+&mspec))))/(exp(mspec+&mspec)/(1+exp(mspec+&mspec)))));

			%let estD = estimate "&DORTitle" &estlnD;
			%let estRD = estimate "&RDORTitle" &estlnD - &lnDOR0;
			%let estLnSens = log(exp(msens+&msens)/(1+exp(msens+&msens)));
			%let estRSens = estimate "&RSensTitle" &estLnSens - &lnSens0;
			%let estLnSpec = log(exp(mspec+&mspec)/(1+exp(mspec+&mspec)));
			%let estRSpec = estimate "&RSpecTitle" &estLnSpec - &lnSpec0;
			%let estLp = estimate "&LRposTitle" log((exp(msens+&msens)/(1+exp(msens+&msens)))/(1-(exp(mspec+&mspec)/(1+exp(mspec+&mspec)))));
			%let estLn = estimate "&LRnegTitle" log((1-(exp(msens+&msens)/(1+exp(msens+&msens))))/(exp(mspec+&mspec)/(1+exp(mspec+&mspec))));
			%let estAlphaL = estimate "&alphaTitle" ((exp(es2uspec)/exp(es2usens))**0.25 *(msens+&msens)+(exp(es2usens)/exp(es2uspec))**0.25*(mspec+&mspec))
					- ((exp(es2uspec)/exp(es2usens))**0.25 * msens+(exp(es2usens)/exp(es2uspec))**0.25 *mspec); /* compute alpha for a covariate level other than the reference */
			%let estThetaL = estimate "&thetaTitle" (0.5*((exp(es2uspec)/exp(es2usens))**0.25 * (msens+&msens)-(exp(es2usens)/exp(es2uspec))**0.25*(mspec+&mspec)))
					- (0.5*((exp(es2uspec)/exp(es2usens))**0.25*msens - (exp(es2usens)/exp(es2uspec))**0.25 * mspec)); /* compute theta for a covariate level other than the reference */
			%let estMuAL = estimate "&ElogitSeTitle" msens+&msens; /* elogit(sens) for each level of the covariate */  
			%let estMuBL = estimate "&ElogitSpTitle" mspec+&mspec; /* elogit(spec) for each level of the covariate  */
					
			/* Now need to build expressions used in the regression equation and estimate statements*/
			
			%if %eval(&I < &&nlevels&p) or %eval(&q ~= %sysfunc(countw(&covariate))) %then %do; 
				%let secv = (&msens * %str(cv&p&I)) +; /* for expression involving sensitivity */
				%let spcv = (&mspec * %str(cv&p&I)) +; /* for expression involving specificity */
				%let estmuA = %str(&estA) %str(;);
				%let estmuB = %str(&estB) %str(;);	
				%let estDOR = %str(&estD) %str(;);
				%let estRDOR = %str(&estRD) %str(;);
				%let estRelSens = &estRSens %str(;);			
				%let estRelSpec = &estRSpec %str(;);
				%let estLRp = %str(&estLp) %str(;);
				%let estLRn = %str(&estLn) %str(;);
				%let estAlphaCvL = %str(&estAlphaL) %str(;);
				%let estThetaCvL = %str(&estThetaL) %str(;);
				%let estMuACvL = %str(&estMuAL) %str(;);
				%let estMuBCvL = %str(&estMuBL) %str(;);
			%end;
			%if %eval(&I = &&nlevels&p) and %eval(&q = %sysfunc(countw(&covariate))) %then %do;
				%let secv=(&msens * %str(cv&p&I));			
				%let spcv=(&mspec * %str(cv&p&I));
				%let estmuA = &estA;
				%let estmuB = &estB;
				%let estDOR = &estD;
				%let estRDOR = &estRD;
				%let estRelSens = &estRSens;			
				%let estRelSpec = &estRSpec;
				%let estLRp = &estLp;
				%let estLRn = &estLn;
				%let estAlphaCvL = &estAlphaL;
				%let estThetaCvL = &estThetaL;
				%let estMuACvL = &estMuAL;
				%let estMuBCvL = &estMuBL;
			%end;

			%let %str(secv&M) = &&secv&j &secv; /* concatenate expressions */
			%let %str(spcv&M) = &&spcv&j &spcv;	
			%let %str(estmuA&M) = &&estmuA&j  &estmuA;
			%let %str(estmuB&M) = &&estmuB&j  &estmuB;
			%let %str(estDOR&M)= &&estDOR&j  &estDOR;
			%let %str(estRDOR&M)= &&estRDOR&j  &estRDOR;
			%let %str(estRelSens&M)= &&estRelSens&j  &estRelSens;
			%let %str(estRelSpec&M)= &&estRelSpec&j  &estRelSpec;
			%let %str(estLRp&M)= &&estLRp&j  &estLRp;
			%let %str(estLRn&M) = &&estLRn&j  &estLRn;
			%let %str(estAlphaCvL&M)= &&estAlphaCvL&j  &estAlphaCvL;
			%let %str(estThetaCvL&M)= &&estThetaCvL&j  &estThetaCvL;
			%let %str(estMuACvL&M)= &&estMuACvL&j  &estMuACvL;
			%let %str(estMuBCvL&M)= &&estMuBCvL&j  &estMuBCvL;
			run;

			%let M = %eval(&M + 1);
		%end;

		/* Put parameter list together and the equation and contrast statements*/
		%let j = %eval(&M - 1);
		%let cvparamlist = %superq(plist&j);

		%if %upcase(&&cveffect&p)=SESP or &&cveffect&p=%str() %then %do;
			%if %upcase(&model) = RE %then
				%let logitp = (msens + usens + %superq(secv&j))*sens +(mspec + uspec + %superq(spcv&j)) * spec;
			%else
				%let logitp = (msens + %superq(secv&j))*sens +(mspec +  %superq(spcv&j)) * spec;

			%let contrastse = contrast "&compse" msens, %superq(csenslist&j);
			%let contrastsp = contrast "&compsp" mspec, %superq(cspeclist&j);
		%end;
		%else %if %upcase(&&cveffect&p)=SE %then %do;
			%if %upcase(&model) = RE %then 
				%let logitp = (msens + usens + %superq(secv&j))*sens +(mspec + uspec) * spec;
			%else 
				%let logitp = (msens + %superq(secv&j))*sens +(mspec) * spec;

			%let contrastse = contrast "&compse" msens, %superq(csenslist&j);
			%let contrastsp= ;
		%end;
		%else %if %upcase(&&cveffect&p)=SP %then %do;
			%if %upcase(&model)=RE %then
				%let logitp = (msens + usens)*sens +(mspec + uspec + %superq(spcv&j)) * spec;
			%else
				%let logitp = (msens )*sens +(mspec + %superq(spcv&j)) * spec;
			%let contrastsp = contrast "&compsp" mspec, %superq(cspeclist&j);
			%let contrastse= ;
		%end;
		

		/* no contrast statements if covariate is continuous  */
		/*%if %upcase(&&cvtype&q)= CON %then %do;		
			%let contrastse= ;
			%let contrastsp= ;
		%end;*/

		%let estmuAcv = &&estmuA&j;
		%let estmuBcv = &&estmuB&j;	
		%let estDOR = &&estDOR&j;
		%let estRDOR = &&estRDOR&j;
		%let estRelSens = &&estRelSens&j;
		%let estRelSpec = &&estRelSpec&j;
		%let estLRpos = &&estLRp&j;
		%let estLRneg = &&estLRn&j;
		%let estAlpha = &&estAlphaCvL&j;
		%let estTheta = &&estThetaCvL&j;
		%let estMuAlevels= &&estMuACvL&j;
		%let estMuBlevels= &&estMuBCvL&j;
		
		run;
	%end;

	/* if macro execution is not terminated prematurely due to an error of 
	some sort then set global variable madareg to 1, i.e., successful execution so far */
	%let madareg=1;

%mend;


/********************************************************
*														*
* 	PERFORM META-ANALYSIS: Uni/Bi - variate Model		*			
*														*
********************************************************/

%macro metab(tech=, 
	subject=, 
	cialpha=, 
	byvar=, 
	covariate=,		
	cvtype=, 
	cveffect=, 
	mtitle=, 
	tbpe=, 
	p1=, p2=, p3=, p4=, p5=, 
	cset1=, cset2=, cset3=, cset4=, cset5=,
	cspa1=, cspa2=,	cspa3=, cspa4=, cspa5=, 
	randeffs=, 
	predict=, 
	checkmod=, 
	hessout=,
	relax = ,
	model = );

	/* set global variable madareg to 0 to begin with */
	%let madareg=0;


	%put %str(*******************);
	%put %str(FIT %upcase(&typem) %upcase(&model) MODEL);
	%put %str(*******************);

	%if &byvar ~=%str() %then %do;
		%put %str(*************************************************);
		%put SEPARATE ANALYSES FOR EACH LEVEL OF %upcase(&byvar);
		%put %str(*************************************************);
	%end;

	/* create an indicator variable to be used later */
	%let modfailed = 0;

	/* if performing several analyses using the BY variable then repeat 
	the analysis for each level of the BY variable ,i.e., 
	produce as many models as levels of the variable otherwise run one
	set of analyses*/
	%do k=1 %to &bylevels;

		%if &bylevels > 1 %then %do;
			data _null_;
			   	set _madareg_&byvar;
				if bylevels=&k then call symput('byname', &byvar);
			run;
		%end;
		%else %let byname = %str();

		/* if the empirical Bayes estimates of the random effects are required or model
		checking is yes, _madareg_a_randeffs will be created when Proc Nlmixed runs*/
		%if %upcase(&randeffs)=Y or %upcase(&checkmod) = Y %then %let outreffs= %str(out = _madareg_a_randeffs_&k);
		%else %let outreffs= ;
				 	
		%put %str(***************************************************);
		%put %str(ANALYSIS: Fitting %lowcase(&typem) model with no covariate);
		%put %str(***************************************************);
		
		/* STARTING VALUES FOR NO COVARIATE MODEL */
		/*if user does not provide starting values for parameters
		 then use the following  */
		/* mu_A is the mean logit sensitivity */
		/* mu_B is the mean logit specificity */
		/* s2_A, s2_B are the variances of logit sensitivity and specifcity */
		/* s2_AB is the covariance of logit sensitivity and specificity */

		%put ------------------------------------------------------;
		%put ANALYSIS STEP 1: Set up parameters and starting values;
		%put ------------------------------------------------------;

		/* for now only set this up once - will modify in next version 
		to allow multiple tables of starting values */
		%if &k = 1 %then %do;

			/* if we are not using a table of parameters */
			%if &tbpe=%str() %then %do;
				/* use these values as default to create a grid of values
				if user doesn't provide starting values for parameters */
				%if &p1=%str() %then %let p1= -2 to 4 by 1;
				%if &p2=%str() %then %let p2= -2 to 4 by 1; 
				%if &p3=%str() %then %let p3= 0 to 2 by 0.25; 
				%if &p4=%str() %then %let p4= 0 to 2 by 0.25; 
				%if &p5=%str() %then %let p5= 0 to 1 by 0.2;		
				
				/*PARAMETERS FOR THE MODEL */
				/*Build list of parameters and their starting values */
				%if &model = RE %then %do;
					%if &relax ~= Y %then
						%let pms = %str(msens=&p1 mspec=&p2 es2usens=&p3 es2uspec=&p4 covsesp=&p5);
					%else
						%let pms = %str(msens=&p1 mspec=&p2 es2usens=&p3 es2uspec=&p4);
				%end;
				%else
					%let pms = %str(msens=&p1 mspec=&p2);				
			%end;
			%else %let pms= %str( / data=&tbpe);
		%end;

		proc sort data=_madareg_ds&k;
			by &subject &covariate;
		run;

		/*RUN PROC NLMIXED: MODEL WITH NO COVARIATE */
		/*Uni/Bi-variate model without covariate*/
		%put ---------------------------------------------------------------------;
		%put %str(ANALYSIS STEP 2: Run PROC NLMIXED for _madareg_ds&k);
		%put ---------------------------------------------------------------------;

		%bivarnocv(data=_madareg_ds&k, 
			tech=&tech, 
			cialpha=&cialpha, 
			subject=&subject, 
			outreffs=&outreffs, 
			pms=&pms, 
			byname=&byname, 
			modelnum=&k, 
			predict=&predict, 
			hessout=&hessout,
			relax = &relax,
			model = &model);
		run;

		/* if above model doesn't converge or the final Hessian matrix is not positive definite, then
		additional estimates will not be produced so try fitting model using Proc MIXED 
		to obtain new starting values and then repeat analysis */
		%if %sysfunc(exist(_madareg_a_addest_&k))  %then 
				%put %str(&typem &model model without covariate successful);
		%else %do;
			%put %str(----------------------------------------------------------------------------------------);
			%put %str(ANALYSIS INFORMATION: &typem analysis using Proc NLMIXED for &byname failed.);
			%put %str(Running Proc MIXED for _madareg_ds&k to obtain new);
			%put %str(starting values for logit sensitivity, logit specificity);
			%put %str(their variances and covariance.);
			%put %str(----------------------------------------------------------------------------------------);
			
			ods select none;
			ods output SolutionF=_m_temp_solfixed
				CovParms= _m_temp_covparms
				ConvergenceStatus=_m_temp_convgstat;

			/* run proc mixed for the data so the model estimates can be used as 
			starting values for Proc NLMIXED */			
			proc mixed data=_madareg_ds&k;
				class &subject; 
				model logit=sens spec/solution noint; /* produce fixed effects solution and no intercept */
				random sens spec /subject=&subject type=un; /* want unstructured covariance structure. This 
														is useful for correlated random coefficient models */
			run ;
			ods select all;
			/* modify this data set to keep only effect and estimate 
			and give the effect the parameter names */
			%if %sysfunc(exist(_m_temp_solfixed)) %then %do;
				data _m_temp_solfixed (keep= Parameter Estimate);
					set _m_temp_solfixed;
					if Effect = 'sens' then Parameter='msens';
					else if Effect ='spec' then Parameter = 'mspec';
				run;
			%end;

			/* modify and keep only the required variables */
			%if %sysfunc(exist(_m_temp_covparms)) %then %do;
				data _m_temp_covparms (keep= Parameter Estimate);
					set _m_temp_covparms;
					Parameter = CovParm;
					label CovParm = 'Effect';
					select (CovParm);
						when ('UN(1,1)')
							do;
							Parameter='es2usens';
							end;
						when ('UN(2,2)')
							do;
							Parameter='es2uspec';
							end;
						when ('UN(2,1)')
							do;
							Parameter='covsesp';
							end;
						when ('Residual')
							do;
							delete;
							end;
						otherwise
							do;	
							end;
						end;
				run;

				/* append the data sets into a parameter estimates table */
				%if %sysfunc(exist(_m_temp_solfixed)) %then %do;
					data _m_temp_pe;
						set _m_temp_covparms _m_temp_solfixed;
					run;
				%end;
			%end;


			/* successful convergence and production of additional estimates then 
			refit model using these as starting values instead*/
			%if %sysfunc(exist(_m_temp_pe)) %then %do;
				%createbpars(tbpe=_m_temp_pe, 
					reffs=y,
					relax = &relax,
					model = &model);
				run;
				%bivarnocv(data=_madareg_ds&k, 
					tech=&tech, 
					cialpha=&cialpha, 
					subject=&subject, 
					outreffs=&outreffs, 
					pms=&pms, 
					byname=&byname, 
					modelnum=&k,
					predict=&predict, 
					hessout=&hessout,
					relax = &relax,
					model = &model);
				run;
				%if %sysfunc(exist(_madareg_a_addest_&k)) %then %do;
					%put %str(&typem modelling without covariate successfully completed for &byname);
					%let modfailed=0;
				%end;
				%else %do;
					%put %str(MODEL FAILURE: &typem modelling without covariate was not successfully completed for &byname);
					%let modfailed = 1;
				%end;
			%end;
			/*if model without random effects failed and we are only running a set of 
			analyses then terminate the macro otherwise carry on with other levels of the
			BY variable */
			%else %if &byar = %str() %then %do;
				%put %str(----------------------------------------------------------------------------------------------------------------);
				%put %str(ANALYSIS INFORMATION: madareg terminated.);
				%put %str(Modelling with Proc MIXED failed to produce suitable starting values for analysis of &byname);
				%put %str(----------------------------------------------------------------------------------------------------------------);
				%let madareg=0;
				%return;
			%end;
		%end;

		/* delete these temporary data sets */
		proc datasets memtype=data nodetails nolist; 
			delete _m_temp_:; 
		run;
		quit;


		/****************************************************************
		*																*
		* Fit model with covariate(s) if covariate(s) is(are) given &	*
		* the no covariate model converged and produced					*
		* additional estimates											*
		*																*
		*****************************************************************/
		
		%if &covariate~=%str() and %sysfunc(exist(_madareg_a_pe_&k)) and %sysfunc(exist(_madareg_a_addest_&k)) %then %do;
			%if &ncovariates < 2 %then 
				%let N = 1;
			%else 
				%let N = %eval(&ncovariates + 1);

			%do c = 1 %to &N;
				%if (%eval(&c < &N) and %eval(&N > 1)) or %eval(&N = 1) %then %do;				
					%let numobs=0;
					%let nstudies=0;
					
					/* check that we have data for the reference level of the covariate otherwise
					skip the analysis */
					%if %upcase(&&cvtype&c) ~= CON %then %do;	
						data _tmp_;
							set _madareg_ds&k;
							where cvlevels&c=0;	
						run;
						data _null_;
							set _tmp_ nobs=numobs;
							call symput('numobs',numobs);
						run;
						%let nstudies&c = %eval(&numobs/2); /* get number of studies for the covariate level */
					%end;

					%if &&nstudies&c = 0 and %upcase(&&cvtype&c) ~= CON %then %do;
						%put %str(-------------------------------------------------------------------------------------------);
						%put %str(ANALYSIS INFORMATION: Unable to perform %lowcase(&typem) analysis with covariate.);
						%put %str(There are no observations for the reference level of %upcase(&&covariate&c) for this data set.);
						%put %str(-------------------------------------------------------------------------------------------);
						%goto endcvanalysis;
						run;
					%end;
				%end;
				
				/* if the empirical Bayes estimates of the random effects are required or model
				checking is yes, _madareg_cv&c_randeffs will be created when proc nlmixed runs*/
				%if %upcase(&randeffs)=Y or %upcase(&checkmod) = Y %then %let outreffs= %str(out = cats(_madareg_cv&c, _randeffs_&k));
				%else %let outreffs= ;
							
				/* PARAMETERS AND ESTIMATE STATEMENTS FOR MODEL WITH COVARIATE*/
				%if (%eval(&c < &N) and %eval(&N > 1)) or %eval(&N = 1) %then %do;	
					%put %str(*********************************************************************);
					%put ANALYSIS: Fitting %lowcase(&typem) model with %upcase(&&covariate&c) as a covariate;
					%put %str(*********************************************************************);
				%end;	
				%else %do;
					%put %str(*********************************************************************);
					%put ANALYSIS: Fitting %lowcase(&typem) model with covariates: %upcase(&covariates);
					%put %str(*********************************************************************);
				%end;

				%put ---------------------------------------------------------------------------;
				%put ANALYSIS STEP 1: Set up parameters, starting values and estimate statements;
				%put ---------------------------------------------------------------------------;
				
				/* just need to do this once but in future will do k times if different pe tables 
				are given for each analysis of the BY variable */
				%if (%eval(&c < &N) and %eval(&N > 1)) or %eval(&N = 1) %then %do; 
					%if &k=1 or &modfailed=1 %then %do;
						/*Build additional parameters and estimate statements */
						%cvparsbivar(covariate=&&covariate&c, 
							cvtype=&&cvtype&c, 
							cveffect=&&cveffect&c, 
							cset1=&cset1, cset2=&cset2, cset3=&cset3, cset4=&cset4, cset5=&cset5,
							cspa1=&cspa1, cspa2=&cspa2, cspa3=&cspa3, cspa4=&cspa4, cspa5=&cspa5,
							model = &model);
						run;
					%end;
				%end;
				%else %do;
					%if &k=1 or &modfailed=1 %then %do;
					/*Build additional parameters and estimate statements */
						%cvparsbivar(covariate=&covariates, 
							cvtype=&cvtype, 
							cveffect=&cveffect,		
							cset1=&cset1, cset2=&cset2, cset3=&cset3, cset4=&cset4, cset5=&cset5,
							cspa1=&cspa1, cspa2=&cspa2, cspa3=&cspa3, cspa4=&cspa4, cspa5=&cspa5,
							model = &model);
						run;
					%end;
				%end;				

				/* create starting values for the parameters msens, mspec 
				s2usens, s2uspec and covsesp from the parameter estimates table */
				%createbpars(tbpe=_madareg_a_pe_&k, 
					reffs=y,
					relax = &relax,
					model = &model);
				run;

				ods output StartingValues = %sysfunc(cats(madareg_cv&c, _sv_&k))
					Parameters= %sysfunc(cats(_madareg_cv&c, _parms_&k))
					ParameterEstimates = %sysfunc(cats(_madareg_cv&c, _pe_&k))
					FitStatistics = %sysfunc(cats(_madareg_cv&c, _fit_&k))
					AdditionalEstimates = %sysfunc(cats(_madareg_cv&c, _addest_&k))
					CovMatAddEst = %sysfunc(cats(_madareg_cv&c, _covaddest_&k))
					CovMatParmEst = %sysfunc(cats(_madareg_cv&c, _covparmest_&k))
					ConvergenceStatus = %sysfunc(cats(_madareg_cv&c, _convgstat_&k))
					Contrasts = %sysfunc(cats(_madareg_cv&c, _contrasts_&k));

				%if &hessout ~=%str() %then %do;
					ods output Hessian=%sysfunc(cats(_madareg_cv&c, _hessian_&k));
				%end;

				/* RUN PROC NLMIXED: MODEL WITH A COVARIATE */
				%if (%eval(&c < &N) and %eval(&N > 1)) or %eval(&N = 1) %then %do;
					%put ------------------------------------------------------------------------------;
					%put %str(ANALYSIS STEP 2: Run Proc NLMIXED for _madareg_ds&k with covariate %upcase(&&covariate&c));
					%put ------------------------------------------------------------------------------;
				%end;
				%else %do;
					%put ------------------------------------------------------------------------------;
					%put %str(ANALYSIS STEP 2: Run Proc NLMIXED for _madareg_ds&k with covariates: %upcase(&covariates));
					%put ------------------------------------------------------------------------------;
				%end;

				%if %upcase(&predict) = Y %then
					%let predictlogit= %quote(predict logitp out = %sysfunc(cats(_madareg_cv&c, _logitp_&k)));
				%else %let predictlogit= ;

				%if (%eval(&c < &N) and %eval(&N > 1)) or %eval(&N = 1) %then %do;
					%if &bylevels > 1 %then %let
						proctitle = %str("&typem analysis &k with covariate %upcase(&&covariate&c)");
					%else %let proctitle = %str("&typem analysis with covariate %upcase(&&covariate&c)");
				%end;
				%else %do;
					%if &bylevels > 1 %then %let
						proctitle = %str("&typem analysis &k with covariates: %upcase(&covariates)");
					%else %let proctitle = %str("&typem analysis with covariates: %upcase(&covariates)");

				%end;

				%if %upcase(&relax) = N %then %do;
					%let covsesp = covsesp;
					
					/*Estimate correlation of expected 	logit(sensitivity)and logit(specificity)*/
					%let estCorr = estimate "Corr(logits)" &covsesp/(SQRT(exp(es2usens))*(SQRT(exp(s2uspec))));
				%end;
				%else %if %upcase(&relax) = Y or %upcase(&model) = 	FE %then %do;
					%let covsesp = 0;
					%let estCorr = ;
				%end;
	
				%if &model = RE %then %do;
					%let sse = %str(s2usens = exp(es2usens)); 
					%let ssp = %str(s2uspec = exp(es2uspec));
					%let randomstatement = %str(random usens uspec ~ normal([0,0],[s2usens, &covsesp, s2uspec]) subject=&subject &outreffs);
					
					/* convert to HSROC parameterisation */
					/* theta is the threshold parameter, lambda is the accuracy parameter*/				
					/* beta is the shape parameter*/
					%let estsse = estimate 's2usens' exp(es2usens);
					%let estssp = estimate 's2uspec' exp(es2uspec);
					%let estlambda = %str(estimate 'Lambda' (exp(es2uspec)/exp(es2usens))**0.25 * msens  + (exp(es2usens)/exp(es2uspec))**0.25 * mspec) ;
					%let esttheta = %str(estimate 'Theta' 0.5* ( (exp(es2uspec)/exp(es2usens))**0.25 * msens - (exp(es2usens)/exp(es2uspec))**0.25 * mspec )) ;
					%let estbeta = %str(estimate 'beta' log( sqrt(exp(es2uspec)/exp(es2usens)) ) );
					%let estvaraccu = %str(estimate 'Var(accuracy)'   2*( sqrt(exp(es2usens)*exp(es2uspec)) + &covsesp) );
					%let estvarthresh = %str(estimate 'Var(threshold)' 0.5*( sqrt(exp(es2usens)*exp(es2uspec)) - &covsesp) );
				%end;
				%else %do;
					%let sse = ; 
					%let ssp = ;
					%let randomstatement =;
					%let estsse = ;
					%let estssp = ;
					%let estlambda = ;
					%let esttheta =  ;
					%let estbeta = ;
					%let estvaraccu =  ;
					%let estvarthresh =  ;
					%let estAlpha = ;
					%let estTheta = ;
				%end;

				
				proc nlmixed data=_madareg_ds&k cov ecov df=1000 start alpha=&cialpha &hessout tech=&tech;
					title &proctitle;
					/* msens is the mean logit sensitivity */
					/* msens_cv1 is the change in mean logit sensitivity for covariate level =1*/
					/* mspec is the mean logit specificity */
					/* mspec_cv1 is the change in mean logit specificity for covariate level =1*/				
					/* s2usens, s2uspec are the variances of logit sensitivity and specifcity */
					/* covsesp is the covariance of logit sensitivity and specificity */
					/* subject is usually study ID*/
					/* To some extent the order of the estimate statements are important in 
					extracting data for output so if you change it check 
					that the tables are still ok - any changes necessary should 
					be done to bivarout */
					parms &pms &cvparamlist /Best=5; /*reset initial value for new analysis*/
					&sse;
					&ssp;
					logitp = &logitp;
					p = exp(logitp)/(1+exp(logitp));
					model true ~ binomial(n,p);
					&randomstatement;

					&predictlogit;
					&estsse;
					&estssp;
					&estCorr;					
					&estlambda;
					&esttheta ;
					&estbeta;
					&estvaraccu ;
					&estvarthresh;

					&estmuAcv; /* logit(sens) change for covariate levels*/
					&estmuBcv; /* logit(spec) change for covariate levels*/
					
					/* estimates of average operating points and other 
					conversions to HSROC parameterisation */
					&estMuAlevels; /* elogit(sens) for each level of the covariate */ 
					&estMuBlevels; /* elogit(spec) for each level of the covariate */
					&estDOR; /* diagnostic odds ratio for each level of the covariate */ 
					&estRelSens; /* relative sensitivity for each level of the covariate */ 
					&estRelSpec; /* relative specificity for each level of the covariate */ 
					&estRDOR; /* relative diagnostic odds ratio for each level of the covariate */ 
					&estLRpos; /* positive likelihood ratio for each level of the covariate */ 
					&estLRneg; /* negative likelihood ratio for each level of the covariate */ 
					&estAlpha; /* alpha for each level of the covariate except the reference category*/ 
					&estTheta; /* theta for each level of the covariate except the reference category*/
					&contrastse;
					&contrastsp;
				run;
				quit;			
			%end;
					
			%endcvanalysis:;

		%end;

	%end;
	/* create data sets in the format required from results of the analysis */
	%if &covariate ~= %str() %then 
		%let covariate = &covariates;

	%bivarout(covariate=&covariate, 
		cvtype=&cvtype, 
		cveffect=&cveffect, 
		cvsummorder=&cvsummorder, 
		byvar=&byvar,
		relax = &relax,
		model = &model);
	run;

	/* if macro execution is not terminated prematurely due to an error of 
	some sort then set global variable madareg to 1, i.e., successful execution so far */
	%let madareg=1;
	
%mend;


/********************************************************************
*																	*
* 			CREATE EMPTY DATA SET FOR STORING SeElogitSe, 			*
*			SeElogitSp, CovES and studies							*
*																	*
********************************************************************/

%macro createds();

	/* set global variable madareg to 0 to begin with */
	%let madareg=0;

	/* create a data set so we can add the values required for the confidence and
	prediction regions in Revman. Datalines not allowed in macros so let's find a 
	way round it using a temporary file with infile */

	filename temp temp; 

    data _null_; 
		file temp; 
		put; 
	run; 

    data _tb_se; 
      	infile temp; 
      	input @; 
      	do _infile_ = 
			'. . . .' ;
         	input SeELogitSe @1 SeELogitSp @1 CovEs @1 Studies @1 @; 
         	output; 
    	end; 
  	run; 

    filename temp clear; 

	%if %sysfunc(exist(_tb_se)) %then %do;
		data _tb_se;
			set _tb_se;
			label SeELogitSe ='SE(E(logitSe))'
					SeELogitSp = 'SE(E(logitSp))'
					CovEs = 'Cov(Es)'
					Studies = 'Studies';
		run;
	%end;

	/* if macro execution is not terminated prematurely due to an error of 
	some sort then set global variable madareg to 1, i.e., successful execution so far */
	%let madareg=1;

%mend;

/********************************************************************
*																	*
* 		CREATE DATA SETS FOR OUTPUT OF BIVARIATE MODEL RESULTS		*
*																	*
********************************************************************/

/* NOTE: similar to hsrocout especially to begin with and could have modified
it to output both models but maybe more readable and manageable separately */

%macro bivarout(covariate = ,
	cvtype = ,
	cveffect = ,
	cvsummorder = ,
	byvar = ,
	relax = ,
	model = );

	/* set global variable madareg to 0 to begin with */
	%let madareg=0;

	%put %str(************************);
	%put %str(CREATE TABLES OF RESULTS);
	%put %str(************************);

	/* create blank data set for storing SeElogitSe, SeElogitSp, CovEs and
	number of studies */
	%createds();
	run;

	%do k=1 %to &bylevels;
		/* if there is a BY variable get the name of the current by category.*/
		%if &byvar ~= %str() %then %do;
			data _null_;
			   	set _madareg_&byvar;
				if bylevels=&k then call symput('bylevelname', &byvar);
			run;
		%end;
		%else 
			%let bylevelname=%str();


		%if %sysfunc(exist(_madareg_ds&k)) %then %do;
			data _null_;
				set _madareg_ds&k nobs=numobs;
				call symput('numobs',numobs);
			run;
			%let nstudies = %eval(&numobs/2);
		%end;

		/* Give each estimate in the additional estimates table
		a unique identifier if the table exists*/
		%if %sysfunc(exist(_madareg_a_addest_&k)) and %sysfunc(exist(_tb_se)) %then %do;

			data _madareg_a_addest_&k;
				set _madareg_a_addest_&k;
				id=_n_;
			run;

			/* get exponents of logDOR, logLR- and logLR+.
			Append to additional estimates table (_a_addest_&k) */
			data _madareg_a_addest_temp (drop=Label);
				length newlabel $ 14;
				set _madareg_a_addest_&k;
				substr(label,1,3)='';
				newlabel=left(trim(label));
				expestimate=exp(estimate);
				explower=exp(lower);
				expupper=exp(upper);
				where label contains 'logL' or label contains 'logD';
				rename Estimate = LogEstimate
						lower = LogLower
						upper = LogUpper;
				StandardError=.;
				tValue=.;
				Probt=.;
				output;
			run;

			data _madareg_a_addest_temp (drop=LogEstimate LogLower LogUpper);
				set _madareg_a_addest_temp;
				rename newlabel=Parameter
						expestimate = Estimate
						explower = Lower
						expupper = Upper;
			run;

			/* get the inverse transformation of logit sensitivity and specificity */
			data _madareg_a_pe_sesp;
				length Parameter $ 14;
				set _madareg_a_pe_&k;
				invlogitestimate=exp(estimate)/(1 + exp(estimate));
				invlogitlower=exp(lower)/(1 + exp(lower));
				invlogitupper=exp(upper)/(1 + exp(upper));
				where Parameter = 'msens' or Parameter = 'mspec';
				rename Estimate = LogitEstimate
						Lower = LogitLower
						Upper = LogitUpper;
				StandardError=.;
				tValue=.;
				Probt=.;
				if Parameter = 'msens' then Parameter = 'Sensitivity';
				else if Parameter = 'mspec' then Parameter = 'Specificity';
				output;
			run;

			data _madareg_a_pe_sesp (drop=LogitEstimate LogitLower LogitUpper);
				set _madareg_a_pe_sesp;
				rename 	invlogitestimate = Estimate
						invlogitlower = Lower
						invlogitupper = Upper;
			run;

			/* get the inverse transformation of variance */
			data _madareg_a_pe_var_&k;
				length Parameter $ 14;
				set _madareg_a_pe_&k;
				expestimate=exp(estimate);
				explower=exp(lower);
				expupper=exp(upper);
				where Parameter = 'es2usens' or Parameter = 'es2uspec';
				rename Estimate = logEstimate
						Lower = logLower
						Upper = logUpper;
				StandardError=.;
				tValue=.;
				Probt=.;
				if Parameter = 'es2usens' then Parameter = 's2usens';
				else if Parameter = 'es2uspec' then Parameter = 's2uspec';
				output;
			run;

		    data _null_;
			   	set _madareg_a_addest_&k;
				if label = 's2usens' then do;
					call symput('seStandardError', StandardError);
					call symput('setValue', tValue);
					call symput('seProbt', Probt);
				end;

				if label = 's2uspec' then do;
					call symput('spStandardError', StandardError);
					call symput('sptValue', tValue);
					call symput('spProbt', Probt);
				end;
			run;

			data _madareg_a_pe_var_&k (drop=logEstimate LogLower LogUpper);
				set _madareg_a_pe_var_&k;
		 		if Parameter ='s2usens' then do;
					StandardError = symgetn('seStandardError');
					tValue=symgetn('setValue');
					Probt=symgetn('seProbt');
				end;
		 		if Parameter ='s2uspec' then do;
					StandardError = symgetn('spStandardError');
					tValue=symgetn('sptValue');
					Probt=symgetn('spProbt');
				end;
				rename 	expestimate = Estimate
						explower = Lower
						expupper = Upper;
			run;
			/* Create data set of measures of test accuracy */
			data _madareg_a_tmp_summst_&k (keep=parameter estimate lower upper);
				set _madareg_a_pe_sesp _madareg_a_addest_temp;
			run;

			%if %sysfunc(exist(_madareg_a_addest_temp)) %then %do;
				proc datasets memtype=data nodetails nolist;
					delete _madareg_a_addest_temp;
				run;
				quit;
			%end;

			%if %sysfunc(exist(_madareg_a_pe_sesp)) %then %do;
				proc datasets memtype=data nodetails nolist;
					delete _madareg_a_pe_sesp;
				run;
				quit;
			%end;

			/* Get the standard error for msens and mspec.
		     They should be in the first 2 rows of the table */
		    data _null_;
			   	set _madareg_a_pe_&k;
				if Parameter='msens' then call symput('SeELogitSe', StandardError);
				else if Parameter='mspec' then call symput('SeELogitSp', StandardError);
			run;

			data _madareg_a_tmp_se_&k;
			   	set _tb_se;
				SeELogitSe=symget('SeElogitSe');
				SeELogitSp=symget('SeElogitSp');
				Studies=resolve('&nstudies');
			run;

			/*Get the covariance CovEs */
			data _null_;
			   	set _madareg_a_covparmest_&k;
				if row=1 then call symput('CovEs', mspec);
				stop;
			run;

			data _madareg_a_tmp_se_&k;
			   	set _madareg_a_tmp_se_&k;
				CovEs=symget('CovEs');
			run;

			/* _tb_nocvse_&k is a wide format so transpose */
			proc transpose data=_madareg_a_tmp_se_&k out=_madareg_a_tmp_se_&k;
			run;

			data _madareg_a_tmp_se_&k(drop = _NAME_);
				set _madareg_a_tmp_se_&k;
				label _LABEL_= 'Parameter'
						COL1= 'Estimate';
			run;

			%if &model = RE %then %do;
				/* Create data set of alternative model parameters, i.e., if method is
				bivariate then obtain data set of HSROC model parameters */
				%if &relax ~= Y %then %do;
					data _madareg_a_tmp_statpms_&k(drop=alpha df id);
						set _madareg_a_addest_&k;
						rename tValue = z
								Probt = Probz
								label= Parameter;
						label tValue = 'z'
								Probt = 'Pr > |z|';
						where id > 6;
					run;
				%end;
				%else %do;
					data _madareg_a_tmp_statpms_&k(drop=alpha df id);
						set _madareg_a_addest_&k;
						rename tValue = z
								Probt = Probz
								label= Parameter;
						label tValue = 'z'
								Probt = 'Pr > |z|';
						where id > 5;
					run;
				%end;
			%end;

			/* get corr(logits) from additional estimates table
			and append to parameter estimates table */
			%if &relax ~= Y %then %do;
				data _madareg_a_tmp_corr_&k(drop=id);
				   	set _madareg_a_addest_&k;
					where label = 'Corr(logits)';
					rename label = Parameter;
				run;
			%end;

			data _madareg_a_pe_&k;
				length Parameter $ 14;
			   	set _madareg_a_pe_&k;
				if Parameter = 'es2usens' or Parameter = 'es2uspec' then delete;
			run;

			data _madareg_a_pe_&k;
				set _madareg_a_pe_&k _madareg_a_pe_var_&k;
			run;

			%if %sysfunc(exist(_madareg_a_pe_var_&k)) %then %do;
				proc datasets memtype=data nodetails nolist;
					delete _madareg_a_pe_var_&k;
				run;
				quit;
			%end;

			%if &relax ~= Y %then %do;
				data _madareg_a_pe_&k;
				   	set _madareg_a_pe_&k _madareg_a_tmp_corr_&k;
				run;

				data _madareg_a_pe_&k(drop=Alpha DF);
				   	set _madareg_a_pe_&k;
					format RM_Name $char14.;
					rename tValue = z
							Probt = Probz;
					label tValue = 'z'
							Probt = 'Pr > |z|';
					RM_Name=Parameter;
					select (Parameter);
							when ('msens')
								do;
								RM_Name='E(logitSe)';
								end;
							when ('mspec')
								do;
								RM_Name='E(logitSp)';
								end;
							when ('s2usens')
								do;
								RM_Name='Var(logitSe)';
								end;
							when ('s2uspec')
								do;
								RM_Name='Var(logitSp)';
								end;
							when ('covsesp')
								do;
								RM_Name='Cov(logits)';
								end;
							when ('Corr(logits)')
								do;
								RM_Name='Corr(logits)';
								end;
							otherwise
								do;
								end;
							end;
				run;
			%end;

			%else %do;
				data _madareg_a_pe_&k(drop=Alpha DF);
				   	set _madareg_a_pe_&k;
					format RM_Name $char14.;
					rename tValue = z
							Probt = Probz;
					label tValue = 'z'
							Probt = 'Pr > |z|';
					RM_Name=Parameter;
					select (Parameter);
							when ('msens')
								do;
								RM_Name='E(logitSe)';
								end;
							when ('mspec')
								do;
								RM_Name='E(logitSp)';
								end;
							when ('s2usens')
								do;
								RM_Name='Var(logitSe)';
								end;
							when ('s2uspec')
								do;
								RM_Name='Var(logitSp)';
								end;
							otherwise
								do;
								end;
							end;
				run;
			%end;
		%end;
		%else %do;
			%put The data set _madareg_a_addest_&k and/or _tb_se does not exist.;
		%end;

		/* if we have (a) covariate(s) create a table with the estimates we need for the uni/bi - variate model */
		%if &covariate ~= %str() %then %do;
		
			%if %eval(%sysfunc(countw(&covariate))) = 1 %then
				%let W = 1;
			%else
				%let W = %eval(&ncovariates + 1);
				
			%do I = 1 %to &W;
				/* Get parameter estimates */
				%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&I, _pe_&k)))) %then %do;
					data %sysfunc(cats(_madareg_cv&I, _pe_all_ds&k))(drop=Alpha DF);
						set %sysfunc(cats(_madareg_cv&I, _pe_&k));
						id=_n_;
						rename tValue=z
								Probt=Probz;
						label tValue = 'z'
							Probt = 'Pr > |z|';
					run;	
					
					%if %upcase(&model) = RE %then %do;
						/* get the inverse transformation of variance */
						data _null_;
							set %sysfunc(cats(_madareg_cv&I, _pe_all_ds&k));
							expestimate=exp(estimate);
							explower=exp(lower);
							expupper=exp(upper);
							where Parameter = 'es2usens' or Parameter = 'es2uspec';

							if Parameter = 'es2usens' then do;
								call symput('seEstimate', expestimate);
								call symput('seLower', explower);
								call symput('seUpper', expupper);
							end;

							if Parameter = 'es2uspec' then do;
								call symput('spEstimate', expestimate);
								call symput('spLower', explower);
								call symput('spUpper', expupper);
							end;
						run;

					    data _null_;
						   	set %sysfunc(cats(_madareg_cv&I, _addest_&k));
							if label = 's2usens' then do;
								call symput('seStandardError', StandardError);
								call symput('setValue', tValue);
								call symput('seProbt', Probt);
							end;

							if label = 's2uspec' then do;
								call symput('spStandardError', StandardError);
								call symput('sptValue', tValue);
								call symput('spProbt', Probt);
							end;
						run;

						data %sysfunc(cats(_madareg_cv&I, _pe_all_ds&k));
							set %sysfunc(cats(_madareg_cv&I, _pe_all_ds&k));

							if Parameter ='es2usens' then do;
								Estimate = symgetn('seEstimate');
								Lower = symgetn('seLower');
								Upper = symgetn('seUpper');
					 			StandardError = symgetn('seStandardError');
							 	z=symgetn('setValue');
								Probz=symgetn('seProbt');
								Parameter = 's2usens';
							end;

							if Parameter ='es2uspec' then do;
								Estimate = symgetn('spEstimate');
								Lower = symgetn('spLower');
								Upper = symgetn('spUpper');
					 			StandardError = symgetn('spStandardError');
								z=symgetn('sptValue');
								Probz=symgetn('spProbt');
								Parameter = 's2uspec';
							end;
						run;

						%if &relax ~= Y %then %do;
							/* Get parameter estimates for level 0*/
							data %sysfunc(cats(_madareg_cv&I, _pe_cv0_ds&k)) (drop=Gradient);
								length Parameter $ 14;
								set %sysfunc(cats(_madareg_cv&I, _pe_all_ds&k));
								where id between 1 and 5;
							run;

							/* Get s2usens, s2uspec and covsesp as we are not estimating these for each level */
							data _tb_s2u_&I&k (drop=Gradient);
								length Parameter $ 14;
								set %sysfunc(cats(_madareg_cv&I, _pe_all_ds&k));
								where id between 3 and 5;
							run;
						%end;
						%else %do;
							/* Get parameter estimates for level 0*/
							data %sysfunc(cats(_madareg_cv&I, _pe_cv0_ds&k)) (drop=Gradient);
								length Parameter $ 14;
								set %sysfunc(cats(_madareg_cv&I, _pe_all_ds&k));
								where id between 1 and 4;
							run;

							/* Get s2usens, s2uspec and covsesp as we are not estimating these for each level */
							data _tb_s2u_&I&k (drop=Gradient);
								length Parameter $ 14;
								set %sysfunc(cats(_madareg_cv&I, _pe_all_ds&k));
								where id between 3 and 4;
							run;
						%end;
					%end;
					%else %do;
						/* Get parameter estimates for level 0*/
						data %sysfunc(cats(_madareg_cv&I, _pe_cv0_ds&k)) (drop=Gradient);
							length Parameter $ 14;
							set %sysfunc(cats(_madareg_cv&I, _pe_all_ds&k));
							where id between 1 and 2;
						run;
					%end;
				%end;
				%else %do;
					%put %str(MODEL FAILURE: Parameter estimates for &bylevelname with covariate &covariate were not produced.
							This may be due to failure to converge or other issues with the model and/or data.);
				%end;

				%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&I, _addest_&k)))) %then %do;

					data %sysfunc(cats(_madareg_cv&I, _ae_all_ds&k))(drop=Alpha DF);
						set %sysfunc(cats(_madareg_cv&I, _addest_&k));
						id=_n_;
						relativestat=.;
						rename tValue=z
							Probt=Probz;
						label tValue = 'z'
							Probt = 'Pr > |z|';
					run;

					/* might be useful to have the correlation too
					in the parameter estimates table so get it and
					append to the variances */
					%if %upcase(&relax) ~= Y and %upcase(&model) = RE %then %do;
						data %sysfunc(cats(_madareg_tmp_corr_cv&I, _&k));
							set %sysfunc(cats(_madareg_cv&I, _ae_all_ds&k));
							where id =1;
							rename label = Parameter;
						run;

						data _tb_s2u_&I&k;
							set _tb_s2u_&I&k %sysfunc(cats(_madareg_tmp_corr_cv&I, _&k));
						run;

						data %sysfunc(cats(_madareg_cv&I, _pe_cv0_ds&k));
							set %sysfunc(cats(_madareg_cv&I, _pe_cv0_ds&k)) %sysfunc(cats(_madareg_tmp_corr_cv&I, _&k));
						run;

						proc datasets nodetails nolist;
							delete %sysfunc(cats(_madareg_tmp_corr_cv&I, _&k));
						run;
						quit;
					%end;
					/* create a variable to identify relative values such as RDOR, relative sens and spec
					and change in sens and spec */
					data %sysfunc(cats(_madareg_cv&I, _ae_rel_ds&k));
						set %sysfunc(cats(_madareg_cv&I, _ae_all_ds&k));
						relativestat=1;
						where label contains "True" or label contains "logR";
					run;

					/* now merge with the data set containing all the additional parameters */
					proc sort data = %sysfunc(cats(_madareg_cv&I, _ae_rel_ds&k));
						by id;
					run;

					proc sort data = %sysfunc(cats(_madareg_cv&I, _ae_all_ds&k));
						by id;
					run;

					data %sysfunc(cats(_madareg_cv&I, _ae_all_ds&k));
						merge %sysfunc(cats(_madareg_cv&I, _ae_all_ds&k)) %sysfunc(cats(_madareg_cv&I, _ae_rel_ds&k));
						by id;
					run;
					%if %upcase(&model) = RE %then %do;
						%if &relax ~= Y %then %do;
							data %sysfunc(cats(_madareg_cv&I, _ae_cv0_ds&k));
								set %sysfunc(cats(_madareg_cv&I, _ae_all_ds&k));
								where (label contains "0" or id between 4 and 8)
										and relativestat~=1;
							run;
						%end;
						%else %do;
							data %sysfunc(cats(_madareg_cv&I, _ae_cv0_ds&k));
								set %sysfunc(cats(_madareg_cv&I, _ae_all_ds&k));
								where (label contains "0" or id between 3 and 7)
									and relativestat~=1;
							run;
						%end;
					%end;
					%else %do;
						data %sysfunc(cats(_madareg_cv&I, _ae_cv0_ds&k));
							set %sysfunc(cats(_madareg_cv&I, _ae_all_ds&k));
							where label contains "0" 
								and relativestat~=1;
						run;
					%end;
				%end;
				%else %do;
					%put %str(MODEL FAILURE: Additional estimates for &bylevelname with covariate &covariate were not produced.
							This may be due to failure to converge or other issues with the model and/or data.);
				%end;
			
				%if %eval(&I <= %sysfunc(countw(&covariate))) %then %do;
					%let z = 1;
					%let y = &I;
				%end;
				%else %do;
					%let z = %sysfunc(countw(&covariate));
					%let y = 1;
				%end;

				%let x = 0;	
	
				%do t = 1 %to &z;

					%if &t = 1 %then 
						%let b = 0;
					%else
						%let b = 1;
	
					%do j = &b %to &&nlevels&y;

						%if %eval(&t > 1) %then %do;
							%let l = &x;
						%end;
						%else %do;
							%let l = &j;
						%end;
						
						/* reset these macro variables to zero */
						%let numobs = 0;
						%let nstudies = 0;

						/* get the number of studies for the level of the covariate */
						%if %upcase(&&cvtype&y) ~= CON %then %do;
							data _tmp_;
								set _madareg_ds&k;
								where cvlevels&y=&j;
							run;
							data _null_;
								set _tmp_ nobs=numobs;
								call symput('numobs',numobs);
							run;
							%let nstudies = %eval(&numobs/2); /* get number of studies for the covariate level */
						%end;

						/* only do this if there's at least one study with the level */
						%if &nstudies > 0 or %upcase(&&cvtype&y) = CON %then %do;

							/* Get additional estimates for each level of the covariate */
							%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&I, _ae_all_ds&k)))) %then %do;

								%if &j > 0  %then %do;
									%if %upcase(&model) = RE %then %do;
										%if &relax ~= Y %then %do;
											data %sysfunc(cats(_madareg_cv&I, _ae_cv&l&sub&k));
												set %sysfunc(cats(_madareg_cv&I, _ae_all_ds&k));
												where label contains "&y&j" or label contains "%sysfunc(scanq(&covariate, &y)): &j" or id between 6 and 8;
											run;
										%end;
										%else %do;
											data %sysfunc(cats(_madareg_cv&I, _ae_cv&l&sub&k));
												set %sysfunc(cats(_madareg_cv&I, _ae_all_ds&k));
												where label contains "&y&j" or label contains "%sysfunc(scanq(&covariate, &y)): &j" or id between 5 and 7;
											run;
											quit;
										%end;
									%end;
									%else %do;
										data %sysfunc(cats(_madareg_cv&I, _ae_cv&l&sub&k));
											set %sysfunc(cats(_madareg_cv&I, _ae_all_ds&k));
											where label contains "&y&j" or label contains "%sysfunc(scanq(&covariate, &y)): &j";
										run;
										quit;
									%end;
									/* manipulate data sets in order to get parameter
									estimates for other levels of the covariate */
									data %sysfunc(cats(_madareg_cv&I, _pe_cv&l&sub&k));
										set %sysfunc(cats(_madareg_cv&I, _ae_all_ds&k)) ;
										rename Label= Parameter;
										where Label contains "msens_&y&j" or Label contains "mspec_&y&j";
									run;
									quit;

									%if %upcase(&model) = RE %then %do;
										/* append the data set above to the one containing the variances */
										data %sysfunc(cats(_madareg_cv&I, _pe_cv&l&sub&k));
											set %sysfunc(cats(_madareg_cv&I, _pe_cv&l&sub&k)) _tb_s2u_&I&k;
										run;
										quit;

										data _temp_HSROCparms;
											set %sysfunc(cats(_madareg_cv&I, _ae_cv&l&sub&k));
											where label contains "Lambda_&y&j" or label contains "Theta_&y&j";
										run;
										quit;

										data %sysfunc(cats(_madareg_cv&I, _ae_cv&l&sub&k));
											set %sysfunc(cats(_madareg_cv&I, _ae_cv&l&sub&k));
											if Label = "Lambda_&y&j" or Label = "Theta_&y&j" then delete;
										run;
										quit;

										data %sysfunc(cats(_madareg_cv&I, _ae_cv&l&sub&k));
											set _temp_HSROCparms %sysfunc(cats(_madareg_cv&I, _ae_cv&l&sub&k));
										run;
										quit;

										proc datasets nodetails nolist;
											delete _temp_HSROCparms;
										run;
										quit;
									%end;
								%end;

								/*Get the SE and covariance CovEs for msens and mspec
								first get the id from _madareg_ae_cv&j_&k which should match
								the row number in the covariance matrix table. The standard
								error should be from row 1 and 2 of the data set.
								To get the covariance use the id for msens as the row
								and the id of mspec as the column */

								/* if it is the reference level get the covariance from
								row 1 in mspec column of the covparmest table
								otherwise get it from the covaddest table using the
								generated column name */
								%if &j = 0 and &t = 1 %then %do;
									data _null_;
										set %sysfunc(cats(_madareg_cv&I, _pe_cv&j&sub&k));
										if Parameter="msens" then do;
											call symput('SeELogitSe', StandardError);
										end;
										else if Parameter="mspec" then do;
											call symput('SeELogitSp', StandardError);
										end;
									run;
									quit;
									data _null_;
										set %sysfunc(cats(_madareg_cv&I, _covparmest_&k));
										if row=1 then call symput('CovEs', mspec);
										stop;
									run;
									quit;
								%end;
								%if &j > 0 %then %do;
									data _null_;
										set %sysfunc(cats(_madareg_cv&I, _ae_cv&l&sub&k));
										if label="msens_&y&j" then do;
											call symput('SeELogitSe', StandardError);
											call symput('sensid', id);
										end;
										if label="mspec_&y&j" then do;
											call symput('SeELogitSp', StandardError);
											call symput('specid', left(id));
										end;
									run;
									quit;

									%let covcol = Cov&specid;

									data _null_;
										set %sysfunc(cats(_madareg_cv&I, _covaddest_&k));
										if row=resolve('&sensid') then call symput('CovEs', resolve(&covcol));
									run;
									quit;
								%end;

								data %sysfunc(cats(_madareg_cv&I, _cov_cv&l&sub&k));
									set _tb_se;
									SeELogitSe=symget('SeElogitSe');
									SeELogitSp=symget('SeElogitSp');
									CovEs=symget('CovEs');
									Studies=resolve('&nstudies');
								run;
								quit;

								/* _tb_cov_cv&j_ds&k is a wide format so transpose */
								proc transpose data=%sysfunc(cats(_madareg_cv&I, _cov_cv&l&sub&k)) out=%sysfunc(cats(_madareg_cv&I, _cov_cv&l&sub&k));
								run;

								data %sysfunc(cats(_madareg_cv&I, _cov_cv&l&sub&k))(drop = _NAME_);
									set %sysfunc(cats(_madareg_cv&I, _cov_cv&l&sub&k));
									label _LABEL_= 'Parameter'
											COL1= 'Estimate';
								run;
								quit;

								/* just some tidying up of data sets */
								data %sysfunc(cats(_madareg_cv&I, _ae_cv&l&sub&k));
									set %sysfunc(cats(_madareg_cv&I, _ae_cv&l&sub&k));
									orderno=_n_;
									cvgroup=&y&j;
									rename Label=Parameter;
								run;
								quit;

								data %sysfunc(cats(_madareg_cv&I, _pe_cv&l&sub&k)) (keep=parameter estimate standarderror z probz lower upper);
									set %sysfunc(cats(_madareg_cv&I, _pe_cv&l&sub&k));
								run;
								quit;

								/* Create one table for estimates of summary statistics for all the levels of the covariate */
								%if &j = 0 and &t = 1 %then %do;
									data _temp_msens;
										length Parameter $ 45;
										set %sysfunc(cats(_madareg_cv&I, _pe_cv&j&sub&k));
										where Parameter contains "msens" or Parameter contains "mspec";
										cvgroup=&y&j;
									run;
									quit;
									%if &z > 1 %then %do;
										%if %upcase(&&cvtype&y)= CON %then %do;
											data _temp_msens;
												set _temp_msens;
												RegularExpressionId1 = prxparse("s/msens/Sensitivity: at 0/");
												RegularExpressionId2 = prxparse("s/mspec/Specificity: at 0/");
												call prxchange(RegularExpressionId1, -1, parameter);
												call prxchange(RegularExpressionId2, -1, parameter);
												output;
											run;
											quit;
										%end;
										%else %do;
											data _temp_msens;
												set _temp_msens;
												RegularExpressionId1 = prxparse("s/msens/Sensitivity: level 0/");
												RegularExpressionId2 = prxparse("s/mspec/Specificity: level 0/");
												call prxchange(RegularExpressionId1, -1, parameter);
												call prxchange(RegularExpressionId2, -1, parameter);
												output;
											run;
											quit;
										%end;
									%end;
									%else %do;
										%if %upcase(&&cvtype&y)= CON %then %do;
											data _temp_msens;
												set _temp_msens;
												RegularExpressionId1 = prxparse("s/msens/Sensitivity: %sysfunc(scanq(&covariate, &y)) at 0/");
												RegularExpressionId2 = prxparse("s/mspec/Specificity: %sysfunc(scanq(&covariate, &y)) at 0/");
												call prxchange(RegularExpressionId1, -1, parameter);
												call prxchange(RegularExpressionId2, -1, parameter);
												output;
											run;
											quit;
										%end;
										%else %do;
											data _temp_msens;
												set _temp_msens;
												RegularExpressionId1 = prxparse("s/msens/Sensitivity: %sysfunc(scanq(&covariate, &y)) level 0/");
												RegularExpressionId2 = prxparse("s/mspec/Specificity: %sysfunc(scanq(&covariate, &y)) level 0/");
												call prxchange(RegularExpressionId1, -1, parameter);
												call prxchange(RegularExpressionId2, -1, parameter);
												output;
											run;
											quit;
										%end;

									%end;

									data %sysfunc(cats(_madareg_cv&I, _summary_&k));
										length Parameter $ 45;
										set _temp_msens %sysfunc(cats(_madareg_cv&I, _ae_cv0_ds&k));
									run;
									quit;

									proc datasets nodetails nolist;
										delete _temp_msens;
									run;
									quit;

								%end;

								%if &j = 1 %then %do;
									%if &I <= %sysfunc(countw(&covariate)) %then %do;
										data %sysfunc(cats(_madareg_cv&I, _ae_cv&l&sub&k));
											set %sysfunc(cats(_madareg_cv&I, _ae_cv&l&sub&k));
											if parameter="%sysfunc(cats(msens_&I,1))" then call symput('SeID', id);
										run;
										quit;

										%let idsens = %eval(&SeID -1); /* redo id for sens level 0 for ordering later */
										%let idspec = %eval(&SeID + &&nlevels&I); /* redo id for spec level 0 for ordering later */
									%end;	
									%else %do;
										data %sysfunc(cats(_madareg_cv&I, _ae_cv&l&sub&k));
											set %sysfunc(cats(_madareg_cv&I, _ae_cv&l&sub&k));
											if parameter="%sysfunc(cats(msens_%eval(&I-1),1))" then call symput('SeID', id);
										run;
										quit;

										%let idsens = %eval(&SeID -1); /* redo id for sens level 0 for ordering later */
										%let idspec = %eval(&SeID + &&nlevels%eval(&I-1)); /* redo id for spec level 0 for ordering later */
									%end;
								%end;

								%if &j > 0 %then %do;
									%if %upcase(&&cvtype&y)= CON %then %do;
										data %sysfunc(cats(_madareg_cv&I, _ae_cv&l&sub&k));
											set %sysfunc(cats(_madareg_cv&I, _ae_cv&l&sub&k));
											RegularExpressionId1 = prxparse("s/msens_&y/Sensitivity for a unit change in %sysfunc(scanq(&covariate, &y))/");
											RegularExpressionId2 = prxparse("s/mspec_&y/Specificity for unit change in %sysfunc(scanq(&covariate, &y))/");
											call prxchange(RegularExpressionId1, -1, parameter);
											call prxchange(RegularExpressionId2, -1, parameter);
											output;
										run;
										quit;
									%end;
									%else %do;
										data %sysfunc(cats(_madareg_cv&I, _ae_cv&l&sub&k));
											set %sysfunc(cats(_madareg_cv&I, _ae_cv&l&sub&k));
											RegularExpressionId1 = prxparse("s/msens_&y/Sensitivity: %sysfunc(scanq(&covariate, &y)) level /");
											RegularExpressionId2 = prxparse("s/mspec_&y/Specificity: %sysfunc(scanq(&covariate, &y)) level /");
											call prxchange(RegularExpressionId1, -1, parameter);
											call prxchange(RegularExpressionId2, -1, parameter);
											output;
										run;
										quit;
									%end;

									data %sysfunc(cats(_madareg_cv&I, _summary_&k)) ;
										set %sysfunc(cats(_madareg_cv&I, _summary_&k)) %sysfunc(cats(_madareg_cv&I, _ae_cv&l&sub&k));
									run;
									quit;
								%end;
								%if %upcase(&model)=RE %then %do;
									/* only keep the first 6 observations in this data set,
									i.e., the HSROC model parameters */
									data %sysfunc(cats(_madareg_cv&I, _ae_cv&l&sub&k)) (keep=parameter estimate standarderror z probz lower upper);
										set %sysfunc(cats(_madareg_cv&I, _ae_cv&l&sub&k));
										where orderno < 6;
									run;
									quit;
								%end;
							%end;
							%else %do;
								%put %str(MODEL FAILURE: Additional estimates for &bylevelname with covariate &covariate
									were not produced. This may be due to failure to converge or other issues with the model and/or data.);
							%end;
						%end;
					%end;
					%let x = %eval(&x + &&nlevels&y + 1);
					%let y = %eval(&y + 1);
				%end;
			
				/* delete these intermediate tables */
				%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&I, _ae_rel_ds&k)))) %then %do;
					proc datasets memtype=data nodetails nolist;
						delete %sysfunc(cats(_madareg_cv&I, _ae_rel_ds&k));
					run;
					quit;
				%end;
				%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&I, _ae_all_ds&k)))) %then %do;
					proc datasets memtype=data nodetails nolist;
						delete %sysfunc(cats(_madareg_cv&I, _ae_all_ds&k));
					run;
					quit;
				%end;
				%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&I, _pe_all_ds&k)))) %then %do;
					proc datasets memtype=data nodetails nolist;
						delete %sysfunc(cats(_madareg_cv&I, _pe_all_ds&k));
					run;
					quit;
				%end;

				/* order the summary table as requested by the user
				STAT (the default) will order by the summary statistic while level
				just orders by the level of the covaraite so all stats
				grouped together for each level */
				%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&I, _summary_&k)))) %then %do;

					/* get exponents of the true positve and true negative log odds ratios
					logDOR, logRDOR, log relative sensitivity and specificity, logLR- and logLR+
					and rename them by searching for log and replacing with nothing. */
					data %sysfunc(cats(_tb_cv&I, _summary_temp));
						set %sysfunc(cats(_madareg_cv&I, _summary_&k));
						RegularExpressionId = prxparse('s/log |log//');
						call prxchange(RegularExpressionId, -1, parameter);
						expestimate=exp(estimate);
						explower=exp(lower);
						expupper=exp(upper);
						where parameter contains 'True' or parameter contains 'logL' or parameter contains 'logD' or parameter contains 'logR';
						rename Estimate = LogEstimate;
						rename lower = LogLower;
						rename upper = LogUpper;
						output;
					run;
					quit;

					data %sysfunc(cats(_tb_cv&I, _summary_temp)) (drop=LogEstimate LogLower LogUpper);
						set %sysfunc(cats(_tb_cv&I, _summary_temp));
						rename 	expestimate = Estimate
								explower = Lower
								expupper = Upper;
					run;
					quit;

					data %sysfunc(cats(_madareg_cv&I, _summary_&k));
						set %sysfunc(cats(_madareg_cv&I, _summary_&k));
						if parameter='Sensitivity: level 0' or parameter='Sensitivity: at 0' then id = resolve('&idsens');
						else if parameter='Specificity: level 0' or parameter='Specificity: at 0' then id = resolve('&idspec');
					run;
					quit;	

					/* get the inverse transformation of logit sensitivity and specificity */
					data %sysfunc(cats(_tb_cv&I, _summary_temp2));
						set %sysfunc(cats(_madareg_cv&I, _summary_&k));
						invlogitestimate=exp(estimate)/(1 + exp(estimate));
						invlogitlower=exp(lower)/(1 + exp(lower));
						invlogitupper=exp(upper)/(1 + exp(upper));
						where parameter contains 'Spec' or parameter contains 'Sens';
						sensspec=1;
						rename Estimate = LogitEstimate;
						rename Lower = LogitLower;
						rename Upper = LogitUpper;
						output;
					run;
					quit;

					
					data  %sysfunc(cats(_tb_cv&I, _summary_temp2)) (drop=LogitEstimate LogitLower LogitUpper);
						set  %sysfunc(cats(_tb_cv&I, _summary_temp2));
						rename 	invlogitestimate = Estimate
								invlogitlower = Lower
								invlogitupper = Upper;
					run;
					quit;

					/* mark the log estimates so they can be deleted from the data set eventually
					need to do it this way because can't use contains with if statement so create a temporary
					data set and variable for the records to be deleted and then merge with the summary data set */
					data _tb_temp_&I&k;
						set %sysfunc(cats(_madareg_cv&I, _summary_&k));
						fordelete=1;
						where parameter contains 'True' or parameter contains 'logL'
							or parameter contains 'logD' or parameter contains 'logR'
							or parameter contains 'Sensitivity' or parameter contains 'Specificity';
					run;
					quit;

					proc sort data = _tb_temp_&I&k;
						by id;
					run;

					proc sort data = %sysfunc(cats(_madareg_cv&I, _summary_&k));
						by id;
					run;

					data %sysfunc(cats(_madareg_cv&I, _summary_&k));
						merge %sysfunc(cats(_madareg_cv&I, _summary_&k)) _tb_temp_&I&k;
						by id;
					run;
					quit;

					/* delete the temporary data set */
					%if %sysfunc(exist(_tb_temp_&I&k)) %then %do;
						proc datasets memtype=data nodetails nolist;
							delete _tb_temp_&I&k;
						run;
						quit;
					%end;
					
					%if %upcase(&model) = RE %then %do;
						/* delete log estimates of parameters */
						data %sysfunc(cats(_madareg_cv&I, _summary_&k));
							set %sysfunc(cats(_madareg_cv&I, _summary_&k));
							if fordelete=1 then delete;
						run;
						quit;

						proc sort data = %sysfunc(cats(_madareg_cv&I, _summary_&k)) nodupkey;
							by parameter;
						run;
						quit;

						/* append the transformed estimates to the rest of the summary data set */
						data %sysfunc(cats(_madareg_cv&I, _summary_&k));
							set %sysfunc(cats(_madareg_cv&I, _summary_&k)) %sysfunc(cats(_tb_cv&I, _summary_temp2)) %sysfunc(cats(_tb_cv&I, _summary_temp));
						run;
						quit;
					%end;
					%else %do;
					/* append the transformed estimates to the rest of the summary data set */
						data %sysfunc(cats(_madareg_cv&I, _summary_&k));
							set %sysfunc(cats(_tb_cv&I, _summary_temp2)) %sysfunc(cats(_tb_cv&I, _summary_temp));
						run;
						quit;
					%end;

					/* delete temporary data sets */
					%if %sysfunc(exist(%sysfunc(cats(_tb_cv&I, _summary_temp)))) %then %do;
						proc datasets memtype=data nodetails nolist;
							delete %sysfunc(cats(_tb_cv&I, _summary_temp));
						run;
						quit;
					%end;

					%if %sysfunc(exist(%sysfunc(cats(_tb_cv&I, _summary_temp2)))) %then %do;
						proc datasets memtype=data nodetails nolist;
							delete %sysfunc(cats(_tb_cv&I, _summary_temp2));
						run;
						quit;
					%end;

					/* If group is specified, items are listed in the table according
					to covariate level. If stat is specified, items are listed
					according to summary statistic such that all levels of the
					covariate are grouped together for the statistic. */
					%if %upcase(&cvsummorder)= STAT %then %do;
						proc sort data = %sysfunc(cats(_madareg_cv&I, _summary_&k));
							by id parameter;
						run;
						quit;
					%end;
					%else %if %upcase(&cvsummorder)= LEVEL or &cvsummorder = %str() %then %do;
						proc sort data = %sysfunc(cats(_madareg_cv&I, _summary_&k));
							by cvgroup;
						run;
						quit;
					%end;

					/* create data set for summary statistics  */
					%if %upcase(&model) = RE %then %do;
						data %sysfunc(cats(_madareg_cv&I, _statsummary_&k)) (keep=Parameter Estimate Lower Upper);
							set %sysfunc(cats(_madareg_cv&I, _summary_&k));
							where (orderno > 5 and relativestat=.) or sensspec=1;
						run;
						quit;
					%end;
					%else %do;
						data %sysfunc(cats(_madareg_cv&I, _statsummary_&k)) (keep=Parameter Estimate Lower Upper);
							set %sysfunc(cats(_madareg_cv&I, _summary_&k));
							where  relativestat=. or sensspec=1;
						run;
						quit;
					%end;

					/* create data set for relative statistics*/
					data %sysfunc(cats(_madareg_cv&I, _relsummary_&k))(keep=Parameter Estimate Lower Upper Probz);
						set %sysfunc(cats(_madareg_cv&I, _summary_&k));
						where relativestat=1;
					run;
					quit;

					/*%if %upcase(&&cvtype&I)= CON %then %do;
						data %sysfunc(cats(_madareg_cv&I, _relsummary_&k))(drop=RegularExpressionId1);
							set _madareg_cv_relsummary_&k;
							RegularExpressionId1 = prxparse("s/ 1//");
							call prxchange(RegularExpressionId1, -1, parameter);
						run;
					%end;*/

					%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&I, _summary_&k)))) %then %do;
						proc datasets memtype=data nodetails nolist;
							delete %sysfunc(cats(_madareg_cv&I, _summary_&k));
						run;
						quit;
					%end;
				%end;
			%end;
		%end;
	%end;

	/* if macro execution is not terminated prematurely due to an error of
	some sort then set global variable madareg to 1, i.e., successful execution so far */
	%let madareg=1;

%mend;


/*************************************************************
*															*
*															*
*				DRAW THE FOREST PLOT						*
*															*
*															*
*************************************************************/
%macro forest(
	cltype = , 
	covariate = , 
	subject = , 
	cialpha = ,
	gtitle = ,
	pmin=,
	pmax =,
	gwidth=,
	gheight=,
	dp=);

	%if %upcase(&cltype) = AC %then %do;
		/*Agresti coull*/
		data _madareg_ds&k;
			set _madareg_ds&k;
			ntilde = n + (quantile('NORMAL', &cialpha))**2;
			truetilde = true + ((quantile('NORMAL', &cialpha))**2)/2;
			ptilde = truetilde/ntilde;
			p = true/n;
			upper = ptilde + (quantile('NORMAL', &cialpha)*(ptilde*(1 - ptilde)/ntilde)**0.5);
			lower = ptilde - (quantile('NORMAL', &cialpha)*(ptilde*(1 - ptilde)/ntilde)**0.5);
		run;
		quit;
	%end;
	%else %if %upcase(&cltype) = W %then %do;
		/* Wilson/Score */
		data _madareg_ds&k;
			set _madareg_ds&k;
			p = true/n;
			upper = (p + ((quantile('NORMAL', &cialpha))**2)/(2*n) + (quantile('NORMAL', &cialpha))*(((p*(1 - p) + ((quantile('NORMAL', &cialpha))**2)/(4*n))/n)**0.5))/(1 + ((quantile('NORMAL', &cialpha))**2)/n);
			lower = (p + ((quantile('NORMAL', &cialpha))**2)/(2*n) - (quantile('NORMAL', &cialpha))*(((p*(1 - p) + ((quantile('NORMAL', &cialpha))**2)/(4*n))/n)**0.5))/(1 + ((quantile('NORMAL', &cialpha))**2)/n);
		run;
		quit;
	%end;
	%else %do;
	/*Clopper - Pearson*/
		data _madareg_ds&k;
			set _madareg_ds&k;
			p = true/n;
			if true = 0 then 
				lower = 0;
			else
				lower = (1 + (n - true + 1)/(true * quantile('F', &cialpha/2, 2 * true, 2 * (n - true + 1))))**(-1);

			if true = n and true > 0 then 
				upper = 1;
			else
				upper = (1 + (n - true)/((true + 1) * quantile('F', 1 - &cialpha/2, 2 * (true + 1), 2 * (n - true))))**(-1);
		run;
		quit;
	%end;

	/*Obtain the minimum and max if not supplied*/
	%if &pmin = %str() or &pmax = %str() %then %do;
		proc means nolabels noprint data=_madareg_ds&k(keep = lower upper)  Min Max;
   			output out=MinMaxCols (keep = lower upper);
		run;
		quit;

		proc iml;
			use MinMaxCols;
			read all var _NUM_ into X[c=varNames];
			close MinMaxCols;
		
			min = X[ 2, ><];    
			max = X[ 3, <>];

			min = max(0, min - 0.0005); 
			max = min(1, max + 0.0005);
			
		 	call symputx("min", min);
			call symputx("max", max); 
		quit;
		
		%if &pmin = %str() %then 
			%let pmin = &min; 
		%if &pmax = %str() %then 
			%let pmax = &max;
	%end;

	%if &covariate ~= %str() %then %do;
		%do j = 0 %to &&nlevels&c;

		/*With covariate*/
			proc transpose data = _madareg_ds&k out=_madareg_p (keep = Study_id p1 p0) prefix=p;
			    by &covariate &subject;
			    id sens;
			    var p;
				where cvlevels&c = &j;
			run;
			quit;

			proc transpose data = _madareg_ds&k out=_madareg_lower (keep = Study_id lower1 lower0 ) prefix=lower;
			    by &covariate &subject ;
			    id sens;
			    var lower;
				where cvlevels&c = &j;
			run;
			quit;

			proc transpose data = _madareg_ds&k out=_madareg_upper (keep = Study_id upper1 upper0 ) prefix=upper;
			    by &covariate &subject ;
			    id sens;
			    var upper;
				where cvlevels&c = &j;
			run;
			quit;

			data _madareg_forest_cv&c&j;
				merge _madareg_p _madareg_lower _madareg_upper;
				format  p1 p0 lower1 lower0 upper1 upper0 5.&dp;
				length Study $ 50;
				Study = right(&subject);
				constant = 1;
				_group = 1;
				cv = &j;
			run;
			quit;

			data subgroup;
				set %sysfunc(cats(_madareg_cv&c, _statsummary_&k));
				length Study $ 50;
				where parameter = "Sensitivity: &covariate level &j" or parameter = "Specificity: &covariate level &j";
				if parameter = "Sensitivity: &covariate level &j" then sens = 1;
					else sens = 0;
				rename estimate = p;
				Study = right("&covariate: level &j");
			run;
			quit;

			proc transpose data=subgroup out=subgroup_p (keep = Study op1 op0) prefix = op;
				by Study;
				id sens;
				var p;
			run;
			quit;

			proc transpose data=subgroup out=subgroup_lower (keep = Study olower1 olower0) prefix = olower;
				by Study;
				id sens;
				var lower;
			run;
			quit;

			proc transpose data=subgroup out=subgroup_upper (keep = Study oupper1 oupper0) prefix = oupper;
				by Study;
				id sens;
				var upper;
			run;
			quit;

			data subgroup;
				merge subgroup_p subgroup_lower subgroup_upper;
				format  op1 op0 olower1 olower0 oupper1 oupper0 5.&dp;
				constant = 1;
				_group = 2;
				cv = &j;
			run;
			quit;
			
			%if &j = 0 %then %do;
				data _madareg_forest;
					set _madareg_forest_cv&c&j subgroup;
				run;
				quit;
			%end;
			%else %do;
				data _madareg_forest;
					set _madareg_forest _madareg_forest_cv&c&j subgroup;
				run;
				quit;
			%end;
		%end;
	%end;
	%else %do;
	/*Without covariate*/
		proc transpose data = _madareg_ds&k out=_madareg_p (keep = Study_id p1 p0) prefix=p;
		    by &subject;
		    id sens;
		    var p;
		run;
		quit;

		proc transpose data = _madareg_ds&k out=_madareg_lower (keep = Study_id lower1 lower0 ) prefix=lower;
		    by &subject ;
		    id sens;
		    var lower;
		run;
		quit;

		proc transpose data = _madareg_ds&k out=_madareg_upper (keep = Study_id upper1 upper0 ) prefix=upper;
		    by &subject ;
		    id sens;
		    var upper;
		run;
		quit;

		data _madareg_forest;
			merge _madareg_p _madareg_lower _madareg_upper;
			format  p1 p0 lower1 lower0 upper1 upper0 5.&dp;
			Study = right(&subject);
			constant = 1;
			_group = 1;
			cv = 1;
		run;
		quit;

	%end;

	/*Overall*/
	data overall;
			set _madareg_a_tmp_summst_&k;
			where parameter = "Sensitivity" or parameter = "Specificity";
			if parameter = "Sensitivity" then sens = 1;
				else sens = 0;
			rename estimate = p;
			Study = right("Overall");
		run;
		quit;

		proc transpose data=overall out=overall_p (keep = Study op1 op0) prefix = op;
			by Study;
			id sens;
			var p;
		run;
		quit;

		proc transpose data=overall out=overall_lower (keep = Study olower1 olower0) prefix = olower;
			by Study;
			id sens;
			var lower;
		run;
		quit;

		proc transpose data=overall out=overall_upper (keep = Study oupper1 oupper0) prefix = oupper;
			by Study;
			id sens;
			var upper;
		run;
		quit;

		data overall;
			merge overall_p overall_lower overall_upper;
			format  op1 op0 olower1 olower0 oupper1 oupper0 5.&dp;
			constant = 1;
			_group = 3;
			call symput('OverallSens', op0);
			call symput('OverallSpec', op1);
		run;
		quit;

	data _madareg_forest;
		set _madareg_forest overall;
		yid = _n_;
	run;
	quit;

	data _null_;
	  pct=1/nobs;
	  call symputx("pct", pct);
	  set _madareg_forest nobs=nobs;
	run;
	quit;

	ods graphics / antialias=off reset imagename="ForestPlot" imagefmt=PNG;
	proc sgrender data=_madareg_forest template=ForestPlot; 
	  dynamic _pct=&pct;
	run;
	quit;
%mend;

proc template;
	  define statgraph ForestPlot; 
	  dynamic  _pct;
	  mvar gtitle subject;
	  nmvar overallSens overallSpec pmin pmax gwidth gheight; 
	  begingraph / designwidth=gwidth designheight=gheight;
	  entrytitle gtitle / pad=(bottom=5px);
	  	layout lattice / columns = 5 columngutter=0 columndatarange=union rowdatarange=union columnweights=(.2 .25 0.15 .25 .15);  
			rowaxes;
		  		rowaxis /display=none griddisplay=off reverse=true type=discrete; 
			endrowaxes;

			layout overlay /walldisplay=none border=false
	           xaxisopts=(display=none offsetmin=0.15 offsetmax=0.15)
				yaxisopts = (type=discrete );

				entry  halign=center subject  /  location=outside valign=top; 

				scatterplot y=yid x=eval(constant*1) / markercharacter = Study;

			endlayout;
			
			layout overlay / xaxisopts = ( linearopts=(Viewmin=pmin viewmax=pmax) label = "Sensitivity") 
							yaxisopts = (type=discrete);

				referenceline x=overallsens /  lineattrs=(pattern=shortdash) datatransparency=0.25;
				scatterplot y=yid x=p1 / markerattrs=(symbol = squarefilled size=2pct) ;
				highlowplot y = yid low = Lower1 high = Upper1 /type=line lineattrs = (thickness = 1 pattern=solid);
				scatterplot y = yid x=op1 / markerattrs=(symbol=diamondfilled size=20) group=_group 
	                      includemissinggroup=true index=_group;		    
				highlowplot y = yid low = oLower1 high = oUpper1 /type=line lineattrs = (thickness = 1 pattern=solid) group=_group index=_group;
			endlayout;

			layout overlay /walldisplay=none border=false
	           xaxisopts=(display=none offsetmin=0.15 offsetmax=0.15)
				yaxisopts = (type=discrete );

				entry  halign=left "SE" halign=center "LCL" halign=right "UCL" /  location=outside valign=top; 

				scatterplot y=yid x=eval(constant*1) / markercharacter=p1 ;
				scatterplot y=yid x=eval(constant*2) / markercharacter=lower1 ;
				scatterplot y=yid x=eval(constant*3) / markercharacter=upper1 ;

				scatterplot y=yid x=eval(constant*1) / markercharacter = op1 markercharacterattrs=(weight=bold) group=_group;
			  	scatterplot y=yid x=eval(constant*2) / markercharacter = olower1 markercharacterattrs=(weight=bold) group=_group;
				scatterplot y=yid x=eval(constant*3) / markercharacter = oupper1 markercharacterattrs=(weight=bold) group=_group;
			endlayout;

			layout overlay / xaxisopts = (linearopts=(Viewmin=pmin viewmax=pmax) label = "Specificity")
				yaxisopts = (type=discrete ); 

				referenceline x=overallSpec /  lineattrs=(pattern=shortdash) datatransparency=0.25;
				scatterplot y=yid x=p0 / markerattrs=(symbol=squarefilled size=2pct) ;
				highlowplot y = yid low=Lower0 high=Upper0 /type=line lineattrs = (thickness = 1 pattern=solid);
				scatterplot y = yid x=op0 / markerattrs=(symbol=diamondfilled size=20) group=_group 
	                      includemissinggroup=true index=_group;			
				highlowplot y = yid low=oLower0 high=oUpper0 /type=line lineattrs = (thickness = 1 pattern=solid) group=_group index=_group;
			endlayout;

			layout overlay / walldisplay=none border=false
	           xaxisopts=(display=none offsetmin=0.15 offsetmax=0.15)
				yaxisopts = (type=discrete );

	      		entry  halign=left "SP" halign=center "LCL" halign=right "UCL" /  location=outside valign=top; 

		      	scatterplot y=yid x=eval(constant*1) / markercharacter = p0;
			  	scatterplot y=yid x=eval(constant*2) / markercharacter = lower0 ;
				scatterplot y=yid x=eval(constant*3) / markercharacter = upper0 ;

				scatterplot y=yid x=eval(constant*1) / markercharacter = op0 markercharacterattrs=(weight=bold) group=_group;
			  	scatterplot y=yid x=eval(constant*2) / markercharacter = olower0 markercharacterattrs=(weight=bold) group=_group;
				scatterplot y=yid x=eval(constant*3) / markercharacter = oupper0 markercharacterattrs=(weight=bold) group=_group;
			endlayout;
		endlayout;
	  endgraph;
	  end;
	run;
quit;

/********************************************************************
*																	*
* 						CREATE WORD OUTPUT							*
*																	*
********************************************************************/


%macro printtodoc(
	covariate =,
	cvtype =,
	cveffect =,
	formatlr =,
	byvar =,
	mtitle =,
	subject =,
	info =,
	dtfile =,
	cialpha =,
	checkmod =,
	predict =,
	incbasic =,
	rfile =,
	relax =,
	graph = ,
	cltype = ,
	gtitle = ,
	pmin=,
	pmax =,
	gwidth = ,
	gheight =,
	gdpi = ,
	dp =,
	model = );

	/* set global variable madareg to 0 to begin with */
	%let madareg=0;

	%put %str(******************************);
	%put %str(CREATE WORD OUTPUT FOR RESULTS);
	%put %str(******************************);

	/* want no title in the document header */
	title ' ';

	/* set up model type and alternative model for use with headers
	and info */

	%let modelname = %str(&typem);
	%let altmodelname = %str(HSROC);

	ods noresults; /* prevents results from appearing within SAS viewer */

	/* use the SAS output delivery system to output results */
	options orientation=portrait pageno = 1;
	ods listing close;
	%if &rfile ~= %str() %then %do;
		ods rtf file=&rfile;
	%end;
	ods rtf startpage=no;
	ods escapechar = '^';
	ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=5} %upcase(&mtitle)";
	ods rtf text="^S={outputwidth=100%} ";
	ods rtf text="^S={outputwidth=100%} ";

	%if %upcase(&graph) ~= N %then %do;
		ods rtf image_dpi=&gdpi;
	%end;

	/* create template for the document */
	proc template;
		define style madaregstyle;
			parent = styles.rtf;
			style body from body/
				bottommargin  = 2 cm
				topmargin = 2 cm
				rightmargin = 1 cm
				leftmargin = 1 cm;

		    parent = styles.analysis;
    		style GraphFonts from GraphFonts                                                      
		      "Fonts used in graph styles" /  
		      'GraphTitleFont' = (", ",10pt,bold)
		      'GraphLabelFont' = (", ",8pt) 
		      'GraphValueFont' = (", ",8pt)
		      'GraphDataFont' = (", ",8pt);
		end;
	run;

	ods rtf style = madaregstyle;

	/* put all information at beginining so that it is not repeated k times */
	%if %upcase(info) ~= N %then %do;

		ods rtf text="^S={outputwidth=100%} ";
		ods rtf text="^S={outputwidth=100% just=l font_weight=bold font_size=4} Analysis Information";
		%if &dtfile ~=%str() and %upcase(&import) ~= N %then %do;
			ods rtf text="^S={outputwidth=100% just=l font_size=3} Data: %str(&dtfile)";
		%end;
		%else %if &dsname ~=%str() and %upcase(&import) = N %then %do;
			ods rtf text="^S={outputwidth=100% just=l font_size=3} Input data set: &dsname";
		%end;

		%if &byvar ~=%str() %then %do;
			ods rtf text="^S={outputwidth=100% just=l font_size=3} BY variable: &byvar";
		%end;

		%let cinterval= %sysevalf((1-&cialpha) * 100);
		ods rtf text="^S={outputwidth=100% just=l font_size=3} Confidence Interval: &cinterval%";
		
		%if &covariate ~= %str() %then %do;
			%do c = 1 %to %sysfunc(countw(&covariate));
				%if %sysfunc(exist(_madareg_&&covariate&c)) and %upcase(&&cvtype&c)~= CON %then %do;

					data _madareg_&&covariate&c (keep=&&covariate&c cvlevels&c);
						set _madareg_&&covariate&c;
						label cvlevels&c = 'Level';
					run;

					proc sort data=_madareg_&&covariate&c;
						by cvlevels&c;
					run;

					%if &c = 1 %then %do;
						ods rtf text="^S={outputwidth=100% just=c font_size=3} Covariate Information";
					%end;

					proc print data= _madareg_&&covariate&c noobs label;
					run;

				%end;
				%else %if %upcase(&&cvtype&c)= CON and &&covariate&c ~= %str() %then %do;
					%if &c = 1 %then %do;
						ods rtf text="^S={outputwidth=100% just=l font_size=3} Covariate: &&covariate&c";
					%end;
					%else %do;
						ods rtf text="^S={outputwidth=100% just=l font_size=3} \t: &&covariate&c";
					%end;
				%end;
			%end;
		%end;		
	%end;

	%do k=1 %to &bylevels;

		/* if there is a BY variable get the name of the current by category.*/
		%if &byvar ~= %str() %then %do;
			data _null_;
			   	set _madareg_&byvar;
				if bylevels=&k then call symput('bylevelname', &byvar);
			run;
		%end;
		%else 
			%let bylevelname= %str();


		%if &k > 1 %then %do;
			/* force a page break after each level of the BY variable*/
			ods rtf startpage=now;
		%end;

		%if &k = 1 %then %do;

			ods rtf text="^S={outputwidth=100%} ";
		%end;

		/* output the model with no covariate only if incbasic is not equal to no (n) */
		%if %upcase(&incbasic)~= N %then %do;

			ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=5} &modelname &model model basic analysis &bylevelname";

			/* Starting printing tables to output file : 19th May, suppress the starting values
			%if %sysfunc(exist(_madareg_a_sv_&k)) %then %do;
				ods rtf text="^S={outputwidth=100%} ";
				ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} Starting values";
				proc print data=_madareg_a_sv_&k noobs;
				run;
			%end;
			*/

			%if %sysfunc(exist(_madareg_a_convgstat_&k)) %then %do;
				ods rtf text="^S={outputwidth=100%} ";
				ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} Convergence status";
				proc print data=_madareg_a_convgstat_&k noobs;
				run;
			%end;

			%if %sysfunc(exist(_madareg_a_fit_&k)) %then %do;
				ods rtf text="^S={outputwidth=100%} ";
				ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} Model fit";
				proc print data=_madareg_a_fit_&k noobs label;
				run;
			%end;
			%else %do;
				ods rtf text="^S={outputwidth=100%} ";
				ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} The fit table was not created. This may be due to failure to converge.";
				ods rtf text="^S={outputwidth=100%} ";
				ods rtf text="^S={outputwidth=100%} ";
			%end;

			%if %sysfunc(exist(_madareg_a_pe_&k)) %then %do;
				ods rtf text="^S={outputwidth=100%} ";
				ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} &modelname &model model parameter estimates";
				proc print data=_madareg_a_pe_&k noobs label;
				run;

				data _nostderr_;
					set _madareg_a_pe_&k;
					where standarderror=.;
				run;

				%let numobs=0;
				%if %sysfunc(exist(_nostderr_)) %then %do;
					data _null_;
			   			set _nostderr_ nobs = numobs;
						call symput('numobs',numobs);
					run;
				%end;
				%let nostderr=&numobs;

				proc datasets memtype=data nodetails nolist;
					delete 	_nostderr_;
				run;
				quit;

				/* checking for correlation of +1 or -1 for bivariate model */
				%if &relax ~= Y %then %do;
					%let corr = 0;
					data _null_;
					   	set _madareg_a_pe_&k;
						if parameter='Corr(logits)' then call symput('corr', put(Estimate, best12.));
					run;

					/* notify user of missing standard errors and that model may be unreliable */
					%if %sysfunc(abs(&corr)) = 1 %then %do;
						ods rtf text="^S={outputwidth=100% just=l font_weight=bold font_size=3} NOTE: Between study correlation of +1 or -1.";
						ods rtf text="^S={outputwidth=100% just=l font_weight=bold font_size=3} There may be insufficient information to estimate the correlation and pooled estimates may be unstable.";
						ods rtf text="^S={outputwidth=100%} ";
					%end;
				%end;

				/* notify user of missing standard errors and that model may be unreliable */
				%if &nostderr > 0 %then %do;
					ods rtf text="^S={outputwidth=100% just=l font_weight=bold font_size=3} WARNING: Standard error missing (.) for &nostderr estimate(s) in the table. Pooled estimates may be unstable.";
					ods rtf text="^S={outputwidth=100%} ";
					ods rtf text="^S={outputwidth=100%} ";
				%end;
			%end;
			%else %do;
				ods rtf text="^S={outputwidth=100%} ";
				ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} Parameters were not estimated. This may be due to failure to converge or other issues with the model and/or data.";
				ods rtf text="^S={outputwidth=100%} ";
				ods rtf text="^S={outputwidth=100%} ";
			%end;
			
			%if &model = RE %then %do;
				%if %sysfunc(exist(_madareg_a_tmp_statpms_&k)) %then %do;

					ods rtf text="^S={outputwidth=100%} ";
					ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} &altmodelname &model model parameter estimates";
					proc print data=_madareg_a_tmp_statpms_&k noobs label;
					run;

					/* checking for missing standard errors */
					data _nostderr_;
						set _madareg_a_tmp_statpms_&k;
						where standarderror=. and (lower=. or upper=.);
					run;

					%let numobs=0;
					%if %sysfunc(exist(_nostderr_)) %then %do;
						data _null_;
							set _nostderr_ nobs = numobs;
							call symput('numobs',numobs);
						run;
					%end;
					%let nostderr=&numobs;

					proc datasets memtype=data nodetails nolist;
						delete 	_nostderr_;
					run;
					quit;


					/* checking for correlation of +1 or -1 for bivariate model */
					%if &relax ~= Y %then %do;
						%let corr = 0;
						data _null_;
							set _madareg_a_tmp_statpms_&k;
							if parameter='Corr(logits)' then call symput('corr', put(Estimate, best12.));
						run;

						/* notify user of missing standard errors and that model may be unreliable */
						%if %sysfunc(abs(&corr)) = 1 %then %do;
							ods rtf text="^S={outputwidth=100% just=l font_weight=bold font_size=3} NOTE: Between study correlation of +1 or -1.";
							ods rtf text="^S={outputwidth=100% just=l font_weight=bold font_size=3} There may be insufficient information to estimate the correlation and pooled estimates may be unstable.";
							ods rtf text="^S={outputwidth=100%} ";
						%end;
					%end;

					%if &nostderr > 0 %then %do;
						ods rtf text="^S={outputwidth=100% just=l font_weight=bold font_size=3} WARNING: Standard error missing (.) for &nostderr estimate(s) in the table. Pooled estimates may be unstable.";
						ods rtf text="^S={outputwidth=100%} ";
						ods rtf text="^S={outputwidth=100%} ";
					%end;

				%end;
				%else %do;
					ods rtf text="^S={outputwidth=100%} ";
					ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} &altmodelname &model model parameters were not estimated. This may be due to failure to converge or other issues with the model and/or data.";
					ods rtf text="^S={outputwidth=100%} ";
					ods rtf text="^S={outputwidth=100%} ";
				%end;
			%end;

			%if %sysfunc(exist(_madareg_a_tmp_se_&k)) %then %do;
				ods rtf text="^S={outputwidth=100%} ";
				ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} Confidence and prediction region parameters";
				proc print data=_madareg_a_tmp_se_&k noobs label;
				run;
				quit;
			%end;

			%if %sysfunc(exist(_madareg_a_tmp_summst_&k)) %then %do;
				ods rtf text="^S={outputwidth=100%} ";
				ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} Summary estimates of test accuracy measures";
				proc print data=_madareg_a_tmp_summst_&k noobs label;
				run;
				data _nostderr_;
					set _madareg_a_tmp_summst_&k;
					where lower=. or upper=.;
				run;

				%let numobs=0;
				%if %sysfunc(exist(_nostderr_)) %then %do;
					data _null_;
			   			set _nostderr_ nobs = numobs;
						call symput('numobs',numobs);
					run;
				%end;
				%let nostderr=&numobs;

				proc datasets memtype=data nodetails nolist;
					delete 	_nostderr_;
				run;
				quit;

				/* notify user of missing standard errors and that model may be unreliable */
				%if &nostderr > 0 %then %do;
					ods rtf text="^S={outputwidth=100% just=l font_weight=bold font_size=3} WARNING: Standard error missing (.) for &nostderr estimate(s) in the table. Pooled estimates may be unstable.";
					ods rtf text="^S={outputwidth=100%} ";
					ods rtf text="^S={outputwidth=100%} ";
				%end;
			%end;
			%else %do;
				ods rtf text="^S={outputwidth=100%} ";
				ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} Summary estimates of test accuracy measures were not produced. This may be due to failure to converge or other issues with the model and/or data.";
				ods rtf text="^S={outputwidth=100%} ";
				ods rtf text="^S={outputwidth=100%} ";
			%end;

			/* modify table of predicted value if prediction was requested and model ok*/
			%if %sysfunc(exist(_madareg_a_logitp_&k)) and %upcase(&predict) ~= N and %sysfunc(exist(_madareg_a_addest_&k)) %then %do;

				data _predsens_ds&k(keep= &subject &covariate sensobs senspred lowerse upperse);
					set _madareg_a_logitp_&k;
					sensobs=true/n;
					senspred=exp(pred)/(1+exp(pred));
					lowerse=exp(lower)/(1+exp(lower));
					upperse=exp(upper)/(1+exp(upper));
					where sens=1;
				run;
				data _predspec_ds&k(keep= &subject &covariate specobs specpred lowersp uppersp);
					set _madareg_a_logitp_&k;
					specobs=true/n;
					specpred=exp(pred)/(1+exp(pred));
					lowersp=exp(lower)/(1+exp(lower));
					uppersp=exp(upper)/(1+exp(upper));
					where spec=1;
				run;

				proc sort data = _predsens_ds&k;
					by &subject;
				run;

				proc sort data = _predspec_ds&k;
					by &subject;
				run;

				data _madareg_a_predict_&k;
		   			merge _predsens_ds&k _predspec_ds&k;
		   			by &subject;
		   		run;

				data _madareg_a_predict_&k;
					set _madareg_a_predict_&k;
					label sensobs = 'Observed sensitivity'
						specobs = 'Observed specificity'
						senspred = 'Predicted sensitivity'
						specpred = 'Predicted specificity'
						lowerse = 'Lower confidence limit for predicted sensitivity'
						upperse = 'Upper confidence limit for predicted sensitivity'
						lowersp = 'Lower confidence limit for predicted specificity'
						uppersp = 'Upper confidence limit for predicted specificity';
				run;

				%if %sysfunc(exist(_predsens_ds&k)) %then %do;
					proc datasets memtype=data nodetails nolist;
						delete _predsens_ds&k;
					run;
					quit;
				%end;

				%if %sysfunc(exist(_predspec_ds&k)) %then %do;
					proc datasets memtype=data nodetails nolist;
						delete _predspec_ds&k;
					run;
					quit;
				%end;

				ods rtf text="^S={outputwidth=100%} ";
				ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} Predicted values of sensitivity and specificity based on parameter and empirical Bayes estimates";
				proc print data=_madareg_a_predict_&k noobs label;
				run;
			%end;


			/* if model	checking is yes, produce histogram and normal probability plot
				of the empirical Bayes estimates of the random effects */
			%if %sysfunc(exist(_madareg_a_addest_&k)) and %upcase(&checkmod) = Y %then %do;

				goptions goutmode=replace noborder;

				/*ods noptitle;*/
				ods rtf text="^S={outputwidth=100%} ";
				ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} Model checking - distribution of random effects";
				ods rtf text="^S={outputwidth=100% just=c font_size=4} Histograms and normal probability plots of the empirical Bayes estimates of the random effects (ua and ut, level two residuals)";
				ods rtf text="^S={outputwidth=100%} ";
				ods rtf text="^S={outputwidth=100%} ";

				proc sort data=_madareg_a_randeffs_&k;
					by effect;
				run;

				ods select HistRE HistRE1;
				proc univariate data=_madareg_a_randeffs_&k noprint;
					title1 " ";
					title3 "Histogram of random effects for &bylevelname";
				  	by effect;
				  	histogram estimate /normal
										cfill=ltgray
										nrows=1
										name = 'HistRE'
										noframe;
				run;

				ods rtf text="^S={outputwidth=100%} ";
				ods rtf text="^S={outputwidth=100%} ";

				ods select QQNRE QQNRE1;
				proc univariate data=_madareg_a_randeffs_&k noprint;
					title1 " ";
					title3 "Normal probability plot of random effects for &bylevelname";
				  	by effect;
				  	probplot estimate/normal
									square
									noframe
									name= 'QQNRE';
				run;
			%end;
		%end;

		title ' ';

		%if %sysfunc(exist(_madareg_a_summary_&k)) %then %do;
			proc datasets memtype=data nodetails nolist;
				delete _madareg_a_summary_&k;
			run;
			quit;
		%end;

		%if %upcase(&graph) ~= N and %sysfunc(exist(_madareg_a_tmp_summst_&k)) and &covariate = %str() %then %do;
			%forest(cltype = &cltype,  
				subject=&subject, 
				cialpha=&cialpha, 
				gtitle = &gtitle,
				pmin= &pmin,
				pmax = &pmax,
				gwidth= &gwidth,
				gheight= &gheight,
				dp=&dp);
		%end;

		%if &covariate ~= %str() %then %do;
		
			%if %eval(%sysfunc(countw(&covariate))) = 1 %then
				%let W = 1;
			%else
				%let W = %eval(&ncovariates + 1);
				
			%let numpars&W = 0;
			
			%do c = 1 %to &W;

				%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&c, _addest_&k)))) %then %do;

					%if %upcase(&incbasic) ~= N %then %do;

						/* want the output for model with covariate on a new page
						so force a page break now if there is output for basic model */
						ods rtf startpage=now;

					%end;
					%if &c <= &ncovariates %then %do;
						ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=5} &modelname &model analysis with covariate  &&covariate&c";
					%end;
					%else %do;
						ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=5} &modelname &model analysis with covariates &covariate";
					%end;
					/*Suppress starting values: 19th may 2017
					%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&c, _sv_&k)))) %then %do;
						ods rtf text="^S={outputwidth=100%} ";
						ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} Starting values";
						proc print data=%sysfunc(cats(_madareg_cv&c, _sv_&k)) noobs;
						run;
					%end;
					*/

					%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&c, _convgstat_&k)))) %then %do;
						ods rtf text="^S={outputwidth=100%} ";
						ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} Convergence status";
						proc print data=%sysfunc(cats(_madareg_cv&c, _convgstat_&k)) noobs;
						run;
					%end;

					%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&c, _fit_&k)))) %then %do;
						ods rtf text="^S={outputwidth=100%} ";
						ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} Model fit";
						proc print data=%sysfunc(cats(_madareg_cv&c, _fit_&k)) noobs label;
						run;

						/* get number of parameters so we can work out df for likelihood ratio test */
						%if &c <= &ncovariates %then %do;

							%if &&cveffect&c=%str()  %then %let plen = 2;
							%else %let plen=%sysfunc(lengthn(&&cveffect&c));

							%let numpars&c = %eval(&plen * &&nlevels&c);

							%if &W > 1 %then
								%let numpars&W = %eval(&&numpars&W + &&numpars&c);
						%end;
						
						/* get difference in -2LogL and compare with Chi-square distribution */
						%if %sysfunc(exist(_madareg_a_fit_&k)) and %sysfunc(exist(%sysfunc(cats(_madareg_cv&c, _addest_&k)))) %then %do;
							data _null_;
								set _madareg_a_fit_&k;
								if descr='-2 Log Likelihood' then call symput('_2logL1', Value);
								stop;
							run;
							data _null_;
								set %sysfunc(cats(_madareg_cv&c, _fit_&k));
								if descr='-2 Log Likelihood' then call symput('_2logL2', Value);
								stop;
							run;
							%let _2logLDiff = %sysfunc(abs(&_2logL1 - &_2logL2));
							%let upval= %sysfunc(probchi(&_2logLDiff, &&numpars&c));
							%let pval=%sysevalf(1 - &upval);

							%if &c <= &ncovariates %then %do;	
								ods rtf text="^S={outputwidth=100% just=l font_weight=bold font_size=4} Likelihood ratio test for &model model with and without &&covariate&c ";
							%end;
							%else %do;
								ods rtf text="^S={outputwidth=100% just=l font_weight=bold font_size=4} Likelihood ratio test for &model model with and without covariates: &covariate ";
							%end;
							/* format the -2log difference and p-value */
							%if %upcase(&formatlr)=N %then %do;
								ods rtf text="^S={outputwidth=100% just=l font_size=3} -2 log likelihood difference is &_2logLDiff on &&numpars&c degrees of freedom, p = &pval";
							%end;
							%else %do;
								%if &_2logLDiff >= 0.001 %then %let _2logld = %sysfunc(putn(&_2logLDiff, 6.3));
								%else %if &_2logLDiff < 0.001 %then %let _2logld =&_2logLDiff;
								%if %sysfunc(putn(&pval, 5.3)) >= 0.001 %then %do;
									%let pv= %sysfunc(putn(&pval, 5.3));
									%let fpval=%str(= &pv);
								%end;
								%else %if %sysfunc(putn(&pval, 5.3)) < 0.001 %then %let fpval= %str(<0.001);
								ods rtf text="^S={outputwidth=100% just=l font_size=3} -2 log likelihood difference is &_2logld on &&numpars&c degrees of freedom, p &fpval";
							%end;
							ods rtf text="^S={outputwidth=100%} ";
							ods rtf text="^S={outputwidth=100%} ";
						%end;
						%else %do;
							ods rtf text="^S={outputwidth=100%} ";
							ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} No likelihood ratio test because of failure to converge or other issues with one or both models.";
							ods rtf text="^S={outputwidth=100%} ";
							ods rtf text="^S={outputwidth=100%} ";
						%end;
					%end;
					%else %do;
						ods rtf text="^S={outputwidth=100%} ";
						ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} The fit table was not created. This may be due to failure to converge.";
						ods rtf text="^S={outputwidth=100%} ";
						ods rtf text="^S={outputwidth=100%} ";
					%end;

					/* modify table of predicted value if prediction was requested */
					%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&c, _logitp_&k)))) and %upcase(&predict) ~= N %then %do;

						data %sysfunc(cats(_predsens_cv&c, _ds&k)) (keep= &subject &&covariate sensobs senspred lowerse upperse);
							set %sysfunc(cats(_madareg_cv&c, _logitp_&k));
							sensobs=true/n;
							senspred=exp(pred)/(1+exp(pred));
							lowerse=exp(lower)/(1+exp(lower));
							upperse=exp(upper)/(1+exp(upper));
							where sens=1;
						run;
						data %sysfunc(cats(_predspec_cv&c, _ds&k)) (keep= &subject &&covariate specobs specpred lowersp uppersp);
							set %sysfunc(cats(_madareg_cv&c, _logitp_&k));
							specobs=true/n;
							specpred=exp(pred)/(1+exp(pred));
							lowersp=exp(lower)/(1+exp(lower));
							uppersp=exp(upper)/(1+exp(upper));
							where spec=1;
						run;


						proc sort data = %sysfunc(cats(_predsens_cv&c, _ds&k));
							by &subject;
						run;

						proc sort data = %sysfunc(cats(_predspec_cv&c, _ds&k));
							by &subject;
						run;

						data %sysfunc(cats(_madareg_cv&c, _predict_&k));
							merge %sysfunc(cats(_predsens_cv&c, _ds&k)) %sysfunc(cats(_predspec_cv&c, _ds&k));
							by &subject;
						run;

						data %sysfunc(cats(_madareg_cv&c, _predict_&k));
							set %sysfunc(cats(_madareg_cv&c, _predict_&k));
							label sensobs = 'Observed sensitivity'
								specobs = 'Observed specificity'
								senspred = 'Predicted sensitivity'
								specpred = 'Predicted specificity'
								lowerse = 'Lower confidence limit for predicted sensitivity'
								upperse = 'Upper confidence limit for predicted sensitivity'
								lowersp = 'Lower confidence limit for predicted specificity'
								uppersp = 'Upper confidence limit for predicted specificity';
						run;

						%if %sysfunc(exist(%sysfunc(cats(_predsens_cv&c, _ds&k)))) %then %do;
							proc datasets memtype=data nodetails nolist;
								delete %sysfunc(cats(_predsens_cv&c, _ds&k));
							run;
							quit;
						%end;

						%if %sysfunc(exist(%sysfunc(cats(_predspec_cv&c, _ds&k)))) %then %do;
							proc datasets memtype=data nodetails nolist;
								delete %sysfunc(cats(_predspec_cv&c, _ds&k));
							run;
							quit;
						%end;

						ods rtf text="^S={outputwidth=100%} ";
						ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} Predicted values of sensitivity and specificity based on parameter and empirical Bayes estimates";
						proc print data=%sysfunc(cats(_madareg_cv&c, _predict_&k)) noobs label;
						run;
					%end;

					%if %eval(&c <= %sysfunc(countw(&covariate))) %then %do;
						%let z = 1;
						%let y = &c;
					%end;
					%else %do;
						%let z = %sysfunc(countw(&covariate));
						%let y = 1;
					%end;

					%let x = 0;
					%do t = 1 %to &z;
						
						%if &t = 1 %then 
							%let b = 0;
						%else
							%let b = 1;
						
						%do j = &b %to &&nlevels&y;
						
							%if %eval(&t > 1) %then %do;
								%let l = &x;
							%end;
							%else %do;
								%let l = &j;
							%end;

							%if %upcase(&&cvtype&y)~= CON %then %do;
								data _null_;
									set _madareg_&&covariate&y;
									if cvlevels&y=&j then call symput('cvlevelname', &&covariate&y);
								run;
								%let name= %str(for &&covariate&y = %sysfunc(strip(&cvlevelname)));
							%end;
							%else %let name = %str(for &&covariate&y);
							
							%if &j = 0 and &z > 1 %then 
								%let name = %str(for &covariate = 0);
								
							%if &j > 0 and &z > 1 %then 
								%let name = %str(&name and other covariates = 0);

							%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&c, _cov_cv&l&sub&k)))) %then %do;
								%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&c, _pe_cv&l&sub&k)))) %then %do;
									ods rtf text="^S={outputwidth=100%} ";
									%if &z = 1 %then %do;
										ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} &modelname &model model parameter estimates &name";
									%end;
									%if &z > 1 %then %do;
										%if &j = 0 %then %do;
											ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} &modelname &model model parameter estimates ";
										%end;
										%else %do;
											ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} &modelname &model model parameter estimates &name ";
										%end;
									%end;
									proc print data=%sysfunc(cats(_madareg_cv&c, _pe_cv&l&sub&k)) noobs label;
									run;

									data _nostderr_;
										set %sysfunc(cats(_madareg_cv&c, _pe_cv&l&sub&k));
										where standarderror=.;
									run;

									%let numobs=0;
									%if %sysfunc(exist(_nostderr_)) %then %do;
										data _null_;
											set _nostderr_ nobs = numobs;
											call symput('numobs',numobs);
										run;
									%end;
									%let nostderr=&numobs;

									proc datasets memtype=data nodetails nolist;
										delete 	_nostderr_;
									run;
									quit;

									/* notify user of missing standard errors and that model may be unreliable */
									%if &nostderr > 0 %then %do;
										ods rtf text="^S={outputwidth=100% just=l font_weight=bold font_size=3} WARNING: Standard error missing (.) for &nostderr estimate(s) in the table. Pooled estimates may be unstable.";
										ods rtf text="^S={outputwidth=100%} ";
										ods rtf text="^S={outputwidth=100%} ";
									%end;
								%end;

								%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&c, _ae_cv&l&sub&k)))) and %upcase(&model)=RE %then %do;

									ods rtf text="^S={outputwidth=100%} ";
									ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} &altmodelname &model model parameter estimates &name";
									proc print data=%sysfunc(cats(_madareg_cv&c, _ae_cv&l&sub&k)) noobs label;
									run;

									data _nostderr_;
										set %sysfunc(cats(_madareg_cv&c, _ae_cv&l&sub&k));
										where standarderror=.;
									run;

									%let numobs=0;
									%if %sysfunc(exist(_nostderr_)) %then %do;
										data _null_;
											set _nostderr_ nobs = numobs;
											call symput('numobs',numobs);
										run;
									%end;
									%let nostderr=&numobs;

									proc datasets memtype=data nodetails nolist;
										delete 	_nostderr_;
									run;
									quit;

									/* notify user of missing standard errors and that model may be unreliable */
									%if &nostderr > 0 %then %do;
										ods rtf text="^S={outputwidth=100% just=l font_weight=bold font_size=3} WARNING: Standard error missing (.) for &nostderr estimate(s) in the table. Pooled estimates may be unstable.";
										ods rtf text="^S={outputwidth=100%} ";
										ods rtf text="^S={outputwidth=100%} ";
									%end;

									/* don't want to repeat this info for all levels of the
									covariate so just do this once after the last level */
									%if &j = &&nlevels&y and &relax ~= Y %then %do;
										%let corr = 0;
										
										data _null_;
											set %sysfunc(cats(_madareg_cv&c, _pe_cv&l&sub&k));
											if parameter='Corr(logits)' then call symput('corr', put(Estimate, best12.));
										run;
										
										%if %sysfunc(abs(&corr)) = 1 %then %do;
											ods rtf text="^S={outputwidth=100% just=l font_weight=bold font_size=3} NOTE: Between study correlation of +1 or -1.";
											ods rtf text="^S={outputwidth=100% just=l font_weight=bold font_size=3} There may be insufficient information to estimate the correlation and pooled estimates may be unstable.";
											ods rtf text="^S={outputwidth=100%} ";
											ods rtf text="^S={outputwidth=100%} ";
										%end;
									%end;

									proc datasets memtype=data nodetails nolist;
										delete 	%sysfunc(cats(_madareg_cv&c, _pe_cv&l&sub&k));
									run;
									quit;

									proc datasets memtype=data nodetails nolist;
										delete %sysfunc(cats(_madareg_cv&c, _ae_cv&l&sub&k));
									run;
									quit;

								%end;

								%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&c, _cov_cv&l&sub&k)))) %then %do;
									ods rtf text="^S={outputwidth=100%} ";
									ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} Confidence and prediction region parameters &name";
									proc print data=%sysfunc(cats(_madareg_cv&c, _cov_cv&l&sub&k)) noobs label;
									run;
									proc datasets memtype=data nodetails nolist;
										delete %sysfunc(cats(_madareg_cv&c, _cov_cv&l&sub&k));
									run;
									quit;
								%end;
							%end;
							%else %do;
								ods rtf text="^S={outputwidth=100%} ";
								ods rtf text="^S={outputwidth=100% just=l font_weight=bold font_size=4} WARNING: This data set has no data &name";
								ods rtf text="^S={outputwidth=100%} ";
							%end;
						%end;
						%let x = %eval(&x + &&nlevels&y + 1);
						%let y = %eval(&y + 1);
					%end;

					%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&c, _statsummary_&k)))) %then %do;
						ods rtf text="^S={outputwidth=100%} ";
						%if &c <= &ncovariates %then %do;
							ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} Summary estimates of test accuracy measures for &&covariate&c";
						%end;
						%else %do;
							ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} Summary estimates of test accuracy measures for covariates: &covariate";
						%end;
						proc print data=%sysfunc(cats(_madareg_cv&c, _statsummary_&k)) noobs label;
						run;

						data _nostderr_;
							set %sysfunc(cats(_madareg_cv&c, _statsummary_&k));
							where lower=. or upper=.;
						run;

						%let numobs=0;
						%if %sysfunc(exist(_nostderr_)) %then %do;
							data _null_;
								set _nostderr_ nobs = numobs;
								call symput('numobs',numobs);
							run;
						%end;
						%let nostderr=&numobs;

						proc datasets memtype=data nodetails nolist;
							delete 	_nostderr_;
						run;
						quit;

						/* notify user of missing standard errors and that model may be unreliable */
						%if &nostderr > 0 %then %do;
							ods rtf text="^S={outputwidth=100% just=l font_weight=bold font_size=3} WARNING: Standard error missing (.) for &nostderr estimate(s) in the table. Pooled estimates may be unstable.";
							ods rtf text="^S={outputwidth=100%} ";
							ods rtf text="^S={outputwidth=100%} ";
						%end;
					%end;
					
					%if &c <= &ncovariates and %upcase(&graph) ~= N  %then %do;
						%if %sysfunc(exist(_madareg_a_tmp_summst_&k)) and %sysfunc(exist(%sysfunc(cats(_madareg_cv&c, _statsummary_&k)))) %then %do;
							%forest(cltype = &cltype,  
								subject=&subject, 
								cialpha=&cialpha, 
								covariate = %sysfunc(scanq(&covariate, &c)),
								gtitle = &gtitle,
								pmin= &pmin,
								pmax = &pmax,
								gwidth= &gwidth,
								gheight= &gheight,
								dp=&dp);
						%end;
					%end;

					%if %sysfunc(exist(%sysfunc(cats(_madareg_cv&c, _relsummary_&k)))) %then %do;
						ods rtf text="^S={outputwidth=100%} ";
						%if %eval(&c <= &ncovariates) %then %do;
							ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} Estimates of relative measures of test accuracy for &&covariate&c";
						%end;
						%else %do;
							ods rtf text="^S={outputwidth=100% just=c font_weight=bold font_size=4} Estimates of relative measures of test accuracy for covariates: &covariate";
						%end;
						proc print data= %sysfunc(cats(_madareg_cv&c, _relsummary_&k)) noobs label;
						run;

						data _nostderr_;
							set %sysfunc(cats(_madareg_cv&c, _relsummary_&k));
							where lower=. or upper=.;
						run;

						%let numobs=0;
						%if %sysfunc(exist(_nostderr_)) %then %do;
							data _null_;
								set _nostderr_ nobs = numobs;
								call symput('numobs',numobs);
							run;
						%end;
						%let nostderr=&numobs;

						proc datasets memtype=data nodetails nolist;
							delete 	_nostderr_;
						run;
						quit;

						/* notify user of missing standard errors and that model may be unreliable */
						%if &nostderr > 0 %then %do;
							ods rtf text="^S={outputwidth=100% just=l font_weight=bold font_size=3} WARNING: Standard error missing (.) for &nostderr estimate(s) in the table. Pooled estimates may be unstable.";
							ods rtf text="^S={outputwidth=100%} ";
							ods rtf text="^S={outputwidth=100%} ";
						%end;
					%end;

				%end;
			%end;
		%end;

		/* delete unnecessary data sets */
		proc datasets memtype=data nodetails nolist;
			delete 	_tb_s2u_&k:;
		run;
		quit;

		proc datasets memtype=data nodetails nolist;
			delete 	_temp_elogits
					_tmp_;
		run;
		quit;

	%end;

	/* delete temporary data sets no longer reuired for output */
	proc datasets memtype=data nodetails nolist;
		delete _madareg_a_tmp_:;
	run;
	quit;

	proc datasets memtype=data nodetails nolist;
		delete _tb_se;
	run;
	quit;

	quit;

	ods results;
	ods rtf close;
	ods listing;

	%put *****************;
	%put  End of Analysis ;
	%put *****************;

	/* if macro execution is not terminated prematurely due to an error of
	some sort then set global variable madareg to 1, i.e., successful execution so far */
	%let madareg=1;

%mend;
