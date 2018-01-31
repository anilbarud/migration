$PBExportHeader$n_cst_t5annualreport.sru
$PBExportComments$most logic goes in here
forward
global type n_cst_t5annualreport from nonvisualobject
end type
end forward

global type n_cst_t5annualreport from nonvisualobject
end type
global n_cst_t5annualreport n_cst_t5annualreport

type variables
constant integer none = 0
constant integer nonclaimant = 1
constant integer annuitypayout = 2


private:
integer iiworkingreport//1 or 2
datastore idsreport[2]// the data of the two reports

string isfromdate[2]//tentative from date
string istodate[2]// tentative to date

// attributes of displayed report.  What user actually sees
date idfromdate[2]
date idtodate[2]

// from window
datawindow idwreport
window iwparent

end variables

forward prototypes
public subroutine of_handlereportselection (integer aireportno)
public subroutine of_handleretrieveclick ()
public subroutine of_handledateedit (boolean abfrom, string asdata)
public subroutine of_handleopen (ref datawindow adwreport)
public function boolean of_ismodified ()
public function date of_convert (string asdate)
public subroutine of_handleclosing ()
public function date of_getdate (boolean abstartdate)
public function integer of_getselectedreport ()
public function boolean of_needrefresh ()
public function string of_getreportparameterstext ()
end prototypes

public subroutine of_handlereportselection (integer aireportno);/*

Sven Oborn
April 12/2011

handles the selection of report from user

*/

boolean lbisreport 

// manage status of current report
if iiworkingreport = none then
	iiworkingreport = aireportno
	lbisreport = false
elseif iiworkingreport = nonclaimant then
	iiworkingreport = aireportno
	lbisreport = true
elseif iiworkingreport = annuitypayout then
	iiworkingreport = aireportno
	lbisreport = true
else
	// error
	lbisreport = false
end if
if lbisreport then
	// reset the edit ed dates if any
	isfromdate[iiworkingreport] = string(idfromdate[iiworkingreport])
	istodate[iiworkingreport] = string(idtodate[iiworkingreport])
end if


// set new report as current report
if aireportno = none then
	iiworkingreport = aireportno
	lbisreport = false
elseif aireportno = nonclaimant then
	iiworkingreport = aireportno
	lbisreport = true
elseif aireportno = annuitypayout then
	iiworkingreport = aireportno
	lbisreport = true
else
	// error
	lbisreport = false
end if

// get working report to share data with report datawindow
if lbisreport then
	this.idwreport.dataobject = idsreport[iiworkingreport].dataobject
	idsreport[iiworkingreport].sharedata(this.idwreport)
	
	iwparent.dynamic wf_reportselected()
	//this.ibismodified = false
end if



end subroutine

public subroutine of_handleretrieveclick ();/*

Sven Oborn
April 12/2011


Retrieve the data if input is valid

*/

date ldstartdate, ldenddate
boolean lbgoodhealth 
lbgoodhealth = true

//check that a report has been selected
if iiworkingreport <>  annuitypayout and  iiworkingreport <>  nonclaimant then
	// if it is true that report is not annuity payout and also not nonclaimant, then bad health
	lbgoodhealth = false
end if

// check that input start date format is correct
if lbgoodhealth then
	ldstartdate = this.of_convert(isfromdate[iiworkingreport])
	if isnull(ldstartdate) then
		lbgoodhealth = false
	else
		
	end if
end if

// check that input end date format is correct
if lbgoodhealth then
	ldenddate = this.of_convert(istodate[iiworkingreport])
	if isnull(ldenddate) then
		lbgoodhealth = false
	end if
end if

// check that end date comes after start date
if lbgoodhealth then
	if ldenddate <= ldstartdate then
		lbgoodhealth = false
	end if
end if

// retrieve the report data
if lbgoodhealth then
	idfromdate[iiworkingreport] = ldstartdate
	idtodate[iiworkingreport] = ldenddate
	long llrows
	llrows = idsreport[iiworkingreport].retrieve(idfromdate[iiworkingreport], idtodate[iiworkingreport])
	//this.ibismodified =false
end if



end subroutine

public subroutine of_handledateedit (boolean abfrom, string asdata);/*

Sven Oborn
April 13/2011

handles the editing of the start and end dates of the datawindow

*/


if IsDate(asdata) then
	


	boolean lbrealreport
	lbrealreport = false
	
	if iiworkingreport = nonclaimant then
		lbrealreport = true
	elseif iiworkingreport = annuitypayout then
		lbrealreport = true	
	end if
	
	if lbrealreport then
		if abfrom then
			isfromdate[iiworkingreport] = asdata
		else
			istodate[iiworkingreport] = asdata
		end if
		
		date lfromdate, ltodate
		lfromdate = of_convert(isfromdate[iiworkingreport] )
		ltodate = of_convert(istodate[iiworkingreport] )
		if lfromdate < ltodate then
			//ok
			iwparent.dynamic wf_modified()
		else
			// inform user
			messagebox("Invalid Date Range", "The End date of " + istodate[iiworkingreport] + " is earlier than the start date of " + isfromdate[iiworkingreport] )
			iwparent.dynamic wf_datenotvalid()
		end if
		
		
		// check to see that 
		
		//if not this.ibismodified then
			//this.ibismodified = true
		//end if
	end if
else
	iwparent.dynamic wf_datenotvalid()
end if

end subroutine

public subroutine of_handleopen (ref datawindow adwreport);/*
Sven Oborn
April 12/2011

handles open event of the window

*/

// initialize UI 
idwreport = adwreport
iwparent = adwreport.getparent()

// setup nonclaimant report
idsreport[1] = create datastore
idsreport[1] .dataobject = "d_t5reportnonclaimantrepayment"
idsreport[1] .settransobject(sqlca)

// setup annuity payout report
idsreport[2] = create datastore
idsreport[2] .dataobject = "d_t5reportannuitypayout"
idsreport[2] .settransobject(sqlca)

//idwdates = adwdates

// initialize all dates to beginning of year to current date
date ldstartdate
date ldenddate
string lsstartdate
string lstoday 
integer lithisyear = year(today())
ldenddate = today()
lstoday = string(ldenddate)
lsstartdate = string(lithisyear) + "-01-01"
ldstartdate = this.of_convert(lsstartdate)

isfromdate[1] = lsstartdate
isfromdate[2] = lsstartdate
istodate[1]= lstoday
istodate[2]= lstoday
idfromdate[1]=ldstartdate
idfromdate[2]=ldstartdate
idtodate[1]=ldenddate
idtodate[2]=ldenddate
//this.of_handledateedit(true, lsstartdate)// format YYYY-MM-DD [time]
//this.of_handledateedit(false, lstoday)

// set initial report to nonclaimants for this year
this.of_handlereportselection(1)




end subroutine

public function boolean of_ismodified ();/*

Sven Oborn
April 12/2011

desc:  is true if there is a mismatch between control settings and report
*/
boolean lbismodified
lbismodified = false
date lddate
//if ibismodified then
	// check to see that modified dates and final dates do not agree
	//	string isfromdate[2]//tentative from date
	//string istodate[2]// tentative to date
	//boolean ibismodified
	//
	//
	//// attributes of displayed report.  What user actually sees
	//integer iidisplayededreport// 1 or 2
	//date idfromdate[2]
	//date idtodate[2]
	if iiworkingreport > 0 then
		lddate =  this.of_convert(isfromdate[iiworkingreport])
		if lddate <> idfromdate[iiworkingreport] then
			lbismodified = true
		else
			lddate =  this.of_convert(istodate[iiworkingreport])
			if lddate <> idtodate[iiworkingreport] then
				lbismodified = true
			end if
		end if
		
	end if
//end if



return lbismodified
end function

public function date of_convert (string asdate);/*

Sven Oborn
April 13/2011

*/

date lddate
lddate = date(asdate)
return lddate
end function

public subroutine of_handleclosing ();/*

Sven Oborn
April 13/2011

handles the close query event of the window

*/

idsreport[1].reset()
idsreport[2].reset()

destroy idsreport[1]
destroy idsreport[2]

end subroutine

public function date of_getdate (boolean abstartdate);/*

Sven Oborn
April 13/2011

*/

date lddate
if abstartdate then
	lddate = idfromdate[iiworkingreport]
else
	lddate = idtodate[iiworkingreport]
end if

return lddate
end function

public function integer of_getselectedreport ();/*

Sven Oborn
April 13/2011

*/

return iiworkingreport
end function

public function boolean of_needrefresh ();/*

Sven Oborn
April 13/2011

basically see if UI needs to retrieve the report

*/
boolean lbneedrefresh
lbneedrefresh = false

if this.idsreport[iiworkingreport].rowcount() = 0 then
	lbneedrefresh = true
elseif this.of_ismodified() then
	lbneedrefresh = true
end if
return lbneedrefresh
end function

public function string of_getreportparameterstext ();/*

Sven Oborn
April 13/2011

*/

string lsparameters
lsparameters = "From "+ String(this.of_getdate(true) , 'yyyy-mm-dd') + ' to ' + String(RelativeDate(this.of_getdate(false), -1),'yyyy-mm-dd')
//  'From ' + String(startdate , 'yyyy-mm-dd') + ' to ' + String(RelativeDate(enddate, -1),'yyyy-mm-dd')
return lsparameters
end function

on n_cst_t5annualreport.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_cst_t5annualreport.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

event constructor;/*

Sven Oborn
April 12/2011


This object serves w_t5annualreport.  The 'of_handle' methods handles events from the window.  Other methods inform the window of
its current state so that window can update itself

*/



iiworkingreport = none


end event

