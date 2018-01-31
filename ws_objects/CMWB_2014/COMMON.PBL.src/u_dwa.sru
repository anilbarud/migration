$PBExportHeader$u_dwa.sru
$PBExportComments$Ancestor datawindow user object with dberror and several functions coded
forward
global type u_dwa from datawindow
end type
end forward

global type u_dwa from datawindow
int Width=494
int Height=361
int TabOrder=1
boolean LiveScroll=true
end type
global u_dwa u_dwa

type variables
Protected:
transaction	vit_trans_object

end variables

forward prototypes
public function integer settransobject (transaction vat_trans_object)
public function transaction gettransobject ()
end prototypes

public function integer settransobject (transaction vat_trans_object);/* ----------------------------------------------------------------------------------------------------	*/
/*	Function Name: 	settransobject																							   */
/*																																			*/
/*	Purpose:				This function sets the transaction object for the datawindow.  It also stores			*/
/*							the transaction object in an instance variable so that it can be used by the			*/
/*							db_error event.																							*/
/*																																			*/
/*	Arguments:			Transaction	-	vat_trans_object	-	The transaction object for which this data		*/
/*																			window is to be set.										*/
/*																																			*/
/* ----------------------------------------------------------------------------------------------------	*/


Integer	vli_return_code


vli_return_code = Super::SetTransObject(vat_trans_object)

If vli_return_code > 0 Then
	vit_trans_object = vat_trans_object
End If

Return vli_return_code
end function

public function transaction gettransobject ();/* ----------------------------------------------------------------------------------------------------	*/
/*	Function Name: 	gettransobject																							   */
/*																																			*/
/*	Purpose:				This function returns the transaction object for the datawindow.  						*/
/*																																			*/
/*																																			*/
/* ----------------------------------------------------------------------------------------------------	*/



return vit_trans_object
end function

event dberror;If IsValid(vit_trans_object) Then
	vit_trans_object.SQLDBCode = sqldbcode
	vit_trans_object.SQLErrText = sqlerrtext
	RETURN 1
End If

end event

