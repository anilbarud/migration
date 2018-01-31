$PBExportHeader$n_working_folders.sru
forward
global type n_working_folders from n_pdc
end type
end forward

global type n_working_folders from n_pdc
end type
global n_working_folders n_working_folders

type variables
u_ds			ids_claim_working
u_ds			ids_ref
u_ds			ids_doc_exists
n_imaging	inv_imaging
LONG			il_claim_no

BOOLEAN		ib_update_required

STRING		is_folder_name
end variables

forward prototypes
public function integer nf_attach_documents (long al_docid[])
public function integer nf_set_claim_no (long al_claim_no)
public function integer nf_update ()
public function integer nf_add (string as_action_code, long al_catid, datetime adt_action_date, string as_action_note, string as_folder_name)
public function long nf_set_identifiers ()
end prototypes

public function integer nf_attach_documents (long al_docid[]);//return 0 - nothing done
//return 1 - Sucess
//return -1 - Failure

LONG			ll_doc_count
LONG			ll_catid
LONG			ll_setid
LONG			li_x
LONG			ll_row

If UpperBound(al_docid) = 0 Then RETURN 0

If ids_claim_working.RowCount() = 0 Then RETURN -1

ids_doc_exists.Retrieve(al_docid)
ll_doc_count = ids_doc_exists.GetItemNumber(1,'doc_count')

If ll_doc_count <> UpperBound(al_docid) Then RETURN -1

ll_catid = ids_claim_working.GetItemNumber(1,'catid')
ll_setid = inv_imaging.nf_get_setid(ll_catid)
If ll_setid = -1 Then SignalError(-666,'Error getting setid for catid = ' + String(ll_catid) )


For li_x = 1 To UpperBound(al_docid)
	ll_row = ids_ref.InsertRow(0)
	
	ids_ref.SetItem(ll_row,'docid',al_docid[li_x])
	ids_ref.SetItem(ll_row,'doccatid',ll_catid)
	ids_ref.SetItem(ll_row,'docsetid',ll_setid)
Next


RETURN 1

end function

public function integer nf_set_claim_no (long al_claim_no);IF al_claim_no = 0 Then SignalError(-666,'Error setting claim no')

il_claim_no = al_claim_no

return 1
end function

public function integer nf_update ();If ids_claim_working.RowCount() > 0 Then
	ids_claim_working.UPdate()
	ImageTrans.nf_handle_error('n_inbasket_messaging','nf_update','Update claim working')
	ids_claim_working.Reset()
ENd if


If ids_ref.RowCount() > 0 Then
	ids_ref.UPdate()
	ImageTrans.nf_handle_error('n_inbasket_messaging','nf_update','Update REF')
	ids_ref.Reset()
End if

ib_update_required = False

RETURN 1
end function

public function integer nf_add (string as_action_code, long al_catid, datetime adt_action_date, string as_action_note, string as_folder_name);//RETURN	1 SUCESS
//RETURN -1 FAILURE

LONG			ll_row

If IsNull(as_action_code) Then SignalError(-666,'Action code cannot be blank')

If IsNull(al_catid) Then SignalError(-666,'Catid cannot be zero')

If IsNull(adt_action_date) or Date(adt_action_date) < Date("1900,01,01") Then
	SignalError(-666,"THe action date must be greater than '1900/01/01'.")
End if

If IsNull(as_action_note) THen SignalError(-666,'Action note cannot be null')

ll_row = ids_claim_working.InsertRow(0)

ids_claim_working.SetItem(ll_row,'action_code',as_action_code)
ids_claim_working.SetITem(ll_row,'catid',al_catid)
ids_claim_working.SetITem(ll_row,'action_date',adt_action_date)
ids_claim_working.SetITem(ll_row,'action_note',as_action_note)
ids_claim_working.SetItem(ll_row,'claim_no',il_claim_no)

ib_update_required = True
is_folder_name = as_folder_name

return 1
end function

public function long nf_set_identifiers ();LONG			ll_folder_id
LONG			ll_catid
INTEGER		li_x
STRING		ls_action_code
STRING		ls_claimant_name

ll_catid = ids_claim_working.GetItemNumber(1,'catid')

ll_folder_id = inv_imaging.nf_create_workfolder('',ll_catid)

Update FLD
set fldname = :is_folder_name
WHERE fldid = :ll_folder_id
USING inv_transobj;

inv_transobj.nf_handle_error('n_working_folders','nf_set_identifiers','')

ids_claim_working.SetITem(1,'folderid',ll_folder_id)

If ids_ref.RowCount() > 0 Then
	For li_x = 1 TO ids_ref.RowCount()
		ids_ref.SetITem(li_x,'folderid',ll_folder_id)
	Next
End if

return 1
end function

event constructor;call super::constructor;ids_claim_working  	= CREATE u_ds
ids_doc_exists			= CREATE u_ds
ids_ref					= CREATE u_ds
inv_imaging				= CREATE n_imaging

ids_claim_working.dataobject = 'd_claim_working_insert'
ids_claim_working.SetTransObject(ImageTrans)


ids_doc_exists.DataObject = 'd_doc_exists'
ids_doc_exists.SetTransObject(ImageTrans)

ids_ref.DataObject = 'd_ref_insert'
ids_ref.SetTransObject(ImageTrans)

inv_transobj = ImageTrans





end event

on n_working_folders.create
call super::create
end on

on n_working_folders.destroy
call super::destroy
end on

