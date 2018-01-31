$PBExportHeader$n_treatment.sru
forward
global type n_treatment from n_pdc
end type
end forward

global type n_treatment from n_pdc
end type
global n_treatment n_treatment

forward prototypes
public subroutine nf_init ()
end prototypes

public subroutine nf_init ();U_DWA ldw_dw[]

/*	register the parent and the datawindows */
ldw_dw[1] = idw_dw[1]												
ldw_dw[2] = idw_dw[2]	
end subroutine

on n_treatment.create
call super::create
end on

on n_treatment.destroy
call super::destroy
end on

