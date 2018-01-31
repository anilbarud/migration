$PBExportHeader$uo_br_exception.sru
forward
global type uo_br_exception from exception
end type
end forward

global type uo_br_exception from exception
end type
global uo_br_exception uo_br_exception

type variables
long  il_error_payment_no
long	il_error_txn_no
long	il_error_claim_no
end variables

forward prototypes
public subroutine getindicators (ref long al_error_payment_no, ref long al_error_txn_no, ref long al_error_claim_no)
public subroutine setindicators (long al_error_payment_no, long al_error_txn_no, long al_error_claim_no)
public subroutine reset_indicator ()
end prototypes

public subroutine getindicators (ref long al_error_payment_no, ref long al_error_txn_no, ref long al_error_claim_no);
al_error_payment_no 	= il_error_payment_no
al_error_txn_no 		= il_error_txn_no
al_error_claim_no		= il_error_claim_no
end subroutine

public subroutine setindicators (long al_error_payment_no, long al_error_txn_no, long al_error_claim_no);
il_error_payment_no 	= 	al_error_payment_no 	 
il_error_txn_no 		= 	al_error_txn_no 		 
il_error_claim_no		=	al_error_claim_no		
end subroutine

public subroutine reset_indicator ();il_error_payment_no  = 0
il_error_txn_no		= 0
il_error_claim_no		= 0
end subroutine

on uo_br_exception.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_br_exception.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

