$PBExportHeader$n_payment_matching.sru
forward
global type n_payment_matching from nonvisualobject
end type
end forward

global type n_payment_matching from nonvisualobject
end type
global n_payment_matching n_payment_matching

type variables
DECIMAL{2}			idec_target_amount
DECIMAL{2}			idec_target_variance

DECIMAL{2}			idec_source_payment_amount[]
DECIMAL{2}			il_match_payment_list[]

LONG					il_source_payment_count
LONG					il_counter

CONSTANT				LONG		LOOP_INTERUPT = 1000000
end variables

forward prototypes
public function integer of_find_match (long al_working_payment_list[], decimal adec_working_total_amount)
public function integer of_start ()
end prototypes

public function integer of_find_match (long al_working_payment_list[], decimal adec_working_total_amount);LONG				ll_K
DECIMAL{2}		ldec_working_total_amount
INTEGER			li_x
INTEGER			li_rtn

//al_working_payment_list contains the index of the payments


//Get the number of elements in the array
ll_K = UpperBound(al_working_payment_list)

//Add one element to the array
ll_K++

//Return -1 if we have run out of payments.
IF al_working_payment_list[ll_K - 1] + 1 > il_source_payment_count THEN
	RETURN 0
END IF

//Set the value to the preceding element plus one
al_working_payment_list[ll_K] = al_working_payment_list[ll_K - 1] + 1


//Search through the payments that follow
For li_x = al_working_payment_list[ll_K] to il_source_payment_count

	ldec_working_total_amount = 0
	//Add all the payment amounts for this combination of payments

	ldec_working_total_amount = adec_working_total_amount + idec_source_payment_amount[al_working_payment_list[ll_K]]
	
	IF ldec_working_total_amount >=  idec_target_amount - idec_target_variance AND &
		ldec_working_total_amount <=  idec_target_amount + idec_target_variance THEN
		
		il_match_payment_list = al_working_payment_list
		//Match found, return 1
		RETURN 1
	ELSEIF ldec_working_total_amount >  idec_target_amount + idec_target_variance THEN
		
		IF al_working_payment_list[ll_K] + 1 > il_source_payment_count THEN
			RETURN 0
		end if
		
		//Set the last element in the working array to the next payment index in the list
		al_working_payment_list[ll_K] = al_working_payment_list[ll_K] + 1	

	ELSE
		il_counter ++	
		
		IF mod(il_counter,LOOP_INTERUPT) = 0 THEN
			li_rtn = MessageBox('Continue','A match has not been found yet. Do you want to continue searching?',Question!,YesNo!)
			IF li_rtn = 2 THEN
				RETURN -1
			END IF
		END IF
		
		IF il_counter >= 2147483648 Then
			MessageBox('Counter','An internal limit has been reached before a match could be found.')
			RETURN -1
		End if
		
		li_rtn = of_find_match(al_working_payment_list,ldec_working_total_amount)
		IF li_rtn = 1 Then
			//This is only temporary. Eventually it will be coded to find all possible matches
			//right now it stops when it finds one possible solution.
			RETURN 1
		ElseIF li_rtn = 0 Then
			IF al_working_payment_list[ll_K] + 1 > il_source_payment_count THEN
				RETURN 0
			end if
			
			//Set the last element in the working array to the next payment index in the list
			al_working_payment_list[ll_K] = al_working_payment_list[ll_K] + 1	
		Else
			RETURN -1
		END IF
	END IF	
	
	
	
Next
	
	
RETURN -1
	
end function

public function integer of_start ();LONG				ll_x
INTEGER			li_rtn
DECIMAL{2}    	ldec_working_list[]



il_source_payment_count = UpperBound(idec_source_payment_amount)

FOR ll_x = 1 to il_source_payment_count
	ldec_working_list[1] = ll_x	
	li_rtn = of_find_match(ldec_working_list,idec_source_payment_amount[ll_x])
	if li_rtn = 1 THen 
		RETURN 1
	ELSEIF li_rtn = -1 THEN
		RETURN -1
	END IF
NEXT


RETURN 0
end function

on n_payment_matching.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_payment_matching.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

