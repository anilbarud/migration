-- pr1071 - new correspondence templates (ILLEGIBL french & english)
-- =================================================================


begin transaction

/*********************************/
/* Script used to insert records */
/* by request into the           */
/* TEMPLATE_FIELD_XREF table.    */
/*********************************/

INSERT 
  INTO TEMPLATE_FIELD_XREF
    (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES
    ('ILLEGIBL', 'E1.0', '{date_of_letter}', 1)
go

INSERT 
  INTO TEMPLATE_FIELD_XREF
    (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES
    ('ILLEGIBL', 'F1.0', '{date_of_letter}', 1)
go

INSERT 
  INTO TEMPLATE_FIELD_XREF
    (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES
    ('ILLEGIBL', 'E1.0', '{addressee}', 2)
go

INSERT 
  INTO TEMPLATE_FIELD_XREF
    (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES
    ('ILLEGIBL', 'F1.0', '{addressee}', 2)
go

INSERT 
  INTO TEMPLATE_FIELD_XREF
    (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES
    ('ILLEGIBL', 'E1.0', '{salutation}', 3)
go

INSERT 
  INTO TEMPLATE_FIELD_XREF
    (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES
    ('ILLEGIBL', 'F1.0', '{salutation}', 3)
go

INSERT 
  INTO TEMPLATE_FIELD_XREF
    (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES
    ('ILLEGIBL', 'E1.0', '{claim_number}', 4)
go

INSERT 
  INTO TEMPLATE_FIELD_XREF
    (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES
    ('ILLEGIBL', 'F1.0', '{claimant_name}', 4)
go

INSERT 
  INTO TEMPLATE_FIELD_XREF
    (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES
    ('ILLEGIBL', 'F1.0', '{claim_number}', 5)
go

INSERT 
  INTO TEMPLATE_FIELD_XREF
    (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES
    ('ILLEGIBL', 'E1.0', '{whscc_local_phone_no}', 960)
go

INSERT 
  INTO TEMPLATE_FIELD_XREF
    (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES
    ('ILLEGIBL', 'F1.0', '{whscc_local_phone_no}', 960)
go

INSERT 
  INTO TEMPLATE_FIELD_XREF
    (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES
    ('ILLEGIBL', 'E1.0', '{toll_free_phone_no}', 970)
go

INSERT 
  INTO TEMPLATE_FIELD_XREF
    (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES
    ('ILLEGIBL', 'F1.0', '{toll_free_phone_no}', 970)
go

INSERT 
  INTO TEMPLATE_FIELD_XREF
    (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES
    ('ILLEGIBL', 'E1.0', '{sender_name}', 980)
go

INSERT 
  INTO TEMPLATE_FIELD_XREF
    (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES
    ('ILLEGIBL', 'F1.0', '{sender_name}', 980)
go

INSERT 
  INTO TEMPLATE_FIELD_XREF
    (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES
    ('ILLEGIBL', 'E1.0', '{sender_position}', 990)
go

INSERT 
  INTO TEMPLATE_FIELD_XREF
    (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES
    ('ILLEGIBL', 'F1.0', '{sender_position}', 990)
go


/*****************/
/* End of INSERT */
/*****************/


/*********************************/
/* Script used to insert records */
/* by request into the           */
/* CORRESPONDENCE_TEMPLATE table.*/
/*********************************/

INSERT 
  INTO CORRESPONDENCE_TEMPLATE
    (template_code, template_version_no, template_desc, signature_required_yn, cc_allowed_yn, remote_mail_allowed_yn,
	active_flag)
  VALUES
    ('ILLEGIBL', 'E1.0', 'Illegible/Incomplete medical document', 'Y', 'N', 'N', 'Y')
go


INSERT 
  INTO CORRESPONDENCE_TEMPLATE
    (template_code, template_version_no, template_desc, signature_required_yn, cc_allowed_yn, remote_mail_allowed_yn,
	active_flag)
  VALUES
    ('ILLEGIBL', 'F1.0', 'Illegible/Incomplete medical document', 'Y', 'N', 'N', 'Y')
go

/*****************/
/* End of INSERT */
/*****************/


-- rollback
-- commit

-- **************** RESULTS ********************************************************************************************

select * from CORRESPONDENCE_TEMPLATE where template_code = 'ILLEGIBL'
select * from TEMPLATE_FIELD_XREF where template_code = 'ILLEGIBL'

template_code template_version_no template_desc                            signature_required_yn cc_allowed_yn remote_mail_allowed_yn active_flag create_date                 modify_date                 create_user_id modify_user_id timestamp          
------------- ------------------- ---------------------------------------- --------------------- ------------- ---------------------- ----------- --------------------------- --------------------------- -------------- -------------- ------------------ 
ILLEGIBL      E1.0                Illegible/Incomplete medical document    Y                     N             N                      Y           Feb 14 2000 11:22AM         (null)                      sa                            0x000000010c7f1c09 
ILLEGIBL      F1.0                Illegible/Incomplete medical document    Y                     N             N                      Y           Feb 14 2000 11:23AM         (null)                      sa                            0x000000010c7f1c37 

(2 row(s) affected)

template_code template_version_no template_field_id                        template_field_seq_no create_date                 modify_date                 create_user_id modify_user_id timestamp          
------------- ------------------- ---------------------------------------- --------------------- --------------------------- --------------------------- -------------- -------------- ------------------ 
ILLEGIBL      E1.0                {addressee}                              2                     Feb 14 2000 11:22AM         (null)                      sa                            0x000000010c7f1bba 
ILLEGIBL      E1.0                {claim_number}                           4                     Feb 14 2000 11:22AM         (null)                      sa                            0x000000010c7f1bcf 
ILLEGIBL      E1.0                {date_of_letter}                         1                     Feb 14 2000 11:22AM         (null)                      sa                            0x000000010c7f1bae 
ILLEGIBL      E1.0                {salutation}                             3                     Feb 14 2000 11:22AM         (null)                      sa                            0x000000010c7f1bc5 
ILLEGIBL      E1.0                {sender_name}                            980                   Feb 14 2000 11:22AM         (null)                      sa                            0x000000010c7f1bf4 
ILLEGIBL      E1.0                {sender_position}                        990                   Feb 14 2000 11:22AM         (null)                      sa                            0x000000010c7f1bfe 
ILLEGIBL      E1.0                {toll_free_phone_no}                     970                   Feb 14 2000 11:22AM         (null)                      sa                            0x000000010c7f1bea 
ILLEGIBL      E1.0                {whscc_local_phone_no}                   960                   Feb 14 2000 11:22AM         (null)                      sa                            0x000000010c7f1bda 
ILLEGIBL      F1.0                {addressee}                              2                     Feb 14 2000 11:22AM         (null)                      sa                            0x000000010c7f1bc0 
ILLEGIBL      F1.0                {claim_number}                           5                     Feb 14 2000 11:22AM         Feb 14 2000  2:44PM         sa             sa             0x000000010c7f5bd3 
ILLEGIBL      F1.0                {claimant_name}                          4                     Feb 14 2000  2:44PM         (null)                      sa                            0x000000010c7f5bdb 
ILLEGIBL      F1.0                {date_of_letter}                         1                     Feb 14 2000 11:22AM         (null)                      sa                            0x000000010c7f1bb3 
ILLEGIBL      F1.0                {salutation}                             3                     Feb 14 2000 11:22AM         (null)                      sa                            0x000000010c7f1bca 
ILLEGIBL      F1.0                {sender_name}                            980                   Feb 14 2000 11:22AM         (null)                      sa                            0x000000010c7f1bf9 
ILLEGIBL      F1.0                {sender_position}                        990                   Feb 14 2000 11:22AM         (null)                      sa                            0x000000010c7f1c04 
ILLEGIBL      F1.0                {toll_free_phone_no}                     970                   Feb 14 2000 11:22AM         (null)                      sa                            0x000000010c7f1bef 
ILLEGIBL      F1.0                {whscc_local_phone_no}                   960                   Feb 14 2000 11:22AM         (null)                      sa                            0x000000010c7f1be4 

(17 row(s) affected)

