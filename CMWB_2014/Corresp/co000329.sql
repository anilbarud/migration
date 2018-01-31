
-- PR 1165/log 0 - New MEDINFO Correspondence Templates
-- *************   ************************************
-- ( revised 2000/03/29 - full_address field in subject section replaced with medicare_no)

-- ====================================================
-- insert new template fields into TEMPLATE_FIELD table
-- ====================================================


begin transaction
go


select * from TEMPLATE_FIELD where template_field_id like('%condition/inj%')

INSERT INTO TEMPLATE_FIELD (template_field_id, template_field_desc, access_code, access_field_name, active_flag)
  VALUES ('{name_specific_condition/injury}', 'Naming the specific injury/condition', 'm', '','Y')

select * from TEMPLATE_FIELD where template_field_id like('%condition/inj%')


-- rollback transaction
-- commit transaction





-- ============================================================
-- insert new template names into CORRESPONDENCE_TEMPLATE table
-- ============================================================


begin transaction
go


select * from CORRESPONDENCE_TEMPLATE where template_code = 'MEDINFO'


INSERT
  INTO CORRESPONDENCE_TEMPLATE (template_code, template_version_no, template_desc, signature_required_yn, cc_allowed_yn,
				remote_mail_allowed_yn, active_flag)
  VALUES ('MEDINFO', 'E1.0', 'Medical Info on specific injury/condition', 'Y', 'N', 'N', 'Y')


INSERT
  INTO CORRESPONDENCE_TEMPLATE (template_code, template_version_no, template_desc, signature_required_yn, cc_allowed_yn,
				remote_mail_allowed_yn, active_flag)
  VALUES ('MEDINFO', 'F1.0', 'Medical Info on specific injury/condition', 'Y', 'N', 'N', 'Y')


select * from CORRESPONDENCE_TEMPLATE where template_code = 'MEDINFO'

-- rollback transaction
-- commit transaction



-- =============================================================
-- insert new template references into TEMPLATE_FIELD_XREF table
-- =============================================================


begin transaction
go

select * from TEMPLATE_FIELD_XREF where template_code = 'MEDINFO' order by template_version_no


-- English template field xrefs

INSERT 
  INTO TEMPLATE_FIELD_XREF (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES ('MEDINFO', 'E1.0', '{date_of_letter}', 1)


INSERT 
  INTO TEMPLATE_FIELD_XREF (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES ('MEDINFO', 'E1.0', '{addressee}', 2)


INSERT 
  INTO TEMPLATE_FIELD_XREF (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES ('MEDINFO', 'E1.0', '{salutation}', 3)


INSERT 
  INTO TEMPLATE_FIELD_XREF (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES ('MEDINFO', 'E1.0', '{claim_number}', 4)


INSERT 
  INTO TEMPLATE_FIELD_XREF (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES ('MEDINFO', 'E1.0', '{claimant_name}', 5)


INSERT 
  INTO TEMPLATE_FIELD_XREF (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES ('MEDINFO', 'E1.0', '{medicare_number}', 6)


INSERT 
  INTO TEMPLATE_FIELD_XREF (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES ('MEDINFO', 'E1.0', '{date_of_birth}', 7)


INSERT 
  INTO TEMPLATE_FIELD_XREF (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES ('MEDINFO', 'E1.0', '{name_specific_condition/injury}', 8)


INSERT 
  INTO TEMPLATE_FIELD_XREF (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES ('MEDINFO', 'E1.0', '{sender_name}', 980)


INSERT 
  INTO TEMPLATE_FIELD_XREF (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES ('MEDINFO', 'E1.0', '{sender_position}', 990)


-- French template field xrefs

INSERT 
  INTO TEMPLATE_FIELD_XREF (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES ('MEDINFO', 'F1.0', '{date_of_letter}', 1)


INSERT 
  INTO TEMPLATE_FIELD_XREF (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES ('MEDINFO', 'F1.0', '{addressee}', 2)


INSERT 
  INTO TEMPLATE_FIELD_XREF (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES ('MEDINFO', 'F1.0', '{salutation}', 3)


INSERT 
  INTO TEMPLATE_FIELD_XREF (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES ('MEDINFO', 'F1.0', '{claim_number}', 4)


INSERT 
  INTO TEMPLATE_FIELD_XREF (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES ('MEDINFO', 'F1.0', '{claimant_name}', 5)


INSERT 
  INTO TEMPLATE_FIELD_XREF (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES ('MEDINFO', 'F1.0', '{medicare_number}', 6)


INSERT 
  INTO TEMPLATE_FIELD_XREF (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES ('MEDINFO', 'F1.0', '{date_of_birth}', 7)


INSERT 
  INTO TEMPLATE_FIELD_XREF (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES ('MEDINFO', 'F1.0', '{name_specific_condition/injury}', 8)


INSERT 
  INTO TEMPLATE_FIELD_XREF (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES ('MEDINFO', 'F1.0', '{sender_name}', 980)


INSERT 
  INTO TEMPLATE_FIELD_XREF (template_code, template_version_no, template_field_id, template_field_seq_no)
  VALUES ('MEDINFO', 'F1.0', '{sender_position}', 990)


select * from TEMPLATE_FIELD_XREF where template_code = 'MEDINFO' order by template_version_no, template_field_seq_no


-- rollback transaction
-- commit transaction
