/*********************************/
/* Script used to insert records */
/* by request into the           */
/* TEMPLATE_FIELD_XREF table.    */
/*********************************/

/* sp_help TEMPLATE_FIELD_XREF */

/****************************************/
/* template_code         = CHAR     8   */
/* template_version_no   = CHAR     5   */
/* template_field_id     = CHAR     40  */
/* template_field_seq_no = SMALLINT     */
/****************************************/

INSERT 
  INTO TEMPLATE_FIELD_XREF
    (template_code,
     template_version_no,
     template_field_id,
     template_field_seq_no)
  VALUES
    (template_code,
     template_version_no,
     template_field_id,
     template_field_seq_no)
go

/*****************/
/* End of INSERT */
/*****************/


/*********************************/
/* Script used to insert records */
/* by request into the           */
/* CORRESPONDENCE_TEMPLATE table.*/
/*********************************/

/* sp_help CORRESPONDENCE_TEMPLATE */

/*****************************************/
/* template_code          = CHAR     8   */
/* template_version_no    = CHAR     5   */
/* template_desc          = VARCHAR  40  */
/* signature_required_yn  = CHAR     1   */
/* cc_allowed_yn          = CHAR     1   */
/* remote_mail_allowed_yn = CHAR     1   */
/* active_flag            = CHAR     1   */
/*****************************************/

INSERT 
  INTO CORRESPONDENCE_TEMPLATE
    (template_code,
     template_version_no,
     template_desc,
     signature_required_yn,
     cc_allowed_yn,
     remote_mail_allowed_yn,
     active_flag)
  VALUES
    (template_code,
     template_version_no,
     template_desc,
     signature_required_yn,
     cc_allowed_yn,
     remote_mail_allowed_yn,
     active_flag)
go

/*****************/
/* End of INSERT */
/*****************/
