/*********************************/
/* Script used to insert records */
/* by request into the           */
/* TEMPLATE_FIELD table.         */
/*********************************/

/* sp_help TEMPLATE_FIELD */

/****************************************/
/* template_field_id     = CHAR     40  */
/* template_field_desc   = VARCHAR  40  */
/* access_code           = CHAR     1   */
/* access_field_name     = VARCHAR  40  */
/* active_flag           = CHAR     1   */
/****************************************/

INSERT 
  INTO TEMPLATE_FIELD
    (template_field_id,
     template_field_desc,
     access_code,
     access_field_name,
     active_flag)
  VALUES
    (template_field_id,
     template_field_desc,
     access_code,
     access_field_name,
     active_flag)
go

/*****************/
/* End of INSERT */
/*****************/
