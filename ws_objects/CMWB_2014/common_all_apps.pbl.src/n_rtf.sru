$PBExportHeader$n_rtf.sru
forward
global type n_rtf from nonvisualobject
end type
end forward

global type n_rtf from nonvisualobject autoinstantiate
end type

type prototypes
//FUNCTION boolean CryptBinaryToString ( Blob pbBinary, ulong cbBinary, ulong dwFlags, Ref string pszString, Ref ulong pcchString ) LIBRARY "crypt32.dll" ALIAS FOR "CryptBinaryToStringA"
//FUNCTION Boolean CryptBinaryToString (Blob pbBinary, ulong cbBinary, ulong dwFlags, Ref string pszString, Ref ulong pcchString ) &
//	LIBRARY "crypt32.dll" ALIAS FOR "CryptBinaryToStringA;Ansi"


end prototypes
type variables
//Define acceptable image types
PUBLIC CONSTANT STRING cs_EMF 	= "emfblip"
PUBLIC CONSTANT STRING cs_PNG 	= "pngblip"
PUBLIC CONSTANT STRING cs_JPEG 	= "jpegblip"
PUBLIC CONSTANT STRING cs_BMP 	= "dibitmap0"

/*
// Base64, with certificate beginning and ending headers
CONSTANT ULONG CRYPT_STRING_BASE64HEADER = 0

// Base64, without headers
CONSTANT ULONG CRYPT_STRING_BASE64 = 1

// Pure binary copy
CONSTANT ULONG CRYPT_STRING_BINARY = 2

// Base64, with request beginning and ending headers
CONSTANT ULONG CRYPT_STRING_BASE64REQUESTHEADER = 3

// Hexadecimal only
CONSTANT ULONG CRYPT_STRING_HEX = 4

//  Hexadecimal, with ASCII character display
CONSTANT ULONG CRYPT_STRING_HEXASCII = 5

// Base64, with X.509 CRL beginning and ending headers
CONSTANT ULONG CRYPT_STRING_BASE64X509CRLHEADER = 9

// Hexadecimal, with address display
CONSTANT ULONG CRYPT_STRING_HEXADDR = 10

// Hexadecimal, with ASCII character and address display
CONSTANT ULONG CRYPT_STRING_HEXASCIIADDR = 11

CONSTANT ULONG CRYPT_STRING_NOCRLF = 4
*/

end variables
forward prototypes
public function string uf_convert_blob_tohex (blob abl_input_blob)
public function string uf_build_rtf_image_tag (blob abl_image, string as_image_type, integer ai_width, integer ai_height)
public function string uf_insert_rtf_tag (string as_rtf, string as_replace_marker, string as_insert_tag)
public function string uf_get_file_type (string as_hex)
public function string uf_build_rtf_image_tag (blob abl_image, integer ai_width, integer ai_height)
public function integer uf_get_width_height (string as_hex, ref integer ai_width, ref integer ai_height)
public function string uf_get_user_signature_rtf_tag (string as_user_id)
public function string uf_get_user_signature_rtf_tag (string as_user_id, string as_sql, long al_image_width, integer al_image_height)
public function string uf_get_user_signature_rtf_tag (string as_user_id, string as_sql)
public function blob uf_get_blob (string as_sql)
public function long uf_or (long al_value1, long al_value2)
end prototypes

public function string uf_convert_blob_tohex (blob abl_input_blob);/*
**		Type  			: Function
**		Name 			: uf_convert_blob_tohex
**		Arguments 	: blob - abl_input_blob
**		Returns 		: string
**		Purpose		: Converts a blob to a hexadecimal string.
**		Date			: 2014/08/07
**		Author			: David Worboys
**	
**		Modifications
**		2015/03/13 Rewrtote for speed
*/

//Ingnore this trash, may need to get API call method working  if not fast enough for Ron
/*
BLOB 		lbl_byte
ULONG	lul_Blob_Length
ULONG	lul_Hex_Length
STRING	ls_hex = ""

lul_Blob_Length = LEN(abl_input_blob) 
lul_Hex_Length = lul_Blob_Length * 2

ls_Hex = SPACE(lul_Hex_Length)

THIS.CryptBinaryToString(abl_input_blob,lul_Blob_Length, CRYPT_STRING_HEX, ls_Hex,  lul_Hex_Length)

ls_Hex = REPLACE(ls_Hex,1,LEN(ls_Hex)," ")

//ls_Hex = LEFT(ls_Hex, LEN(ls_Hex) - 2 )

ls_Hex = TRIM(ls_Hex)


//ls_Hex = TRIM(ls_Hex)
// CryptBinaryToString ( Blob pbBinary, ulong cbBinary, ulong dwFlags, Ref string pszString, Ref ulong pcchString ) 

*/
CHAR 		lca_hex_digits[0 TO 15] = {'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'}
CHAR 		lca_Hex[]
BYTE 		ly_Byte[]
INTEGER 	li_Right		= 0
INTEGER 	li_Left		= 0
LONG		ll_Index	= 0
LONG 		ll_Loop 	= 0
LONG 		ll_Max 		= 0

STRING 	ls_Hex

ly_Byte[] = GetByteArray(abl_input_blob)

ll_Max = UPPERBOUND(ly_Byte)

lca_Hex[ll_Max * 2] = " "

FOR ll_Loop = 1 TO ll_Max
	li_Left  		= ly_Byte[ll_Loop] / 16
	li_Right 	= Mod(ly_Byte[ll_Loop], 16)
	ll_Index++
	lca_Hex[ll_Index] = lca_hex_digits[li_Left]
	ll_Index++
	lca_Hex[ll_Index] = lca_hex_digits[li_Right]	
//	ls_Hex += lca_hex_digits[li_left] + lca_hex_digits[li_right]
NEXT

ls_Hex = lca_Hex

RETURN ls_Hex
end function

public function string uf_build_rtf_image_tag (blob abl_image, string as_image_type, integer ai_width, integer ai_height);/*
**		Type  		: Function
**		Name 		: uf_build_rtf_image_tag
**		Arguments 	: blob - abl_image,
**		Returns 		: string - ls_image_tag, string - as_image_type (defined in instance variables), integer - ai_width (pixels), integer ai_height (pixels)
**		Purpose		: Builds an RTF image tag that can be inserted into an RTF string.
**		Date			: 2014/08/07
**		Author		: David Worboys
**	
**		Modifications
*/
STRING ls_hex 			= ""
STRING ls_image_tag = ""

ls_hex = THIS.uf_convert_blob_tohex( abl_image)

//Build RTF Image Tag
ls_image_tag = "{\pict\"+as_image_type+"\picw"+STRING(ai_width)+"\pich"+STRING(ai_height)+"\picwgoal"+ STRING(ai_width)+"\pichgoal"+STRING(ai_height)+" "+ ls_hex +"}"

RETURN ls_image_tag
end function

public function string uf_insert_rtf_tag (string as_rtf, string as_replace_marker, string as_insert_tag);/*
**		Type  		: uf_insert_rtf_tag
**		Name 		: Function
**		Arguments 	: string - as_rtf, string as_replace_marker, string as_insert_tag
**		Returns 		: string - as_rtf with replace_marker replaced with the insert tag
**		Purpose		: Basically inserts a tag into the RTF string where a marker is placed.  Dumb does no checks
**		Date			: 2014/08/07
**		Author		: David Worboys
**	
**		Modifications
*/
LONG ll_char = 0

ll_char = Pos(as_RTF,as_replace_marker)

if (ll_char > 0) then //Found as_replace_marker			
	if (Trim(as_insert_tag) = "" or IsNull(as_insert_tag)) then //delete replace marker
		as_RTF = Replace(as_RTF,ll_char,Len(as_replace_marker),"")	
	else //Place insert Tag where replace_marker is
		as_RTF = Replace(as_RTF,ll_char,Len(as_replace_marker),as_insert_tag) 
	end if
end if

RETURN as_RTF
end function

public function string uf_get_file_type (string as_hex);/*
**		Type  			: Function
**		Name 			: uf_get_file_type
**		Arguments 	:
**		Returns 		:
**		Purpose		: Returns the type of file
**		Date			: 2014/07/??
**		Author			: David Worboys
**	
**		Modifications
*/
STRING ls_file_type = ""

//JPEG marked as : ffD8ffE0
IF (Upper(Mid (as_hex, 1 , 6 )) = "FFD8FF") THEN //JPEG
	ls_file_type = cs_JPEG
ELSEIF(Upper(Mid (as_hex, 1 , 4 )) = "424D") THEN //Windows Bitmap
	ls_file_type = cs_BMP
ELSEIF(Upper(Mid (as_hex, 1 ,8 )) = "89504E47") THEN //PNG file
	ls_file_type = cs_PNG
ELSEIF(Upper(Mid (as_hex, 1 ,16 )) = "0100000058000000") THEN //EMF file
	ls_file_type = cs_EMF
END IF

RETURN ls_file_type
end function

public function string uf_build_rtf_image_tag (blob abl_image, integer ai_width, integer ai_height);/*
**		Type  			: Function
**		Name 			: uf_build_rtf_image_tag
**		Arguments 	: blob - abl_image,
**		Returns 		: string - ls_image_tag,  integer - ai_width (pixels), integer ai_height (pixels)
**		Purpose		: Builds an RTF image tag that can be inserted into an RTF string.
**		Date			: 2014/08/07
**		Author		: David Worboys
**	
**		Modifications
*/
STRING ls_file_type = ""
STRING ls_hex 			= ""
STRING ls_image_tag = ""

ls_hex = THIS.uf_convert_blob_tohex( abl_image)

THIS.uf_Get_Width_Height(ls_hex, ai_width, ai_height)

ls_file_type = THIS.uf_get_file_type(ls_hex)

ls_image_tag = THIS.uf_build_rtf_image_tag(abl_image, ls_file_type,  ai_width, ai_height)

RETURN ls_image_tag
end function

public function integer uf_get_width_height (string as_hex, ref integer ai_width, ref integer ai_height);integer 	li_return 		= 1
LONG ll_Pos = 0
string		ls_file_type	= ""
STRING ls_Temp = ""

ls_file_type = THIS.uf_get_file_type( as_hex)

CHOOSE CASE ls_file_type
	CASE cs_JPEG //Jpeg width height a little ugly
		ll_Pos = POS(as_Hex,"ffc0")
		
		IF (ll_Pos > 0) THEN
			ls_Temp = MID(as_Hex, ll_Pos + 7,4)
			ls_Temp = MID(as_Hex, ll_Pos + 15,4)
		END IF

END CHOOSE


return li_return
end function

public function string uf_get_user_signature_rtf_tag (string as_user_id);/*
**		Type  			: Function
**		Name 			: uf_get_user_signature_rtf_tag
**		Arguments 	: string - as_user_id
**		Returns 		:
**		Purpose		: Return an RTF tag contiang a user signature.
**		Date			: 2014/07/??
**		Author			: David Worboys
**	
**		Modifications
*/
RETURN THIS.uf_get_user_signature_rtf_tag(as_user_id, "")
end function

public function string uf_get_user_signature_rtf_tag (string as_user_id, string as_sql, long al_image_width, integer al_image_height);/*
**		Type  			: Function
**		Name 			: uf_get_user_signature_rtf_tag
**		Arguments 	: string - as_user_id, string  as_sql, long - al_image_width, long al_image_height
**		Returns 		:
**		Purpose		: Return an RTF tag contiang a user signature.
**		Date			: 2014/07/??
**		Author			: David Worboys
**	
**		Modifications
*/
BLOB     lbl_signature
STRING ls_Signature_Tag	= ""
STRING ls_SQL 				= ""

IF (ISNULL(as_SQL)  OR TRIM(as_SQL) = "") THEN
	ls_SQL  = "SELECT Signature FROM User_Signature WHERE User_Id ='" +as_user_id+"'"
ELSE
	ls_SQL  = as_SQL
END IF

lbl_Signature = THIS.uf_Get_Blob(ls_SQL)

IF (len(lbl_signature) > 0) THEN //we have a signature
		ls_Signature_Tag = THIS.uf_build_rtf_image_tag(lbl_signature,  al_image_width,al_image_height)
END IF

RETURN ls_Signature_Tag

end function

public function string uf_get_user_signature_rtf_tag (string as_user_id, string as_sql);/*
**		Type  			: Function
**		Name 			: uf_get_user_signature_rtf_tag
**		Arguments 	: string - as_user_id, string - as_SQL
**		Returns 		:
**		Purpose		: Return an RTF tag contiang a user signature.
**		Date			: 2014/07/??
**		Author			: David Worboys
**	
**		Modifications
*/

RETURN THIS.uf_Get_User_Signature_RTF_Tag(as_user_id,as_sql,2000,500 )


end function

public function blob uf_get_blob (string as_sql);/*
**		Type  			: Function
**		Name 			: uf_get_blob
**		Arguments 	: striing - as_SQL
**		Returns 		:
**		Purpose		: Fetch a blob from the database
**		Date			: 2015/03/13
**		Author			: David Worboys
**	
**		Modifications
*/
BLOB     lbl_Blob

IF (ISNULL(as_SQL) OR TRIM(as_SQL) = "") THEN
	SETNULL(lbl_Blob)
ELSE
	DECLARE blob_cursor DYNAMIC CURSOR FOR SQLSA ;
	
	PREPARE SQLSA FROM :as_SQL ;
	OPEN DYNAMIC blob_cursor ;
	FETCH blob_cursor INTO :lbl_blob ;
	
	CLOSE blob_cursor ;
END IF

RETURN lbl_blob

end function

public function long uf_or (long al_value1, long al_value2);Integer		li_Cnt
Long			ll_Result
Boolean		lb_Value1[32], lb_Value2[32]

// Check for nulls
If IsNull(al_Value1) Or IsNull(al_Value2) Then
	SetNull(ll_Result)
	Return ll_Result
End If

// Get all bits for both values
For li_Cnt = 1 To 32
	IF (Int(Mod(al_Value1 / (2 ^(li_Cnt - 1)), 2)) > 0 ) THEN
		lb_Value1[li_Cnt] = TRUE 
	ELSE
		lb_Value1[li_Cnt] = FALSE
	END IF
	
	IF (Int(Mod(al_Value2 / (2 ^(li_Cnt - 1)), 2)) > 0 ) THEN
		lb_Value2[li_Cnt] = TRUE 
	ELSE
		lb_Value2[li_Cnt] = FALSE
	END IF

Next

// Or them together
For li_Cnt = 1 To 32
	If lb_Value1[li_Cnt] Or lb_Value2[li_Cnt] Then
		ll_Result = ll_Result + (2^(li_Cnt - 1))
	End If
Next

Return ll_Result

end function

on n_rtf.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_rtf.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

