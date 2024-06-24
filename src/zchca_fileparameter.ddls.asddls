@Metadata.allowExtensions: true
@EndUserText.label: 'FILE PARAMETER'
define abstract entity ZCHCA_FILEPARAMETER 
{
    filename           : abap.char(128);
    
    typeName            : abap.char(10) ;
}
