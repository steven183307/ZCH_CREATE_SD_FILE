@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'FILE INTERFACE VIEW'
define root view entity ZCHI_FILE as select from zcht_file
{
  key ruuid              as RUUID,
  key fileno             as FileNO,
      filename           as Filename,
      mimetype           as Mimetype,
      longstring         as longString,
      @EndUserText.label: 'Attachments'
      @Semantics.largeObject:{
          mimeType: 'Mimetype',
          fileName: 'Filename',
          contentDispositionPreference: #ATTACHMENT}
      attachment         as Attachment,
      @Semantics.user.createdBy: true
      createdby         as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      createdat         as CreatedAt,
      @Semantics.user.lastChangedBy: true
      lastchangedby      as Lastchangedby,
      @Semantics.systemDateTime.lastChangedAt: true
      lastchangedat      as Lastchangedat,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      locallastchangedat as Locallastchangedat 
}
