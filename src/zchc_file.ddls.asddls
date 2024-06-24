@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'FILE CONSUNPTION VIEW'
@UI: {
  headerInfo: {
    typeName: 'File',
    typeNamePlural: 'File'
  }
}

@Search.searchable: true
define root view entity ZCHC_FILE 
  provider contract transactional_query
  as projection on ZCHI_FILE
{
//      @UI.facet: [ {
//        id: 'idIdentification',
//        type: #IDENTIFICATION_REFERENCE,
//        label: 'txt',
//        position: 10
//      } ]
      @UI.lineItem: [ { position: 10 } 
      ,{ type: #FOR_ACTION, dataAction: 'testMessage', label: 'test message'}
      
//  -->Add by Neil 2024.5.13 Upload Data to 1909 AL11      
      ,{ type: #FOR_ACTION, dataAction: 'uploadDataTo1909', label: 'Upload to AL11'}
//  <--Add by Neil 2024.5.13 Upload Data to 1909 AL11   
   
//                      ,{ type: #FOR_ACTION, dataAction: 'statusUpdate', label: 'Create text', invocationGrouping: #CHANGE_SET}
                        ]
      @UI.hidden: true
  key RUUID,
      @UI.lineItem: [ {
      position: 20 ,
      importance: #MEDIUM,
      label: 'FileNO'
      } ]
      @UI.selectionField:[{position: 20}]
      @Search.defaultSearchElement: true
  key FileNO,
      @UI.lineItem: [ {
      position: 30 ,
      importance: #MEDIUM,
      label: 'Filename'
      } ]
//      @UI.identification: [ {
//        position: 30 ,
//        label: 'Filename'
//      } ]
      Filename,
      @UI.lineItem: [ {
        position: 40 ,
        importance: #MEDIUM,
        label: 'Mimetype'
      } ]
//      @UI.identification: [ {
//        position: 40 ,
//        label: 'Mimetype'
//      } ]
      Mimetype,
      @UI.lineItem: [ {
        position: 50 ,
        importance: #MEDIUM,
        label: 'longString'
      } ]
//      @UI.identification: [ {
//        position: 50 ,
//        label: 'longString'
//      } ]
      @UI.multiLineText: true
      longString,
      @UI.lineItem: [ {
        position: 60 ,
        importance: #MEDIUM,
        label: 'Attachment'
      } ]
//      @UI.identification: [ {
//        position: 60 ,
//        label: 'Attachment'
//      } ]
      Attachment,
      @UI.lineItem: [ {
        position: 70 ,
        importance: #MEDIUM,
        label: 'CreatedBy'
      } ]
//      @UI.identification: [ {
//        position: 70 ,
//        label: 'CreatedBy'
//      } ]
      CreatedBy,
      @UI.lineItem: [ {
        position: 80 ,
        importance: #MEDIUM,
        label: 'CreatedAt'
      } ]
//      @UI.identification: [ {
//        position: 80 ,
//        label: 'CreatedAt'
//      } ]
      CreatedAt,
      @UI.hidden: true
      Locallastchangedat 
}
