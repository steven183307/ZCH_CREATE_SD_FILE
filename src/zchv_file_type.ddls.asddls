@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'FILE TYPE VALUE HELP'

@ObjectModel.resultSet.sizeCategory: #XS
define view entity zchv_file_type
  as select from zcht_type{
  @UI.hidden: true
  key ruuid,
  key type as typeName
}
