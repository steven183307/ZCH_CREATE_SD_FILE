CLASS lhc_ZCHI_T_HEAD DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR zchi_t_head RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zchi_t_head RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ zchi_t_head RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK zchi_t_head.

    METHODS create_file FOR MODIFY
      IMPORTING keys FOR ACTION zchi_t_head~create_file.



ENDCLASS.

CLASS lhc_ZCHI_T_HEAD IMPLEMENTATION.

  METHOD get_instance_features.
  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD read.
    SELECT *
  FROM zchi_t_head
  INTO TABLE @DATA(lt_head).

    MOVE-CORRESPONDING lt_head TO result.

  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD create_file.

    DATA l_uuid TYPE string.
    "生成一個Excel 空檔
    DATA(lo_write_access) = xco_cp_xlsx=>document->empty( )->write_access( ).
    "建立Excel 頁籤
    DATA(lo_worksheet) = lo_write_access->get_workbook(
      )->worksheet->at_position( 1 ).
    DATA: descript TYPE string.
    DATA l_tabix TYPE i.
    DATA: l_column TYPE i.
    DATA: lw_tab_ref        TYPE REF TO data.
    DATA: lo_tablestructure TYPE REF TO cl_abap_structdescr.
    DATA: ls_file TYPE STRUCTURE FOR CREATE zchi_file,
          lt_file TYPE TABLE FOR CREATE zchi_file.
    DATA: l_msg TYPE string.
    "檔案名
    DATA l_filename TYPE string.
    DATA: lv_json   TYPE /ui2/cl_json=>json.
    DATA: l_type         TYPE string,
          l_mintype(128),
          l_attachment   TYPE xstring,
          l_longString   TYPE string.
*    DATA(lv_file_content) = lo_write_access->get_file_content( ).

    " A selection pattern that was obtained via XCO_CP_XLSX_SELECTION=>PATTERN_BUILDER.
    DATA(lo_selection_pattern) = xco_cp_xlsx_selection=>pattern_builder->simple_from_to( )->get_pattern( ).

    IF keys[ 1 ]-%param-filename IS INITIAL OR keys[ 1 ]-%param-typeName IS INITIAL.

      l_msg = '檔案名稱 or TypeName不能為空'.
      INSERT VALUE #(
       %msg = new_message_with_text(
                                     severity = if_abap_behv_message=>severity-warning
                                     text     = l_msg )
  ) INTO TABLE reported-zchi_t_head.

    ELSE.

      "計算當前筆數 用於排列序號
      SELECT COUNT( * )
      FROM zcht_file INTO @DATA(l_count).
      IF l_count IS INITIAL.
        l_count = 1.
      ELSE.
        l_count += 1.
      ENDIF.
      "讀取所選的欄位資料
      READ ENTITIES OF zchi_t_head IN LOCAL MODE
      ENTITY zchi_t_head
      ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_result).

      CREATE DATA lw_tab_ref LIKE LINE OF lt_result.
      "將lw_tab_ref 描述寫入 lo_tablestructure 為了得到欄位name
      lo_tablestructure ?= cl_abap_typedescr=>describe_by_data_ref( lw_tab_ref ).

      "寫入 Field Value
      LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_result4>).

        IF sy-tabix = 1.
          "寫入 Field Name
          LOOP AT  lo_tablestructure->components ASSIGNING FIELD-SYMBOL(<ls_tab1>) .

            IF sy-tabix = 1.
              descript = <ls_tab1>-name.
            ELSE.
              descript = descript &&  | \t | &&  <ls_tab1>-name.
            ENDIF.

            AT LAST.
              descript = descript && | \n |.
            ENDAT.

          ENDLOOP.
        ENDIF.

        CLEAR l_column.

        "讀取structure 欄位value 謝入對應的Field Name
        LOOP AT lo_tablestructure->components REFERENCE INTO DATA(lo_component1).

          l_column = l_column + 1.

          ASSIGN COMPONENT lo_component1->name OF STRUCTURE <ls_result4> TO FIELD-SYMBOL(<lfs_value4>).

          IF sy-subrc = 0.
            IF l_column = 1.
              descript = descript && <lfs_value4>.
            ELSE.
              descript = descript &&  | \t | && <lfs_value4>.
            ENDIF.

          ENDIF.

          AT LAST.
            descript = descript && | \n |.
          ENDAT.


        ENDLOOP.

      ENDLOOP.

      CASE  keys[ 1 ]-%param-typeName.

        WHEN 'JSON'.

          l_type = '.json'.
          l_mintype = 'application/json'.
          " serialize table lt_flight into JSON, skipping initial fields and converting ABAP field names into camelCase
          lv_json = /ui2/cl_json=>serialize( data          = lt_result
                                             pretty_name   = /ui2/cl_json=>pretty_mode-camel_case
                                             compress      = abap_true
                                            ).
          l_longstring = lv_json.
          DATA(lxstring) =  xco_cp=>string( lv_json )->as_xstring( xco_cp_character=>code_page->utf_8
                           )->value.
          TRY.
              DATA(xstring) = cl_abap_conv_codepage=>create_out( )->convert( lv_json ).
            CATCH cx_sy_conversion_codepage INTO DATA(ex).
          ENDTRY.

          l_longstring = descript.
          l_attachment = lxstring.

        WHEN 'XLSX'.

          l_type = '.xlsx'.
          l_mintype = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'.

          " serialize table lt_flight into JSON, skipping initial fields and converting ABAP field names into camelCase
          lv_json = /ui2/cl_json=>serialize( data          = lt_result
                                             pretty_name   = /ui2/cl_json=>pretty_mode-camel_case
                                             compress      = abap_true
                                            ).
          l_longstring = lv_json.

          "建立格式
          lo_worksheet->select( lo_selection_pattern
            )->row_stream(
            )->operation->write_from( REF #( lt_result )
            )->execute( ).

          "將lw_tab_ref 描述寫入 lo_tablestructure 為了得到欄位name
          lo_tablestructure ?= cl_abap_typedescr=>describe_by_data_ref( lw_tab_ref ).

          "宣告開始寫入位置
          DATA(lo_cursor) = lo_worksheet->cursor(
               io_column = xco_cp_xlsx=>coordinate->for_alphabetic_value( 'A' )
               io_row    = xco_cp_xlsx=>coordinate->for_numeric_value( 1 )
              ).

          "寫入 Field Name
          LOOP AT  lo_tablestructure->components ASSIGNING FIELD-SYMBOL(<ls_tab>) .

            IF sy-tabix = 1.
              lo_cursor->get_cell( )->value->write_from( <ls_tab>-name ).

            ELSE.
              lo_cursor->move_right( )->get_cell( )->value->write_from( <ls_tab>-name ).

            ENDIF.

          ENDLOOP.

          "寫入 Field Value
          LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_result2>).

            l_tabix = sy-tabix + 1.

            CLEAR l_column.

            DATA(lo_cursor2) = lo_worksheet->cursor(
              io_column = xco_cp_xlsx=>coordinate->for_alphabetic_value( 'A' )
              io_row    = xco_cp_xlsx=>coordinate->for_numeric_value( l_tabix )
            ).
            "讀取structure 欄位value 謝入對應的Field Name
            LOOP AT lo_tablestructure->components REFERENCE INTO DATA(lo_component).

              l_column = l_column + 1.

              ASSIGN COMPONENT lo_component->name OF STRUCTURE <ls_result2> TO FIELD-SYMBOL(<lfs_value>).

              IF sy-subrc = 0.
                IF l_column = 1.
                  lo_cursor2->get_cell( )->value->write_from( <lfs_value> ).
                ELSE.
                  lo_cursor2->move_right( )->get_cell( )->value->write_from( <lfs_value> ).
                ENDIF.
              ENDIF.

            ENDLOOP.

          ENDLOOP.

          l_attachment =  lo_write_access->get_file_content( ).

        WHEN 'TXT'.
          l_type = '.txt'.
          l_mintype = 'text/plain'.




          DATA(lxstring2) =  xco_cp=>string( descript )->as_xstring( xco_cp_character=>code_page->utf_8
                           )->value.
          TRY.
              DATA(xstring2) = cl_abap_conv_codepage=>create_out( )->convert( descript ).
            CATCH cx_sy_conversion_codepage INTO DATA(ex2).
          ENDTRY.

          l_longstring = descript.
          l_attachment = lxstring2.

        WHEN 'XML'.

          l_type = '.xml'.
          l_mintype = 'text/xml'.

          """"""""""""""""""""""""""""""""""""""""""""""""""""""
          TYPES: BEGIN OF ls_item,
                   include TYPE ztdguit_billdetl,
*                   uuidi              TYPE ztdguit_billdetl-uuidi,
*                   seq                TYPE ztdguit_billdetl-seq,
*                   billingnum         TYPE ztdguit_billdetl-billingnum,
*                   prodname           TYPE ztdguit_billdetl-prodname,
*                   quantity           TYPE ztdguit_billdetl-quantity,
*                   price              TYPE ztdguit_billdetl-price,
*                   memo               TYPE ztdguit_billdetl-memo,
*                   created_by         TYPE ztdguit_billdetl-created_by,
*                   created_at         TYPE ztdguit_billdetl-created_at,
*                   lastchangedby      TYPE ztdguit_billdetl-lastchangedby,
*                   lastchangedat      TYPE ztdguit_billdetl-lastchangedat,
*                   locallastchangedat TYPE ztdguit_billdetl-locallastchangedat,
                 END OF ls_item.

          TYPES: t_item TYPE STANDARD TABLE OF ls_item WITH NON-UNIQUE KEY table_line.

          TYPES: BEGIN OF ls_heade,
                   include TYPE ztdguit_billing,
*                   billingnum         TYPE ztdguit_billing-billingnum,
*                   sellernum          TYPE ztdguit_billing-sellernum,
*                   sellername         TYPE ztdguit_billing-sellername,
*                   buyernum           TYPE ztdguit_billing-buyernum,
*                   amount             TYPE ztdguit_billing-amount,
*                   quantityall        TYPE ztdguit_billing-quantityall,
*                   random             TYPE ztdguit_billing-random,
*                   type               TYPE ztdguit_billing-type,
*                   date01             TYPE ztdguit_billing-date01,
*                   time01             TYPE ztdguit_billing-time01,
*                   created_by         TYPE ztdguit_billing-created_by,
*                   created_at         TYPE ztdguit_billing-created_at,
*                   lastchangedby      TYPE ztdguit_billing-lastchangedby,
*                   lastchangedat      TYPE ztdguit_billing-lastchangedat,
*                   locallastchangedat TYPE ztdguit_billing-locallastchangedat,
                   inner              TYPE  t_item,
                 END OF ls_heade.

          DATA: ls_header TYPE ls_heade,
                ls_item3  TYPE LS_item.

          DATA: lt_table    TYPE STANDARD TABLE OF ls_heade WITH NON-UNIQUE KEY include-billingnum.

          SELECT SINGLE *
            FROM ztdguit_billdetl
           WHERE uuidi = 'BE14B431B7961EEEBAE7D59AB5924821'
            INTO @DATA(ls_item2).

*
          SELECT SINGLE *
            FROM ztdguit_billing
           WHERE billingnum = 'ZH94304406'
            INTO @DATA(ls_head).
*

*          MOVE-CORRESPONDING ls_item2 TO ls_header-inner.
*          ls_item2-uuidi = 'BE14B431B7961EEEBAE7D59AB5924822'.
          MOVE-CORRESPONDING ls_item2 TO ls_item3.
*            APPEND ls_item2 TO ls_header-inner.
            INSERT ls_item3 INTO TABLE ls_header-inner.
            INSERT ls_item3 INTO TABLE ls_header-inner.
*            ls_header-inner = ls_item2.
          IF ls_head IS NOT INITIAL.

            INSERT ls_header INTO TABLE lt_table.
            INSERT ls_header INTO TABLE lt_table.
          ENDIF.

          """"""""""""""""""""""""""""""""""""""""""""""""""""""


*          TYPES: BEGIN OF inner_ty,
*                   a TYPE string,
*                   b TYPE string,
*                 END OF inner_ty,
*
*                 t_inner_ty TYPE STANDARD TABLE OF inner_ty WITH NON-UNIQUE KEY table_line,
*
*                 BEGIN OF outer_ty,
*                   c     TYPE string,
*                   inner TYPE t_inner_ty,
*                 END OF outer_ty.
*
*
*          DATA:BEGIN OF ls_item,
*                 a TYPE string,
*                 b TYPE string,
*               END OF ls_item.
*
*          DATA: lt_table    TYPE STANDARD TABLE OF outer_ty WITH NON-UNIQUE KEY c,
*                ls_wa_outer TYPE outer_ty,
*                ls_wa_inner TYPE inner_ty.
*          DATA ls_outer TYPE outer_ty.






*          ls_wa_inner-a = 'EE'.
*          ls_wa_inner-b = 'EEE'.
*          INSERT ls_wa_inner INTO TABLE ls_outer-inner.
*
*          ls_outer-c = '1'.
*
*          INSERT ls_outer INTO TABLE  lt_table.

*          CALL TRANSFORMATION id SOURCE lt_a = lt_result
              CALL TRANSFORMATION id SOURCE lt_a = lt_table
                                RESULT XML FINAL(xml_xstring).

          l_longstring = descript.
          l_attachment = xml_xstring.

      ENDCASE.

      l_filename = keys[ 1 ]-%param-filename && l_type.

      ls_file = VALUE #( %cid = '3'
                        FileNO = l_count
                        Attachment = l_attachment
                        mimetype = l_mintype
                        filename = l_filename
                        longString = l_longString
                        %control-Attachment = if_abap_behv=>mk-on
                        %control-mimetype = if_abap_behv=>mk-on
                        %control-filename = if_abap_behv=>mk-on
                        %control-longString = if_abap_behv=>mk-on
                        %control-FileNO = if_abap_behv=>mk-on
                        ).

      APPEND ls_file TO lt_file.

      MODIFY ENTITIES OF zchi_file
         ENTITY zchi_file
         CREATE FROM lt_file
         MAPPED DATA(mapped3)
         FAILED DATA(Failed1)
         REPORTED DATA(report3).

      LOOP AT mapped3-zchi_file ASSIGNING FIELD-SYMBOL(<ls_map>).
        DATA(lv_result) = <ls_map>-ruuid.
      ENDLOOP.

      l_uuid = lv_result.
      "SEND MAIL
*      TRY.
*
*          DATA(lo_mail) = cl_bcs_mail_message=>create_instance( ).
*
*          lo_mail->set_sender( 'chase80090123@gmail.com' ).
*
**          lo_mail->add_recipient( 'chase80090123@gmail.com' ).
*          lo_mail->add_recipient( 'chasechou@soetek.com.tw' ).
*          lo_mail->add_recipient( iv_address = 'recipient2@yourcompany.com' iv_copy = cl_bcs_mail_message=>cc ).
*
*          lo_mail->set_subject( 'Test Mail' ).
*          lo_mail->set_main( cl_bcs_mail_textpart=>create_text_html( '<h1>Hello</h1><p>This is a test mail.</p>' ) ).
*          lo_mail->add_attachment( cl_bcs_mail_textpart=>create_text_plain(
*            iv_content      = 'This is a text attachment'
*            iv_filename     = 'Text_Attachment.txt'
*          ) ).
*          lo_mail->add_attachment( cl_bcs_mail_textpart=>create_instance(
*             iv_content      = '<note><to>John</to><from>Jane</from><body>My nice XML!</body></note>'
*             iv_content_type = 'text/xml'
*             iv_filename     = 'Text_Attachment.xml'
*             ) ).
*          lo_mail->send( IMPORTING et_status = DATA(lt_status) ).
**          lo_mail->set_main( cl_bcs_mail_textpart=>create_instance(
**              iv_content      = '<h1>Hello</h1><p>Hello world send from RAP!</p>'
**              iv_content_type = 'text/html' ) ).
**
**          lo_mail->send( IMPORTING et_status = DATA(lt_status) ).
**          DATA(ld_mail_content) = ``.
**          LOOP AT lt_result INTO DATA(ls_invoice).
**            ld_mail_content &&= |Doc: { ls_invoice-SalesDocument }, From: { ls_invoice-SalesDocumentItem } |.
**          ENDLOOP.
**          lo_mail->add_attachment( cl_bcs_mail_textpart=>create_instance(
**
**                                                                  iv_content      = ld_mail_content
**                                                                  iv_content_type = 'text/plain'
**                                                                  iv_filename     = 'Attachment.txt' ) ).
*
*        CATCH cx_bcs_mail INTO DATA(lo_err).
*
*      ENDTRY.
*
*      TRY.
*          DATA(lo_mail2) = cl_bcs_mail_message=>create_instance(  ).
*          lo_mail2->set_sender( 'DoNotReply@my405649.mail.s4hana.cloud.sap' ).  "可以寄到內/外部
*
*          lo_mail2->add_recipient( 'chase80090123@gmail.com' ).
**      lo_mail->add_recipient( 'kevinlee7755@gmail.com' ).
**      lo_mail->add_recipient( 'chasechou@soetek.com.tw' ).
*
*          lo_mail2->set_subject( |Test Email from S4HC| ).
*          lo_mail2->set_main( cl_bcs_mail_textpart=>create_instance(
*            iv_content = |<h1>Test1023</h1><p>This is a test mail in HTML format.</p><br>Hi</br>|
*            iv_content_type = 'text/html' ) ).
*          lo_mail->add_attachment( cl_bcs_mail_textpart=>create_instance(
*             iv_content      = '<note><to>John</to><from>Jane</from><body>My nice XML!</body></note>'
*             iv_content_type = 'text/xml'
*             iv_filename     = 'Text_Attachment.xml'
*             ) ).
**      cl_abap_tx=>save(  ).
**      lo_mail2->send_async(  ).
*
*
*        CATCH cx_bcs_mail INTO DATA(lx_mail).
*
*      ENDTRY.



      IF failed IS INITIAL.
        l_filename = l_filename && ' 建立成功'.
        l_msg = 'FileＮＯ : ' && l_count  && |\n 檔案: | && l_filename && |\n UUID: | && l_uuid .
        INSERT VALUE #(
         %msg = new_message_with_text(
                                       severity = if_abap_behv_message=>severity-success
                                       text     = l_msg )
    ) INTO TABLE reported-zchi_t_head.
      ENDIF.

    ENDIF.

  ENDMETHOD.


ENDCLASS.

CLASS lsc_ZCHI_T_HEAD DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZCHI_T_HEAD IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.
  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
