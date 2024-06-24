CLASS lhc_ZCHI_FILE DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.
    DATA g_count TYPE n VALUE 0.
    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zchi_file RESULT result.

    METHODS check_data FOR DETERMINE ON MODIFY
      IMPORTING keys FOR zchi_file~check_data.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR zchi_file RESULT result.

    METHODS testmessage FOR MODIFY
      IMPORTING keys FOR ACTION zchi_file~testmessage RESULT result.

    METHODS uploaddatato1909 FOR MODIFY
      IMPORTING keys FOR ACTION zchi_file~uploaddatato1909
*      RESULT result
      .


ENDCLASS.

CLASS lhc_ZCHI_FILE IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD check_data.
  ENDMETHOD.



  METHOD get_instance_features.
  ENDMETHOD.

  METHOD testMessage.
    DATA: ls_test TYPE STRUCTURE FOR UPDATE zchi_file,
          lt_test TYPE TABLE FOR UPDATE zchi_file.
    READ ENTITIES OF zchi_file IN LOCAL MODE
      ENTITY zchi_file
      ALL FIELDS  WITH CORRESPONDING #( keys )
      RESULT DATA(checkXblnrs).

    LOOP AT checkxblnrs ASSIGNING FIELD-SYMBOL(<fs_text2>).

      IF <fs_text2>-longString <> 'W'.
        INSERT VALUE #(
                   %msg = new_message( id = '1'
                      number  = 01
                      severity = if_abap_behv_message=>severity-warning
                      v1 = |R U SURE?|
*              text     = |R U SURE?|
                      )
            ) INTO TABLE reported-zchi_file.
      ENDIF.
    ENDLOOP.
*
    LOOP AT checkxblnrs ASSIGNING FIELD-SYMBOL(<fs_text>).
      <fs_text>-longString = 'W'.
      MOVE-CORRESPONDING <fs_text> TO ls_test.
      ls_test-%control-longString = 01.
      APPEND ls_test TO lt_test.
      UPDATE zcht_file FROM @( VALUE #( ruuid =  <fs_text>-ruuid
                                        fileno  = <fs_text>-FileNO
                                  longString = 'W' ) ).
    ENDLOOP.

*       MODIFY ENTITIES OF zchi_file IN LOCAL MODE
*         ENTITY zchi_file
*         UPDATE FROM lt_test.

*     LOOP AT checkXblnrs INTO DATA(checkXblnr).
*           APPEND VALUE #( %tky = checkXblnr-%tky ) TO failed-zchi_file.
*          APPEND VALUE #(
*          %tky                 = checkXblnr-%tky
**                         %state_area          = 'VALIDATE_XBLNR1'
**                         %path = VALUE #(
**                                          _head-%is_draft   = checkXblnr-%is_draft
**                                          _head-%key-HeadID = checkXblnr-HeadID
**                                         )
*                           %msg = new_message_with_text(
*                                  severity = if_abap_behv_message=>severity-warning
*                                  text     = '憑證號碼格式錯誤' )
**                         %element-Xblnr  = if_abap_behv=>mk-on
*                         ) TO reported-zchi_file.
*                         ENDLOOP.
  ENDMETHOD.

  METHOD uploadDataTo1909.

    DATA: l_msg  TYPE string,
          l_txt  TYPE zcht_file-Attachment,
          l_xml  TYPE zcht_file-Attachment,
          l_json TYPE zcht_file-Attachment,
          l_xlsx TYPE zcht_file-Attachment.

    READ ENTITIES OF zchi_file IN LOCAL MODE
      ENTITY zchi_file
      ALL FIELDS  WITH CORRESPONDING #( keys )
      RESULT DATA(lt_data_for_1909).

    LOOP AT lt_data_for_1909 INTO DATA(ls_df1909).
      CASE ls_df1909-Mimetype.

        WHEN 'text/plain'.
          l_txt = ls_df1909-Attachment.

        WHEN 'text/xml'.
          l_xml = ls_df1909-Attachment.

        WHEN 'application/json'.
          l_json = ls_df1909-Attachment.

        WHEN 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'.
          l_xlsx = ls_df1909-Attachment.

      ENDCASE.
    ENDLOOP.

    TRY.
        DATA(lo_destination) = cl_rfc_destination_provider=>create_by_comm_arrangement(
                               comm_scenario   = 'ZNE_OUTBOUND_RFC_001_CSCEN'  " Communication scenario
                               service_id      = 'ZNE_OUTBOUND_RFC_001_SRFC'   " Outbound service
                               comm_system_id  = 'ZNE_OUTBOUND_RFC_CSYS'       " Communication system
                            ).
        DATA(lv_destination) = lo_destination->get_destination_name( ).
        DATA msg TYPE c LENGTH 255.

        CALL FUNCTION 'ZNEFM_AL11_TEST01'
          DESTINATION lv_destination
          EXPORTING
            i_txt                 = l_txt
            i_xml                 = l_xml
            i_json                = l_json
            i_xlsx                = l_xlsx
          IMPORTING
            e_msg                 = l_msg
*          TABLES
*           et_head               = lt_t_head
*           et_guit11             = lt_t_detail
*           et_sum                = lt_detail_sum
          EXCEPTIONS
            system_failure        = 1 MESSAGE msg
            communication_failure = 2 MESSAGE msg
            OTHERS                = 3.

        CASE sy-subrc.
          WHEN 0.

            INSERT VALUE #(
                   %msg = new_message_with_text(
                      severity = if_abap_behv_message=>severity-information
                      text     = l_msg
                      )
            ) INTO TABLE reported-zchi_file.

          WHEN 1.
            INSERT VALUE #(
                   %msg = new_message_with_text(
                      severity = if_abap_behv_message=>severity-information
                      text     = |EXCEPTION SYSTEM_FAILURE | && msg
                      )
            ) INTO TABLE reported-zchi_file.

          WHEN 2.
            INSERT VALUE #(
                   %msg = new_message_with_text(
                      severity = if_abap_behv_message=>severity-information
                      text     = |EXCEPTION COMMUNICATION_FAILURE | && msg
                      )
            ) INTO TABLE reported-zchi_file.
*                WHEN 3.
*                  out->write( |EXCEPTION OTHERS| ).
        ENDCASE.


      CATCH cx_root INTO DATA(lx_root).
*        out->write(  lx_root->get_longtext( ) ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
