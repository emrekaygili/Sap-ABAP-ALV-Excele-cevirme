# Sap-ABAP-Pratice-4
Sap ABAP ALV Yapısını Excele çevirme 
*&---------------------------------------------------------------------*
*& Report  ZRR_EGT_03
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report zrr_egt_03.
CLASS lcl_main DEFINITION CREATE PUBLIC .
 
   PUBLIC SECTION  .
 
     DATA gt_sflight TYPE TABLE OF sflight .
 
     METHODS :
     get_data RETURNING VALUE(rv_subrc) TYPE sysubrc ,
     download_as_excel,
 
     start_of_selection .
 
 ENDCLASS .
 
 CLASS lcl_main IMPLEMENTATION.
 
     METHOD get_data .
       SELECT * FROM sflight UP TO 100 ROWS INTO TABLE gt_sflight .
       rv_subrc = sy-subrc .
     ENDMETHOD .
 
     METHOD start_of_selection .
       CHECK get_data( ) IS INITIAL .
       download_as_excel( ).
     ENDMETHOD .
 
     METHOD download_as_excel .
 
       DATA lt_fcat         TYPE lvc_t_fcat .
       DATA lr_data         TYPE REF TO data.
       DATA lr_result       TYPE REF TO cl_salv_ex_result_data_table .
       DATA lv_guitype      TYPE i .
       DATA lv_display_mode TYPE i .
       DATA lt_xml_choice   TYPE if_salv_bs_xml=>t_type_xml_choice.
       DATA ls_xml_choice   LIKE LINE OF lt_xml_choice .
       DATA lv_xml_type     TYPE salv_bs_constant .
       DATA lv_xml_version  TYPE string .
       DATA lv_xml_flavour  TYPE string .
       DATA lv_gtyp         TYPE salv_bs_constant .
       DATA lv_xml          TYPE xstring .
 
       CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
         EXPORTING
           i_structure_name = 'SFLIGHT'
         CHANGING
           ct_fieldcat      = lt_fcat.
 
       GET REFERENCE OF gt_sflight[] INTO lr_data.
       cl_salv_ex_util=>factory_result_data_table(
         EXPORTING
           r_data                 = lr_data    " Data table
           t_fieldcatalog         = lt_fcat     
         RECEIVING
           r_result_data_table    = lr_result
       ).
 
       lv_guitype = if_salv_bs_xml=>c_gui_type_gui .
       lv_display_mode = cl_salv_export_xml_dialog=>c_display_mode_inactive_cbox .
       cl_salv_export_xml_dialog=>execute(
         EXPORTING
           gui_type     = lv_guitype
           display_mode = lv_display_mode
         RECEIVING
           value        = lt_xml_choice
       ).
 
       READ TABLE lt_xml_choice INTO ls_xml_choice INDEX 1   .
       IF sy-subrc NE 0.
         RETURN .
       ENDIF.
 
       lv_xml_type = ls_xml_choice-xml_type .
       lv_gtyp =  if_salv_bs_xml=>c_gui_type_gui .
 
       CASE cl_salv_bs_a_xml_base=>get_version( ).
         WHEN if_salv_bs_xml=>version_25.
           lv_xml_version = if_salv_bs_xml=>version_25 .
         WHEN if_salv_bs_xml=>version_26.
           lv_xml_version = if_salv_bs_xml=>version_26 .
       ENDCASE.
 
       lv_xml_flavour = if_salv_bs_c_tt=>c_tt_xml_flavour_export .
       cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform(
        EXPORTING
          r_result_data  = lr_result
          xml_type       = lv_xml_type
          xml_version    = lv_xml_version
          gui_type       = lv_gtyp
          xml_flavour    = lv_xml_flavour
        IMPORTING xml = lv_xml ).
 
   cl_salv_export_xml_dialog=>download( s_xml_choice = ls_xml_choice
                                        xml          =  lv_xml ).
 
     ENDMETHOD .
 ENDCLASS .
 
   DATA go_main TYPE REF TO lcl_main .
 
   START-OF-SELECTION .
    CREATE OBJECT go_main .
 
    go_main->start_of_selection( ) .
