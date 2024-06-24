@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'T_Head interface view'
define root view entity ZCHI_T_HEAD 
as select from ztdguit_t_head as HEAD
{
  key headid             as HeadID,
      @EndUserText.label: '公司代碼'   
      bukrs              as Bukrs,
      @EndUserText.label: '公司名稱'   
      butxt              as Butxt,
      @EndUserText.label: '會計文件號碼'       
      belnr              as Belnr,
      @EndUserText.label: '過帳日期'        
      budat              as Budat,
      bldat              as Bldat,
      @EndUserText.label: '稅碼'         
      mwskz              as Mwskz,
      case 
      when syncmsg = '同步錯誤' then 1
      when syncmsg = '尚未同步' then 2 
      when syncmsg = '已同步'  then 3
      else 0
      end as  criticality1,         
      case 
      when hwbas = 0 then 3
      when hwbas > 0 then 2 
      when hwbas < 0 then 1
      else 0
      end as  criticality2,   
      @EndUserText.label: '狀態'        
      syncmsg           as SyncMsg,    
      @EndUserText.label: '銷售金'       
      @Semantics.amount.currencyCode: 'waers'
     
      hwbas              as Hwbas,
      @EndUserText.label: '稅額'       
      @Semantics.amount.currencyCode: 'waers'
      hwste              as Hwste,
      @Semantics.amount.currencyCode: 'waers'
      hwbas2             as Hwbas2,
      @Semantics.amount.currencyCode: 'waers'
      hwste2             as Hwste2,
      waers              as waers,
      icon               as Icon,  
      icon2              as Icon2,
      gjahr              as Gjahr,
      blart              as Blart,
      @EndUserText.label: '建立者'       
      usnam              as Usnam,
      awtyp              as Awtyp,
      awkey              as Awkey,
      stblg              as Stblg,
      @EndUserText.label: '彙開'      
      sum_fg             as SumFg,
      posflag            as Posflag,
      manual             as Manual,
      vbeln              as Vbeln,
      vatcode            as Vatcode,
      zform              as Zform,
      copies             as Copies,
      zexport            as Zexport,
      tax_type           as TaxType,
      tax_per            as TaxPer,
      am_type            as AmType,
      notcheck           as Notcheck,
      @EndUserText.label: 'For Hide Field' 


      hidefield         as Hidefield,
      cust_flag          as CustFlag,
      @EndUserText.label: '彙開銷售額'       
      @Semantics.amount.currencyCode: 'waers'
      hwbas_index        as HwbasIndex,
      @EndUserText.label: '彙開稅額'       
      @Semantics.amount.currencyCode: 'waers'
      hwste_index        as HwsteIndex,
      bukrs_index        as BukrsIndex,
      @EndUserText.label: '彙開代表號'       
      belnr_index        as BelnrIndex,
      gjahr_index        as GjahrIndex,
      @EndUserText.label: '模組' 
      ztdgui_module         as ztdgui_module,
      edigui             as Edigui,
      @EndUserText.label: '刪除'      
      canl               as Canl,
      sample_fg          as SampleFg,
      @EndUserText.label: '淨額彙開'         
      net_fg             as NetFg,
      created_by         as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at         as CreatedAt,
      @Semantics.user.lastChangedBy: true
      lastchangedby      as Lastchangedby,
      @Semantics.systemDateTime.lastChangedAt: true
      lastchangedat      as Lastchangedat,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      locallastchangedat as Locallastchangedat,
      @Semantics.amount.currencyCode: 'waers'
      @EndUserText.label: '銷售額'    
      s_bas as s_bas,
      
      @Semantics.amount.currencyCode: 'waers'
      @EndUserText.label: '稅額'    
      s_ste as s_ste,
      
      @EndUserText.label: '銷售額'   
      @Semantics.amount.currencyCode: 'waers'
      u_bas as u_bas,
      
      @EndUserText.label: '稅額'  
      @Semantics.amount.currencyCode: 'waers'
      u_ste as u_ste,
      
      @EndUserText.label: '銷售額'   
      @Semantics.amount.currencyCode: 'waers'
      c_bas as c_bas,
      @EndUserText.label: '稅額'   
      @Semantics.amount.currencyCode: 'waers'
      c_ste as c_ste,
      @Semantics.amount.currencyCode: 'waers'
      @EndUserText.label: '銷售額'  
      case 
      when belnr = belnr_index then hwbas_index
      else hwbas
      end as  S_BAS2, 
      @Semantics.amount.currencyCode: 'waers'
      @EndUserText.label: '稅額'
      case 
      when belnr = belnr_index then hwste_index
      else hwste
      end as  S_STE2 
}
