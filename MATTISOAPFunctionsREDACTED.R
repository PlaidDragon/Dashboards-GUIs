library(RMySQL)
library(tidyverse)
library(rio)
library(readxl)
library(dplyr)
library(RCurl) 
library(xml2)
library(RODBC)
library(openxlsx)



source("xxx")

CollapseSearchString <- function(l){
  searchString <- ""
  for(i in 1:length(l)){
    searchString <- paste0(searchString,"'",l[i],"'",sep="")
    if(i!=length(l)){
      searchString <- paste0(searchString,",")
    }
    
  }
  return(searchString)
}
HPCID <- SOAPGetCompanyID()

MATTIVerifyTransaction <- function(salesID){
  #Get Data From MAS
  salesID <- trimws(salesID)
  
  
  dbconnection <- odbcDriverConnect("Driver=SQL Server;Server=xxx; Database=xxx;Uid=xxx; Pwd=xxx;")
  
  #-----data for POs From MAS----

  mas_po_key <- suppressWarnings({ sqlQuery(dbconnection, paste0("Select pr.TranID, pr.TranID2 AS PO, lt.ItemID, lt.LotNo, lt.DistQty As qty, lt.ItemKey, it.CreateDate As TranDate, il.ExpirationDate AS Expiration,
                                        pol.TranID as PO, pol.Description, pol.VendName, pol.VendID, pol.RequestDate
                                        From mas500_app.dbo.vPORcvrID As pr
                                        Left Join mas500_app.dbo.vdvLotTransactions as lt On pr.TranID = lt.TranID
                                        Left Join mas500_app.dbo.vdvInventoryTran As it On lt.ItemID = it.ItemID And pr.TranID = it.TranID
                                        Left Join  mas500_app.dbo.vdvItemLot AS il On il.ItemID = it.ItemID AND lt.ItemKey = il.ItemKey AND lt.LotNo = il.LotNo
                                        Left Join mas500_app.dbo.vdvPurchaseOrderLine AS pol On pol.ItemID = lt.ItemID AND pr.TranID2 = pol.TranID
                                        Where pr.CompanyID = 'HPC' And pr.TranID2 in (", CollapseSearchString(salesID),")"), as.is = T) %>%
    mutate_at(c("TranDate", "Expiration", "RequestDate"), funs(as.Date(.))) %>%
    mutate("qty" = as.numeric(qty))
  })
  
  if(length(mas_po_key$TranID) == 0){
    message("No POs Found in MAS")
    POBillingAddr <- NULL
    }else{
    mas_po_key <- mas_po_key %>%
    filter(nchar(ItemID) == 13) %>%
    group_by(PO,ItemID,LotNo) %>%
    mutate("qty" = sum(qty)) %>%
    ungroup() %>%
    distinct() %>%
    rename(Item = ItemID, Lot = LotNo)
    
    POBillingAddr <- sqlQuery(dbconnection, paste0("SELECT TranID as PO, PurchAddrName as BillToName, PurchAddrLine1 as BillToAddrLine1, PurchAddrLine2 as BillToAddrLine2,
                                                  PurchAddrCity as BillToAddrCity, PurchAddrStateID AS BillToState, PurchAddrPostalCode as BillToZip From vdvPurchaseOrder
                                                 WHERE CompanyID = 'HPC' AND TranID in (", CollapseSearchString(salesID),")"), as.is = T) %>%
      mutate_all(.,funs(as.character(.))) %>%
      mutate_all(.,funs(replace_na(.,""))) 
    
  }
  

  
  
  # #----data for SOs from MAS----
  query <- paste0("Select CustID, SOKey, ItemID, TranDate as RequestDate, Description, ItemKey, ShipDate, CustAddrID, TranID as SO, CustName From vdvSalesOrderLine Where CompanyID = 'HPC' And TranID in ('",salesID,"')")
  tempdf <- NULL
  for(i in 1:length(query)){
    tempdf <- rbind(tempdf, sqlQuery(dbconnection,query[i])) %>% na.omit() %>%
      mutate_at(c("CustAddrID", "ItemID","SO","CustName","ItemKey"),funs(as.character(.))) 
  }
  
  query <- paste0("Select SalesOrder as SO, TranID, ItemID From vdvShipmentLine Where CompanyID = 'HPC'And SalesOrder in ('",salesID,"')")
  tempdf2 <- data.frame("SO" = NA, "TranID" = NA, "ItemID" = NA)
  for(i in 1:length(query)){
    tempdf2 <- rbind(tempdf2, sqlQuery(dbconnection, query[i])) %>% na.omit() %>% mutate_at(c("TranID", "ItemID","SO"), funs(as.character(.)))
  }
  
  query <- paste0("Select ItemID, LotNo, TranID, ItemKey, DistQty AS QtyShipped From vdvLotTransactions Where CompanyID = 'HPC' And TranID in ('",tempdf2$TranID,"') And ItemID in ('",tempdf2$ItemID,"')")
  tempdf3 <- NULL
  for(i in 1:length(query)){
    tempdf3 <- rbind(tempdf3, sqlQuery(dbconnection, query[i], as.is = T)) %>% na.omit() %>% mutate_at(c("TranID", "ItemID","ItemKey", "LotNo"), funs(as.character(.)))
  }
  
  
  mas_so_key <- suppressWarnings(suppressMessages({ tempdf %>% 
      left_join(sqlQuery(dbconnection, paste0("Select CustAddrID, CustID, ContactName, ContactPhone, ContactPhoneExt, ContactEMailAddr, AddrLine1,AddrLine2, AddrName, AddrCity,
                       AddrPostalCode as Zip From vdvCustomerAddr Where CompanyID = 'HPC'
                         AND CustID IN (",CollapseSearchString(unique(tempdf$CustID)),")")), by = c("CustAddrID", "CustID")) %>%
      left_join(tempdf2) %>%
      
      left_join(sqlQuery(dbconnection, paste0("Select TranID, CustName, CustID, ShipToAddrID as ShipTo, ShipToAddrLine1,ShipToAddrLine2, ShipToAddrCity as ShipToCity, ShipToAddrPostalCode as ZipCode,
                                               ShipToAddrState as State,  ShipToAddrName as ShipToName From vdvShipment
                       Where CompanyID = 'HPC'
                       AND TranID IN (",CollapseSearchString(unique(tempdf2$TranID)),")"))) %>%
      
      left_join(tempdf3) %>% 
      left_join(sqlQuery(dbconnection, paste0("Select ItemKey, LotNo, ExpirationDate as Expiration From vdvItemLot
                                        WHERE LotNo IN (",CollapseSearchString(unique(tempdf3$LotNo)),")")) %>%
                  mutate_at(vars("ItemKey","LotNo"), funs(as.character(.))) %>%
                  mutate("Expiration" = as.Date(substr(Expiration,1,10))) %>%
                  distinct()
      ) %>%
      group_by(ItemID, LotNo, SOKey) %>%
      mutate("QtyShipped" = sum(abs(as.numeric(QtyShipped)))) %>%
      ungroup() %>%
      filter(nchar(ItemID) == 13) %>%
      distinct() %>%
      rename(Item = ItemID, Name = CustName) %>%
      filter(!str_detect(Item, "[[:alpha:]]"))
  }))
  
  #Get Billing Address From MAS for SOs
  DistCusts <- unique(mas_so_key$CustID)
  SOBillingAddr <- sqlQuery(dbconnection, paste0("SELECT CustID, BillAddrName, BillAddrLine1, BillAddrLine2, BillAddrCity, BillAddrState, BillAddrPostalCode, ContactName as BillingContact ,ContactPhone as BillingPhone,
                                                  ContactPhoneExt as BillingPhoneExt, ContactEMailAddr as BillingEmail
                                                  FROM vdvCustomer WHERE
                                                  CompanyID = 'HPC'
                                                  AND CustID in (",CollapseSearchString(DistCusts),")")) %>%
    mutate_all(.,funs(as.character(.))) %>%
    mutate_all(.,funs(replace_na(.,"")))
    
  
  odbcCloseAll()
  
  
  #Create columns to be checked for Verification and format Columns for hands on table in Shiny for SOs
  SO_MAS_data <- mas_so_key %>% mutate("Verified" = FALSE) %>% mutate("Not in TrackTrace" = TRUE) %>% select(Verified, "Not in TrackTrace", SO, Name, CustID, Item,Description, Expiration, QtyShipped, LotNo,  RequestDate,ShipDate, ShipTo, ShipToName,  ShipToAddrLine1, ShipToAddrLine2,
                                                                                                             ShipToCity, ZipCode, State, ContactPhone,ContactPhoneExt,ContactEMailAddr) %>%
    mutate_at(c("ShipTo","CustID","Expiration","Description","RequestDate","ShipDate","ShipToName","ShipToAddrLine1","ShipToAddrLine2","ShipToCity","State","ContactPhoneExt","ContactEMailAddr"), funs(as.character(.))) %>%
    mutate_at(c("Expiration", "RequestDate", "ShipDate"), funs(as.Date(substr(.,1,10), format = "%Y-%m-%d"))) %>%
    mutate("ShipDate" = RequestDate) %>%
    mutate("Description" = gsub("&"," AND ",Description))
  
  #Check if SOs exist in MAS
  if(length(SO_MAS_data$Verified) > 0){
    ExistingSOs <- SO_MAS_data %>% select(Item,LotNo,SO) %>% mutate("NumEntries" = NA) #%>% mutate("SOnum" = ifelse(str_count(SO,"-") > 1, sub("-[^-]+$","",SO), SO)) %>% mutate("SOnum" = as.numeric(str_extract(SO, "[[:digit:]].+")))
    for(j in 1:length(ExistingSOs$Item)){
      tryCatch({
      ExistingSOs$NumEntries[j] <- SOAPCheckIfExists(ExistingSOs$Item[j], ExistingSOs$LotNo[j], ExistingSOs$SO[j], Type = "Outbound")
      
      if(as.numeric(ExistingSOs$NumEntries[j]) > 0){
        message(paste0("A SO has been detected in TrackTrace for Item ",  ExistingSOs$Item[j]," Lot ", ExistingSOs$LotNo[j], " On SO ", ExistingSOs$SO[j]))
        SO_MAS_data$`Not in TrackTrace`[j] <- FALSE
      }
      }, error=function(e){
        message("Error with ",ExistingSOs$Item[j],". Does the shipment exist in MAS yet?")
      })
    }
  }
  
  
  if(length(mas_po_key$TranID) > 0){
  #Create columns to be checked for Verification and format Columns for hands on table in Shiny for POs
  PO_MAS_data <- mas_po_key %>% mutate("Verified" = FALSE) %>% mutate("Lot" = as.character(Lot)) %>% rename("ShipDate" = TranDate) %>% mutate("Not in TrackTrace" = TRUE) %>% select(Verified,"Not in TrackTrace", PO, Item,Description, qty, Lot, Expiration, RequestDate, ShipDate, VendName, VendID) %>%
    mutate_at(c("Item","PO","Description","Expiration","RequestDate","ShipDate","VendName", "VendID"), funs(as.character(.))) %>%
    mutate_at(c("Expiration", "RequestDate", "ShipDate"), funs(as.Date(substr(.,1,10), format = "%Y-%m-%d"))) %>%
    mutate("Description" = gsub("&"," AND ",Description)) 
  
  #Check if POs exist in MAS
  if(length(PO_MAS_data$Verified) > 0){
    ExistingPOs <- PO_MAS_data %>% select(Item,Lot,PO) %>% mutate("NumEntries" = NA) #%>% mutate("PO" = ifelse(str_count(PO,"-") > 1, sub("-[^-]+$","",PO), PO))
    for(j in 1:length(ExistingPOs$Item)){
      ExistingPOs$NumEntries[j] <- SOAPCheckIfExists(ExistingPOs$Item[j], ExistingPOs$Lot[j], ExistingPOs$PO[j], Type = "Inbound")
      if(as.numeric(ExistingPOs$NumEntries[j]) > 0){
        message(paste0("A PO has been detected in TrackTrace for Item ",  ExistingPOs$Item[j]," Lot ", ExistingPOs$Lot[j], " On PO ", ExistingPOs$PO[j]))
        PO_MAS_data$`Not in TrackTrace`[j] <- FALSE
      }
    }
  }
  
  
  
  #Format PO data to be shown to user
  if(length(PO_MAS_data$PO) == 0) {message("No POs Found")}else{ #Display message if no pos exist
    for(i in 1:length(PO_MAS_data$VendName)){
      
      MASID <- as.character(PO_MAS_data$VendID[i]) #Get masID for input vendor
      ID <- (SOAPGetIDExtRef(MASID) %>% filter(ExtRef == MASID))$ID #Get TrackTRace Id using the given MASID
      if(is_empty(ID) | length(ID) > 1){
        message(paste0("No Unique ID for ",PO_MAS_data$VendName[i]," found in Track Trace. Skipping")) #Display message  if the input does not exist in Track Trace
       
        suppressWarnings({
         PO_MAS_data$TTID[i] <- "NO UNIQUE ID FOUND" #Fill data with empty values if the vendor was not found
        #PO_MAS_data$ShipFromName[i] <- NA
        PO_MAS_data$ShipFromAdddr1[i] <- ""
        PO_MAS_data$ShipFromAdddr2[i] <- ""
        PO_MAS_data$ShipFromState[i] <- ""
        PO_MAS_data$ShipFromZip[i] <- ""
        PO_MAS_data$ShipFromCity[i] <- ""
        PO_MAS_data$ShipFromCountry[i] <- ""
        })
      }
      if(is_empty(ID) | length(ID) > 1){next} #Go to next item in loop if vendor is not found
      
      defaultaddress <- as.character(SOAPGetVendorInfo(ID)$Address.ID) #Get defualt address to be used as billing address and for contact info
      addressinfo <- SOAPGetAddress(ID) %>% filter(ID == defaultaddress) #Get address info from TrackTrace
      companyname <- as.character(SOAPGetVendorInfo(ID)$Name)
      
      suppressWarnings({
        PO_MAS_data$TTID[i] <- as.character(ID) 
        #PO_MAS_data$ShipFromName[i] <- as.character(addressinfo$Address.Line.1)
        PO_MAS_data$ShipFromAdddr1[i] <- as.character(addressinfo$Address.Line.1)
        PO_MAS_data$ShipFromAdddr2[i] <- as.character(addressinfo$Address.Line.2)
        PO_MAS_data$ShipFromState[i] <- as.character(addressinfo$State)
        PO_MAS_data$ShipFromZip[i] <- as.character(addressinfo$Zip)
        PO_MAS_data$ShipFromCity[i] <- as.character(addressinfo$City)
        PO_MAS_data$ShipFromCountry[i] <- as.character(addressinfo$Country)
      })
    }}
  
  PO_MAS_data <- PO_MAS_data %>% left_join(POBillingAddr)
  }else{PO_MAS_data <- NULL}
  
  if(length(SO_MAS_data$SO) == 0) {message("No SOs Found")}else{ #Display message if no SOs were found
    for(i in 1:length(SO_MAS_data$Name)){
      
      MASID <- as.character(SO_MAS_data$CustID[i]) #Get MAS Id for indexed customer
      ID <- (SOAPGetIDExtRef(toupper(MASID)) %>% filter(ExtRef == toupper(MASID)))$ID #Get TrackTrace ID from MASID
      
      if(is_empty(ID)){message(paste0("ID for ",SO_MAS_data$Name[i]," not found in Track Trace. Creating Customer")) #Display message and fill df with empty values if no TrackTRace ID is found
        source("CreateNewTPV2.R")
        tryCatch({
        CreateTP(MASID)
        CreateShipTo(MASID)
        ID <- (SOAPGetIDExtRef(MASID) %>% filter(ExtRef == MASID))$ID
        },error=function(e){
          message("There was an error when creating the customer")
        })
      }
      if(is_empty(ID)){next} #Go to next index if no TrackTrace ID is found
      
      if(length(ID) > 1){
        message("Multiple profiles found with ",MASID," set as Ext. Ref")
        next
      }
      
      addressinfo <- SOAPGetAddress(ID) %>% filter(str_detect(ShipTo, as.character(SO_MAS_data$ShipTo[i]))) #Find address that corresponds to the ship to listed
      
      if(length(addressinfo$ID) < 1){
        message("Ship to for ", MASID, " not found in TrackTrace.")
        next()
      }
      
      suppressWarnings({
      SO_MAS_data$ContactPhone[i] <- addressinfo$Phone #Fill contact Info
      SO_MAS_data$ContactPhoneExt[i] <- addressinfo$Phone.Ext
      SO_MAS_data$ContactEMailAddr[i] <- addressinfo$Email
      
      SO_MAS_data$TTID[i] <- ID 
      })

      
    }}
  
  #Join with billing address data from MAS
  
  SO_MAS_data <- SO_MAS_data %>% left_join(SOBillingAddr, by = "CustID") %>%
    filter(substr(Item,1,2) == "75")
  
  if(!is.null(PO_MAS_data)){
  PO_MAS_data <- PO_MAS_data %>%
    filter(substr(Item,1,2) == "75")
  }
  
  result <- list(POs = PO_MAS_data, SOs = SO_MAS_data, warns = warnings())
  cat("done \n")
  return(result)
  
}


MATTICreateInboundTransaction <- function(df){
  df <- df %>% mutate_at(c("RequestDate", "Expiration","ShipDate"), funs(as.Date(., origin = "1899-12-30"))) %>% mutate_all(.,funs(gsub("&"," AND ",.))) #Format dates from HOT
  
  #Error check to stop function if nothing is there to push
  if(is.null(df) | length(df$Verified) < 1){
    message("Nothing to Push")
    return(NULL)
    stop("Nothing to push")
  }else{
    output <- df %>% select(PO) %>% mutate("TrackTraceID" = "") #Create holding frame to display POs and their TrackTraceID
    Errors <- NULL
    
    
    tryCatch({
      for(i in 1:length(df$PO)){
        
        MASID <- df$VendID[i] #Define MASID for index
        
        ID <- (SOAPGetIDExtRef(MASID) %>% filter(ExtRef == MASID))$ID #Get TrackTraceID for index
        
        #Skip items that include an EDI enabled vendor with a date that occurs after the vendor's EDI enableing date
        # EDITest <- EDIVendors %>% filter(VendID %in% ID)
        # if(length(EDITest$VendID) > 0){
        #   if(df$RequestDate[i] >= EDITest$StartDate){
        #     message("Vendor ",MASID," is EDI enabled. Skipping")
        #     next()}
        # }

        
        addressinfo <- SOAPGetAddress(ID) #Get addresses for contact info
        defaultaddress <- as.character(SOAPGetVendorInfo(ID)$Address.ID) #Get default address to use as billing address
        companyname <- as.character(SOAPGetVendorInfo(ID)$Name)
        
        body <- paste0("<soapenv:Envelope xmlns:xsi=\"xxx" xmlns:xsd=\"xxx" xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:urn=\"urn:xxx\">
                 <soapenv:Header/>
                  <soapenv:Body>
                    <urn:NewInboundTransaction soapenv:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\">
                      <Auth xsi:type=\"urn:AuthObject\">
                        <ApiKey xsi:type=\"xsd:string\">",APIKey,"</ApiKey>
                        <ApiSecret xsi:type=\"xsd:string\">",APISecret,"</ApiSecret>
                      </Auth>
                      <InboundTransaction xsi:type=\"urn:TPInboundTransaction\">
                      <ItemID xsi:type=\"xsd:string\">",df$Item[i],"</ItemID>
                      <NDC xsi:type=\"xsd:string\">",str_replace(substr(df$Item[i],3,13),"(\\d{5})(\\d{4})(\\d{2})$","\\1-\\2-\\3"),"</NDC>
                      <ProductName xsi:type=\"xsd:string\">",as.character(df$Description[i]),"</ProductName>
                      <Supplier xsi:type=\"xsd:string\">",df$VendName[i],"</Supplier>
                      <Lot xsi:type=\"xsd:string\">",df$Lot[i],"</Lot>
                      <ExpirationDate xsi:type=\"urn:TPDate\">
                        <Day xsi:type=\"xsd:int\">",substr(df$Expiration[i],9,10),"</Day>
                        <Month xsi:type=\"xsd:int\">",substr(df$Expiration[i],6,7),"</Month>
                        <Year xsi:type=\"xsd:int\">",substr(df$Expiration[i],1,4),"</Year>
                      </ExpirationDate>
                      <Quantity xsi:type=\"xsd:int\">",df$qty[i],"</Quantity>
                      <TransactionDate xsi:type=\"urn:TPDate\">
                        <Day xsi:type=\"xsd:int\">",substr(df$ShipDate[i],9,10),"</Day>
                        <Month xsi:type=\"xsd:int\">",substr(df$ShipDate[i],6,7),"</Month>
                        <Year xsi:type=\"xsd:int\">",substr(df$ShipDate[i],1,4),"</Year>
                      </TransactionDate>
                      <ShippingDate xsi:type=\"urn:TPDate\">
                        <Day xsi:type=\"xsd:int\">",substr(df$ShipDate[i],9,10),"</Day>
                        <Month xsi:type=\"xsd:int\">",substr(df$ShipDate[i],6,7),"</Month>
                        <Year xsi:type=\"xsd:int\">",substr(df$ShipDate[i],1,4),"</Year>
                      </ShippingDate>
                      <IsDirectlyBoughtFromManufacturer xsi:type=\"xsd:boolean\">true</IsDirectlyBoughtFromManufacturer>
                      <PONbr xsi:type=\"xsd:string\">",df$PO[i],"</PONbr>
                      <TradingPartnerID xsi:type=\"xsd:int\">",ID,"</TradingPartnerID>
                      <SellerCustomAddress xsi:type=\"urn:TPAddress\">
                         <RecipientName xsi:type=\"xsd:string\">",df$BillToName[i],"</RecipientName>
                         <Line1 xsi:type=\"xsd:string\">",df$BillToAddrLine1[i],"</Line1>
                         <Line2 xsi:type=\"xsd:string\">",df$BillToAddrLine2[i],"</Line2>
                         <CountryCode xsi:type=\"xsd:string\">US</CountryCode>
                         <State xsi:type=\"xsd:string\">",df$BillToState[i],"</State>
                         <City xsi:type=\"xsd:string\">",df$BillToAddrCity[i],"</City>
                         <Zip xsi:type=\"xsd:string\">",df$BillToZip[i],"</Zip>
                         <Phone xsi:type=\"xsd:string\">",as.character(addressinfo$Phone),"</Phone>
                         <PhoneExt xsi:type=\"xsd:string\">",as.character(addressinfo$Phone.Ext),"</PhoneExt>
                      </SellerCustomAddress>
                      <ShipFromCustomAddress xsi:type=\"urn:TPAddress\">
                        <RecipientName xsi:type=\"xsd:string\">",df$VendName[i],"</RecipientName>
                        <Line1 xsi:type=\"xsd:string\">",df$ShipFromAdddr1[i],"</Line1>
                        <Line2 xsi:type=\"xsd:string\">",df$ShipFromAdddr2[i],"</Line2>
                        <CountryCode xsi:type=\"xsd:string\">US</CountryCode>
                        <State xsi:type=\"xsd:string\">",df$ShipFromState[i],"</State>
                        <City xsi:type=\"xsd:string\">",df$ShipFromCity[i],"</City>
                        <Zip xsi:type=\"xsd:string\">",df$ShipFromZip[i],"</Zip>
                        <Phone xsi:type=\"xsd:string\">",as.character(addressinfo$Phone),"</Phone>
                        <PhoneExt xsi:type=\"xsd:string\">",as.character(addressinfo$Phone.Ext),"</PhoneExt>
                      </ShipFromCustomAddress>
                      <TransactionStatementDate xsi:type=\"urn:TPDate\">
                        <Day xsi:type=\"xsd:int\">",substr(df$RequestDate[i],9,10),"</Day>
                        <Month xsi:type=\"xsd:int\">",substr(df$RequestDate[i],6,7),"</Month>
                        <Year xsi:type=\"xsd:int\">",substr(df$RequestDate[i],1,4),"</Year>
                      </TransactionStatementDate>
                     <TransactionStatementName xsi:type=\"xsd:string\">",(SOAPGetIDExtRef(MASID) %>% filter(ExtRef == MASID))$Name,"</TransactionStatementName>
                     <TransactionStatementPhone xsi:type=\"xsd:string\">",as.character(addressinfo$Phone),"</TransactionStatementPhone>
                     <TransactionStatementPhoneExt xsi:type=\"xsd:string\">",as.character(addressinfo$Phone.Ext),"</TransactionStatementPhoneExt>
                     <TransactionStatementEmail xsi:type=\"xsd:string\">",as.character(addressinfo$Email),"</TransactionStatementEmail>
                     <BuyerAddressID xsi:type=\"xsd:int\">",HPCID,"</BuyerAddressID>
                     <ShipToAddressID xsi:type=\"xsd:int\">",HPCID,"</ShipToAddressID>
                    </InboundTransaction>
                  </urn:NewInboundTransaction>
                </soapenv:Body>
              </soapenv:Envelope>")
        
        
        result <- read_xml(getURL(url = paste0(url,"/api/v1_endpoint"),
                                         httpheader = headerFields,
                                         postfields = body[1]))
        tranID <- as.character(result %>% xml_find_all(".//TransactionID") %>% xml_text()) #Find resulting TrackTrace ID
        output$TrackTraceID[i] <- tranID #Fill the holding dataframe with the TrackTrace ID
        
        
      }}, error=function(e){ #Catch Errors
        cat(paste0("ERROR with input ",df$PO[i]," \n R warning message:"), conditionMessage(e), "\n") #Tell what error occured on what input PO
        message(paste0("ERROR with input ",df$PO[i]," \n R warning message:"), conditionMessage(e), "\n")
        Error <- result %>% xml_find_all(".//faultstring") %>% xml_text
        message("API Call Error: ", Error)
        #Send Error info to SQL Table
        body <- gsub("\\'","",body)
        #dbSendQuery(connRVRX, paste0("INSERT INTO `rvrx_warehouse`.`ttrx_errors` (`SalesID`,`Request`,`DateTime`) VALUES ('",df$PO[i],"','", body,"','",Sys.time(),"');"))
        
        output$TrackTraceID[i] <- "ERROR" }) #Fill TrackTrace ID as "ERROR"
    output <- output %>% mutate("TrackTraceID" = ifelse(!str_detect(TrackTraceID, "\\d"),"ERROR",TrackTraceID)) #Fill other errors generated with "ERROR" in output
    lapply(dbListConnections(dbDriver( drv = "MySQL")), dbDisconnect)
    return(output)
  }
}

MATTICreateOutboundTransaction <- function(df){
  #connRVRX <- dbConnect(MySQL(),user='reporting',password='west^realvalue^mysql',host='192.168.200.11')
  #dbSendQuery(connRVRX,"Use rvrx_warehouse")
  BridgeConn = odbcDriverConnect("Driver=ODBC Driver 17 for SQL Server;Server=xxx\\SQLEXPRESS,xxxx;Database=xxxx;Uid=xxxx;Pwd=xxxx;")
  
  #df <- read.xlsx("O:\\Medical\\TrackTraceRx\\so_ids_test.xlsx")
  
  df <- df %>% mutate_at(c("RequestDate", "Expiration","ShipDate"), ~(as.Date(., origin = "1899-12-30"))) %>% #Format input dates from HOT and remove "&" because Tracktrace is touchy
    mutate_at(vars(-CustID), ~gsub("&"," AND ",.)) %>%
    arrange(SO)
  
  output <- df %>% select(SO) %>% mutate("TrackTraceID" = "") %>% distinct() #Create holding dataframe for output table of SOs and assigned TrackTraceIDs

  dfin <- df %>% distinct(SO, .keep_all = T)
  for(i in 1:length(dfin$SO)){
  
    MASID <- dfin$CustID[i] #Index MASId
    ID <- (SOAPGetIDExtRef(toupper(MASID)) %>% filter(ExtRef == toupper(MASID)))$ID #Get trackTraceID from MASID
    
    
    # if(is_empty(ID)){
    #   message("ID for ", MASID," not found in Track Trace")
    #   cat("Creating Customer....\n")
    #   tryCatch({
    #     source("\\\\fileserver\\Medical\\TrackTraceRx\\CreateNewTPV2.R")
    #     ID <- as.character(CreateTP(MASID)$TradingPartnerID)},
    #     error=function(e){
    #       message("Error while creating Trading Partner ",df$CustID[l]," in TrackTrace")
    #       outboundoutput$TrackTraceID[l] <- "ERROR"
    #       next()})
    #   }
    if(is_empty(ID)){next} #Skip if MASID is not found in TrackTrace
    
    
    ShipToID <- SOAPGetAddress(ID) %>% filter(str_detect(ShipTo, dfin$ShipTo[i])) %>% select(ID) #Get ID of Address that pertains to the given Shipto
    companyname <- as.character(SOAPGetVendorInfo(ID)$Name)
    
    Items <- df %>% filter(SO %in% dfin$SO[i]) %>% #filter out items that pertain to the SO and create an NDC from the ITemID
      mutate("NDC" = str_replace(substr(Item,3,13),"(\\d{5})(\\d{4})(\\d{2})$","\\1-\\2-\\3"))
    
    #Create soap envelope for first item on SO
    items <- paste0("<TPPackingSlipItem> 
            <NDC xsi:type=\"xsd:string\">",Items$NDC[1],"</NDC>
            <Lot xsi:type=\"xsd:string\">",Items$LotNo[1],"</Lot>
            <Quantity xsi:type=\"xsd:string\">",Items$QtyShipped[1],"</Quantity>
            </TPPackingSlipItem>")
    
    #Create item envelope if more than one item exists on SO
    if(length(Items$SO) >= 2){
      j <<- 2 #For debugging purposes
      for(j in 2:length(Items$Item)){
        items <- paste0(items,"<TPPackingSlipItem>
                           <NDC xsi:type=\"xsd:string\">",Items$NDC[j],"</NDC>
                            <Lot xsi:type=\"xsd:string\">",Items$LotNo[j],"</Lot>
                            <Quantity xsi:type=\"xsd:string\">",Items$QtyShipped[j],"</Quantity>
                            </TPPackingSlipItem>")
      }
    }
    
    if(dfin$BillAddrState[i] == "PR"){
      dfin$BillAddrState[i] <- dfin$BillAddrCity[i]
      billCountryCode <- "PR"
    }else{
      billCountryCode <- "US"
    }
    if(dfin$State[i] == "PR"){
      dfin$State[i] <- dfin$ShipToCity[i]
      countryCode <- "PR"
    }else{
      countryCode <- "US"
    }
    
    
    #Web Order and tracking info from Derek
    urlResponse <- read_xml(getURL(paste0("http://xxx/xxx/xxx/xxx/masorder?summary=true&format=xml&companyid=HPC&tranno=",gsub("SO-","",dfin$SO[i]))))
    urlList <- as_list(urlResponse)
    
    CustPO <- urlList$QueryResponseOfMasOrderLftI_S1V0$Results$MasOrder$CustPONo[[1]]
    if(is.null(CustPO)){
      tryCatch({
      CustPO <- urlList$QueryResponseOfMasOrderLftI_S1V0$Results$MasOrder$SalesOrder$CustPONo[[1]]
      },error=function(e){
        CustPO <- NULL})
    }
    if(is.null(CustPO)){
      CustPOString <- NULL
    }else{
      CustPOString <- paste0("<PONbr xsi:type=\"xsd:string\">",CustPO,"</PONbr>")
    }
    
    
    TrackingNumber <- ifelse(length(urlList$QueryResponseOfMasOrderLftI_S1V0$Results$MasOrder$tracking_id) > 0,  urlList$QueryResponseOfMasOrderLftI_S1V0$Results$MasOrder$tracking_id[[1]], "NA")
 
    
    Regs <- sqlQuery(BridgeConn, paste0("SELECT [DEAReg], [HIN], [ShipToCode]
                                FROM [ONLINEORDERSDATA].[dbo].[CustomerStateWholesaleLN]
                                WHERE ShipToCode = '",dfin$ShipTo[i],"'"))
    DEANumber <- ifelse(length(Regs) > 0, as.character(Regs$DEAReg), NA)

    #HIN <- as.character(Regs$HIN)
     if(!is.na(DEANumber)){
      LicString <- paste0("<LicenseType xsi:type=\"xsd:string\">11</LicenseType>
                          <LicenseID xsi:type=\"xsd:string\">",DEANumber,"</LicenseID>")
      }else{
      #   LicString <- paste0("<TPIsaIDType xsi:type=\"xsd:string\"></TPIsaIDType>
      #                       <TPIsaID xsi:type=\"xsd:string\">",HIN,"</TPIsaID>")
        LicString <- NULL
    }

    if(dfin$ShipToAddrLine1[i] == dfin$BillAddrLine1[i]){
      BillLicString <- LicString
    }else{
      BillLicString <-  paste0("<LicenseType xsi:type=\"xsd:string\">92</LicenseType>
                                <LicenseID xsi:type=\"xsd:string\">",dfin$CustID[i],"</LicenseID>")
    }
    
    if(substr(MASID,1,2) == "32"){
      approve = "false"
    }else{
      approve = "true"
    }
    
    
    body <- paste0("<soapenv:Envelope xmlns:xsi=\"xxx" xmlns:xsd=\"xxx" xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:urn=\"urn:xxx\" xmlns:soapenc=\"http://schemas.xmlsoap.org/soap/encoding/\">
                   <soapenv:Header/>
                    <soapenv:Body>
                      <urn:SendPackingSlip soapenv:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\">
                        <Auth xsi:type=\"urn:AuthObject\">
                          <ApiKey xsi:type=\"xsd:string\">",APIKey,"</ApiKey>
                          <ApiSecret xsi:type=\"xsd:string\">",APISecret,"</ApiSecret>
                         </Auth>
                         <PackingSlip xsi:type=\"urn:TPPackingSlip\">
                         <TradingPartnerID xsi:type=\"xsd:int\">",ID,"</TradingPartnerID>
                         <OrderDate xsi:type=\"urn:TPDate\">
                          <Day xsi:type=\"xsd:int\">",substr(dfin$RequestDate[i],9,10),"</Day>
                          <Month xsi:type=\"xsd:int\">",substr(dfin$RequestDate[i],6,7),"</Month>
                          <Year xsi:type=\"xsd:int\">",substr(dfin$RequestDate[i],1,4),"</Year>
                         </OrderDate>
                         <SellerAddressID xsi:type=\"xsd:int\">",HPCID,"</SellerAddressID>
                         <ShipDate xsi:type=\"urn:TPDate\">
                          <Day xsi:type=\"xsd:int\">",substr(dfin$ShipDate[i],9,10),"</Day>
                          <Month xsi:type=\"xsd:int\">",substr(dfin$ShipDate[i],6,7),"</Month>
                          <Year xsi:type=\"xsd:int\">",substr(dfin$ShipDate[i],1,4),"</Year>
                         </ShipDate>
                         <BuyerCustomAddress xsi:type=\"urn:TPAddress\">
                          <RecipientName xsi:type=\"xsd:string\">",dfin$BillAddrName[i],"</RecipientName>
                          <Line1 xsi:type=\"xsd:string\">",dfin$BillAddrLine1[i],"</Line1>
                          <Line2 xsi:type=\"xsd:string\">",dfin$BillAddrLine2[i],"</Line2>
                          <CountryCode xsi:type=\"xsd:string\">",billCountryCode,"</CountryCode>
                          <State xsi:type=\"xsd:string\">",dfin$BillAddrState[i],"</State>
                          <City xsi:type=\"xsd:string\">",dfin$BillAddrCity[i],"</City>
                          <Zip xsi:type=\"xsd:string\">",dfin$BillAddrPostalCode[i],"</Zip>
                          <Phone xsi:type=\"xsd:string\">",dfin$BillingPhone[i],"</Phone>
                          <PhoneExt xsi:type=\"xsd:string\">",dfin$BillingPhoneExt[i],"</PhoneExt>
                          <Email xsi:type=\"xsd:string\">",dfin$BillingEmail[i],"</Email>",
                          BillLicString,"
                         </BuyerCustomAddress>
                         <ShipFromAddressID xsi:type=\"xsd:int\">",HPCID,"</ShipFromAddressID>
                         <ShipToCustomAddress xsi:type=\"urn:TPAddress\">
                          <RecipientName xsi:type=\"xsd:string\">",dfin$ShipToName[i],"</RecipientName>
                          <Line1 xsi:type=\"xsd:string\">",dfin$ShipToAddrLine1[i],"</Line1>
                          <Line2 xsi:type=\"xsd:string\">",dfin$ShipToAddrLine2[i],"</Line2>
                          <CountryCode xsi:type=\"xsd:string\">",countryCode,"</CountryCode>
                          <State xsi:type=\"xsd:string\">",dfin$State[i],"</State>
                          <City xsi:type=\"xsd:string\">",dfin$ShipToCity[i],"</City>
                          <Zip xsi:type=\"xsd:string\">",dfin$ZipCode[i],"</Zip>
                          <Phone xsi:type=\"xsd:string\">",dfin$ContactPhone[i],"</Phone>
                          <PhoneExt xsi:type=\"xsd:string\">",dfin$ContactPhoneExt[i],"</PhoneExt>
                          <Email xsi:type=\"xsd:string\">",dfin$ContactEMailAddr[i],"</Email>",
                          LicString,"
                         </ShipToCustomAddress>
                         <InvoiceNbr xsi:type=\"xsd:string\">",dfin$SO[i],"</InvoiceNbr>
                         <OrderNbr xsi:type=\"xsd:string\">",dfin$SO[i],"</OrderNbr>",
                          CustPOString,"
                         <TrackingNumber xsi:type=\"xsd:string\">",TrackingNumber,"</TrackingNumber>
                         <ApproveTransactions xsi:type=\"xsd:boolean\">",approve,"</ApproveTransactions>
                         <Items>",
                          items,"
                         </Items>
                        </PackingSlip>
                      </urn:SendPackingSlip>
                    </soapenv:Body>
                  </soapenv:Envelope>")

    tryCatch({ #Catch Errors
      result <- read_xml(getURL(url = paste0(url,"/api/v1_endpoint"),
                                       httpheader = headerFields,
                                       postfields = body[1]))
      tranID <- as.character(result %>% xml_find_all(".//item") %>% xml_text()) #Find Transaction IDs
      output$TrackTraceID[i] <- paste0(tranID, collapse = ", ") #Fill output holding frame with generated Transaction ID
      if(is_empty(tranID)){
        message("API Call Error: ", result %>% xml_find_all(".//faultstring") %>% xml_text)
      }
      
      
    }, error=function(e){
      cat(paste0("ERROR with input ",dfin$SO[i]," \n R warning message:"), conditionMessage(e), "\n") #Display what input and what error occured
      message(paste0("ERROR with input ",dfin$SO[i]," \n R warning message:"), conditionMessage(e), "\n")
      Error <- result %>% xml_find_all(".//faultstring") %>% xml_text
      message("API Call Error: ", Error)
      
      #Send error Data to SQL Table
      #body <- cat(gsub("\\'","",body))
      #dbSendQuery(connRVRX, paste0("INSERT INTO `rvrx_warehouse`.`ttrx_errors` (`SalesID`,`Request`,`DateTime`) VALUES ('",df$SO[i],"','", body,"','",Sys.time(),"');"))
      
      output$TrackTraceID[i] <- "ERROR"}) #Fill output holding frame with "ERROR" if one occured
    
  }
  output <- output %>% mutate("TrackTraceID" = ifelse(!str_detect(TrackTraceID, "\\d"),"ERROR",TrackTraceID))   #Fill other IDs with "ERROR" if empty
  odbcCloseAll()
  return(output)
  
}
