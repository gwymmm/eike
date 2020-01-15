with File_Handler;
with Error_Handler;

package UBL_Lexer is
pragma SPARK_Mode( On );

type UBL_Token is 

  ( None, EOF, Begin_FinancialInstitutionBranch,

    End_FinancialInstitutionBranch, HolderName, MultiplierFactorNumeric,

    URI, Value, Amount_With_Attribute, Begin_Attachment, End_Attachment,

    ActualDeliveryDate, Begin_AccountingSupplierParty, 

    End_AccountingSupplierParty, AccountingCost, Begin_AccountingCustomerParty,

    End_AccountingCustomerParty, AddressLine, Begin_AdditionalItemProperty,

    End_AdditionalItemProperty, Begin_AdditionalDocumentReference,

    End_AdditionalDocumentReference, AdditionalStreetName, 

    Begin_AllowanceTotalAmount_With_Attribute, End_AllowanceTotalAmount,

    Begin_AllowanceCharge, End_AllowanceCharge, AllowanceChargeReason,

    AllowanceChargeReasonCode, Begin_BillingReference, End_BillingReference,

    BaseAmount_With_Attribute, BaseQuantity_With_Optional_Attribute,

    BuyerReference, Begin_BuyersItemIdentification, 

    End_BuyersItemIdentification, Begin_CardAccount, End_CardAccount,

    CityName, Begin_ClassifiedTaxCategory, End_ClassifiedTaxCategory,

    CustomizationID, ChargeIndicator, ChargeTotalAmount_With_Attribute,

    Begin_CommodityClassification, End_CommodityClassification, 

    CompanyID_With_Optional_Attribute, CompanyLegalForm, Begin_Contact, 

    End_Contact, Begin_ContractDocumentReference, End_ContractDocumentReference,

    Begin_Country, End_Country, CountrySubentity, 

    CreditedQuantity_With_Attribute, Begin_CreditNoteLine, End_CreditNoteLine,

    CreditNoteTypeCode, DueDate, Begin_DespatchDocumentReference, 

    End_DespatchDocumentReference, Description, DescriptionCode, Begin_Delivery,

    End_Delivery, Begin_DeliveryLocation, End_DeliveryLocation, 

    Begin_DeliveryParty, End_DeliveryParty, DocumentCurrencyCode, 

    DocumentDescription, Begin_DocumentReference, End_DocumentReference,

    ElectronicMail, EmbeddedDocumentBinaryObject_With_Attribute, 

    Begin_ExternalReference, End_ExternalReference, EndDate, 

    EndpointID_With_Attribute, ID_With_Optional_Attribute, IdentificationCode,

    IssueDate, Begin_InvoiceDocumentReference, End_InvoiceDocumentReference,

    InvoicedQuantity_With_Attribute, Begin_InvoiceLine, End_InvoiceLine,

    Begin_InvoicePeriod, End_InvoicePeriod, InvoiceTypeCode, Begin_Item, 

    End_Item, ItemClassificationCode_With_Optional_Attribute, 

    Begin_LegalMonetaryTotal, End_LegalMonetaryTotal, Line, 

    LineExtensionAmount_With_Attribute, LineID, Note, Name, NetworkID,

    Begin_OrderReference, End_OrderReference, Begin_OrderLineReference, 

    End_OrderLineReference, Begin_OriginatorDocumentReference, 

    End_OriginatorDocumentReference, Begin_OriginCountry, End_OriginCountry,

    Percent, Begin_Party, End_Party, Begin_PartyIdentification, 

    End_PartyIdentification, Begin_PartyLegalEntity, End_PartyLegalEntity,

    Begin_PartyName, End_PartyName, Begin_PartyTaxScheme, End_PartyTaxScheme,

    PayableAmount_With_Attribute, PayableRoundingAmount_With_Attribute,

    Begin_PayerFinancialAccount, End_PayerFinancialAccount, Begin_PayeeParty, 

    End_PayeeParty, Begin_PayeeFinancialAccount, End_PayeeFinancialAccount,

    PaymentDueDate, PaymentID, Begin_PaymentMandate, End_PaymentMandate,

    Begin_PaymentMeans, End_PaymentMeans, 

    PaymentMeansCode_With_Optional_Attribute, Begin_PostalAddress, 

    End_PostalAddress, PostalZone, PrepaidAmount_With_Attribute,

    PrimaryAccountNumberID, Begin_Price, End_Price, PriceAmount_With_Attribute,

    ProfileID, Begin_ProjectReference, End_ProjectReference, 

    Begin_ReceiptDocumentReference, End_ReceiptDocumentReference, 

    RegistrationName, SalesOrderID, Begin_SellersItemIdentification, 

    End_SellersItemIdentification, StreetName, StartDate, 

    Begin_StandardItemIdentification, End_StandardItemIdentification,

    Telephone, TaxAmount_With_Attribute, TaxableAmount_With_Attribute,

    TaxInclusiveAmount_With_Attribute, TaxPointDate, 

    Begin_TaxRepresentativeParty, End_TaxRepresentativeParty, Begin_TaxTotal, 

    End_TaxTotal, Begin_TaxCategory, End_TaxCategory, TaxCurrencyCode,

    TaxExclusiveAmount_With_Attribute, TaxExemptionReason, 

    TaxExemptionReasonCode, Begin_TaxScheme, End_TaxScheme, Begin_TaxSubtotal, 

    End_TaxSubtotal );


procedure Next_Token
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token )

  with
    Global => null,
    Depends => (Error_Log => Error_Log, Token => null, null => Input),
    Pre => File_Handler.Is_Open(Input) 
             and then File_Handler.In_Read_Mode(Input);

end UBL_Lexer;