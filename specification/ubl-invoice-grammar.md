    -- Electronic Invoicing Kit for EU (EIKE) - Tools for EN 16931 E-Invoices
    -- Copyright (C) 2019  Dmitrij Novikov
    --
    -- This program is free software: you can redistribute it and/or modify
    -- it under the terms of the GNU General Public License as published by
    -- the Free Software Foundation, either version 3 of the License, or
    -- (at your option) any later version.
    --
    -- This program is distributed in the hope that it will be useful,
    -- but WITHOUT ANY WARRANTY; without even the implied warranty of
    -- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    -- GNU General Public License for more details.
    --
    -- You should have received a copy of the GNU General Public License
    -- along with this program.  If not, see <https://www.gnu.org/licenses/>.

# UBL Invoice Grammar

    ubl-invoice := 'customization-id' post-customization-id

    post-customization-id := 'profile-id' post-profile-id
                          or 'id' post-id 

    post-profile-id := 'id' post-id

    post-id := 'issue-date' post-issue-date

    post-issue-date := 'due-date' post-due-date
                    or 'invoice-type-code' post-invoice-type-code

    post-due-date := 'invoice-type-code' post-invoice-type-code

    post-invoice-type-code := 'note' post-note

    post-note := 'note' post-note-2
          or 'tax-point-date' post-tax-point-date
          or 'document-currency-code' post-document-currency-code

    post-note-2 := 'tax-point-date' post-tax-point-date
            or 'document-currency-code' post-document-currency-code

    post-tax-point-date := 'document-currency-code' post-document-currency-code

    post-document-currency-code 
      := 'tax-currency-code' post-tax-currency-code
      or 'accounting-cost' post-accounting-cost
      or 'buyer-reference' post-buyer-reference
      or 'STAG-invoice-period' BG-invoice-period
      or 'EOF'

    post-tax-currency-code 
      := 'accounting-cost' post-accounting-cost
      or 'buyer-reference' post-buyer-reference
      or 'STAG-invoice-period' BG-invoice-period
      or 'EOF'

    post-accounting-cost
      := 'buyer-reference' post-buyer-reference
      or 'STAG-invoice-period' BG-invoice-period
      or 'EOF'

    post-buyer-reference 
      := 'STAG-invoice-period' BG-invoice-period
      or 'EOF'

    BG-invoice-period 
      := 'start-date' post-start-date
      or 'end-date' post-end-date
      or 'description-code' post-description-code
      or 'ETAG-invoice-period'

    post-start-date
      := end-date' post-end-date
      or 'description-code' post-description-code
      or 'ETAG-invoice-period'

    post-end-date
      := 'description-code' post-description-code
      or 'ETAG-invoice-period'

