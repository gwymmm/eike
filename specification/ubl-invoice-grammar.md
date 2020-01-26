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

    ubl-invoice := 'customization-id' post-customization-id --> bt-24

    post-customization-id := 'profile-id' post-profile-id --> bt-23
                          or 'id' post-id 

    post-profile-id := 'id' post-id     --> bt-1

    post-id := 'issue-date' post-issue-date     --> bt-2

    post-issue-date := 'due-date' post-due-date     --> bt-9
                    or 'invoice-type-code' post-invoice-type-code   --> bt-3

    post-due-date := 'invoice-type-code' post-invoice-type-code     --> bt-3

    post-invoice-type-code := 'note' post-note  --> bt-21 / bt-22
          or 'tax-point-date' post-tax-point-date  --> bt-7  
          or 'document-currency-code' post-document-currency-code  --> bt-5

    post-note := 'note' post-note-2     --> bt-22
          or 'tax-point-date' post-tax-point-date  --> bt-7 
          or 'document-currency-code' post-document-currency-code  --> bt-5

    post-note-2 := 'tax-point-date' post-tax-point-date --> bt-7
            or 'document-currency-code' post-document-currency-code --> bt-5

    post-tax-point-date := 'document-currency-code' post-document-currency-code
      --> bt-5

    post-document-currency-code 
      := 'tax-currency-code' post-tax-currency-code  --> bt-6
      or 'accounting-cost' post-accounting-cost  --> bt-19
      or 'buyer-reference' post-buyer-reference  --> bt-10
      or 'STAG-invoice-period' BG-invoice-period  --> bg-14
      or 'EOF'

    post-tax-currency-code 
      := 'accounting-cost' post-accounting-cost  --> bt-19
      or 'buyer-reference' post-buyer-reference  --> bt-10
      or 'STAG-invoice-period' BG-invoice-period  --> bg-14
      or 'EOF'

    post-accounting-cost
      := 'buyer-reference' post-buyer-reference  --> bt-10
      or 'STAG-invoice-period' BG-invoice-period  --> bg-14
      or 'EOF'

    post-buyer-reference 
      := 'STAG-invoice-period' BG-invoice-period  --> bg-14
      or 'EOF'

    BG-invoice-period 
      := 'start-date' post-start-date  --> bt-73
      or 'end-date' post-end-date  --> bt-74
      or 'description-code' post-description-code  --> bt-8
      or 'ETAG-invoice-period' 

    post-start-date
      := 'end-date' post-end-date  --> bt-74
      or 'description-code' post-description-code  --> bt-8
      or 'ETAG-invoice-period'

    post-end-date
      := 'description-code' post-description-code  --> bt-8
      or 'ETAG-invoice-period'

    post-description-code
      := 'ETAG-invoice-period'

