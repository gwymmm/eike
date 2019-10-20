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

# Syntax Recognition

    syntax-recon-1 := '0xEF' bom 
               or '<' pbc
               or ' ' post-prolog

    bom := '0xBB' bom-2

    bom-2 := '0xBF' post-bom

    post-bom := '<' pbc
             or ' ' post-prolog

    pbc 
      := '?' prolog
      or '!' comm
      or (alpha) begin-elem

    begin-elem-or-comm := '!' comm
                       or (alpha) begin-elem

    comm := (char, not '>') comm
         or '>' post-prolog

    prolog := (not '?') prolog
           or '?' prolog-end

    prolog-end := '>' post-prolog

    post-prolog := ' ' post-prolog
               or '<' begin-elem-or-comm

    begin-elem := (alpha) begin-elem
               or ' ' skip-ns
               or ':' begin-elem

    skip-ns := (char, not '>') skip-ns
            := '>' --<<<< return ubl-invoice or ubl-xxx or cii-invoice