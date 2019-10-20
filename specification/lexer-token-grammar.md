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

# Lexer (get_token)

    next-element 
      := EOF --<<<< Token: EOF
      or ' ' next-element
      or '<' start-tag-or-comment-or-end-tag


    start-tag-or-comment-or-end-tag 
      := '!' comment
      or (alpha) start-tag
      or '/' end-tag


    comment
      := '>' next-element
      or (char) comment


Read up to 2 attributes

    start-tag
      := (alpha) start-tag
      or ':' start-tag
      or ' ' skip-to-attribute-one-or-end
      or '>' --<<<< Token: STAG


    skip-to-attribute-one-or-end 
      := ' ' skip-to-attribute-one-or-end
      or '>' --<<<< Token: STAG
      or (alpha) attribute-one-name


    attribute-one-name
      := (alpha) attribute-one-name
      or '=' skip-to-attribute-one-value
      or ' ' skip-to-equal-sign-one


    skip-to-equal-sign-one
      := ' ' skip-to-equal-sign-one
      or '=' skip-to-attribute-one-value


    skip-to-attribute-one-value
      := ' ' skip-to-attribute-one-value
      or '"' attribute-one-value


    attribute-one-value
      := (alphanum) attribute-one-value
      or '"' skip-to-attribute-two-or-end


    skip-to-attribute-two-or-end
      := ' ' skip-to-attribute-two-or-end
      or (alpha) attribute-two-name
      or '>' --<<<< Token: STAG

    attribute-two-name
      := (alpha) attr-two-name
      or '=' skip-to-attribute-two-value
      or ' ' skip-to-equal-sign-two


    skip-to-equal-sign-two
      := ' ' skip-to-equal-sign-two
      or '=' skip-to-attribute-two-value


    skip-to-attribute-two-value
      := ' ' skip-to-attribute-two-value
      or '"' attribute-two-value


    attribute-two-value
      := (alphanum) attribute-two-value
      or '"' skip-whitespaces


    skip-whitespaces
      := ' ' skip-whitespaces
      or '>' --<<<< Token: STAG 


    end-tag
      := (alpha) end-tag
      or ':' end-tag
     or ' ' skip-whitespaces-2
      or '>' --<<<< Token: ETAG


    skip-whitespaces-2
      := ' ' skip-whitespaces-2
      or '>' --<<<< Token: ETAG 

# Lexer (get_tail)

The '>' of the STag was already seen, read content and then the ETag

    content 
      := (char) content
      or '<' end-tag


    end-tag
      := '/' end-tag-2


    end-tag-2
      := (alpha) end-tag-2
      or ':' end-tag-2
      or ' ' skip-whitespaces
      or '>' --<<<< 


    skip-whitespaces
      := ' ' skip-whitespaces
      or '>' --<<<<