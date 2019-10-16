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


-- read up to 2 attributes
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

-- the '>' of the STag was already seen
-- read content and then the ETag

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