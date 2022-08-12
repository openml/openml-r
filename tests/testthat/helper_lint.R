# 
# # check if lintr version is sufficient
# # if `error.if.not` is TRUE an error is thrown with a meaningful message.
# isLintrVersionOk = function(error.if.not = FALSE) {
#   lintr.ver = try(packageVersion("lintr"), silent = TRUE)
#   lintr.required = "1.0.0.9001"
#   if (inherits(lintr.ver, "try-error")) {
#     msg = "lintr is not installed."
#   } else {
#     if (package_version(lintr.ver) >= package_version(lintr.required)) {
#       return(TRUE)
#     }
#     msg = sprintf("lintr is version %s, but version %s is required.", lintr.ver, lintr.required)
#   }
#   if (error.if.not) {
#     stopf(paste("%s\nInstalling the github version of lintr will probably solve this issue. For that, please run",
#       "> devtools::install_github(\"jimhester/lintr\")", sep = "\n"), msg)
#   }
#   return(FALSE)
# }
# 
# if (isLintrVersionOk() && require("lintr", quietly = TRUE) && require("rex", quietly = TRUE)) {
# 
# 
#   # The following functions are adaptions of the corresponding functions in the `lintr` packages
#   # The lintr package, and the original versions of these functions, can be found at https://github.com/jimhester/lintr
#   # Copyright notice of original functions:
#   # Copyright (c) 2014-2016, James Hester
#   #
#   # Permission is hereby granted, free of charge, to any person obtaining
#   # a copy of this software and associated documentation files (the
#   # "Software"), to deal in the Software without restriction, including
#   # without limitation the rights to use, copy, modify, merge, publish,
#   # distribute, sublicense, and/or sell copies of the Software, and to
#   # permit persons to whom the Software is furnished to do so, subject to
#   # the following conditions:
#   #
#   # The above copyright notice and this permission notice shall be
#   # included in all copies or substantial portions of the Software.
#   #
#   # THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#   # EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#   # MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
#   # NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
#   # LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
#   # OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
#   # WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#   #
#   # End copyright notice.
#   # All modifications are licensed as the rest of mlr.
# 
#   # linters that differ from the default linters
#   # this is necessary because mlr's style is weird.
# 
#   # prohibit <-
#   left.assign.linter = function(source_file) {
#     lapply(lintr:::ids_with_token(source_file, "LEFT_ASSIGN"), function(id) {
#       parsed = lintr:::with_id(source_file, id)
#       Lint(filename = source_file$filename, line_number = parsed$line1,
#         column_number = parsed$col1, type = "style", message = "Use =, not <-, for assignment.",
#         line = source_file$lines[as.character(parsed$line1)],
#         linter = "assignment_linter")
#     })
#   }
# 
#   # prohibit ->
#   right.assign.linter = function(source_file) {
#     lapply(lintr:::ids_with_token(source_file, "RIGHT_ASSIGN"), function(id) {
#       parsed = lintr:::with_id(source_file, id)
#       Lint(filename = source_file$filename, line_number = parsed$line1,
#         column_number = parsed$col1, type = "style", message = "Use =, not ->, for assignment.",
#         line = source_file$lines[as.character(parsed$line1)],
#         linter = "assignment_linter")
#     })
#   }
# 
#   `%!=%` = function (x, y) 
#   {
#     !identical(x, y)
#   }
#   `%==%` = function (x, y) 
#   {
#     identical(x, y)
#   }
# 
#   spaces.left.parentheses.linter = function(source_file) {
#     lapply(lintr:::ids_with_token(source_file, "'('"), function(id) {
#       parsed = source_file$parsed_content[id, ]
#       terminal.tokens.before = source_file$parsed_content$token[source_file$parsed_content$line1 ==
#           parsed$line1 & source_file$parsed_content$col1 <
#           parsed$col1 & source_file$parsed_content$terminal]
#       last.type = tail(terminal.tokens.before, n = 1)
#       is.function = length(last.type) %!=% 0L && (last.type %in%
#           c("SYMBOL_FUNCTION_CALL", "FUNCTION", "'}'", "')'",
#             "']'"))
#       if (!is.function) {
#         line = source_file$lines[as.character(parsed$line1)]
#         before.operator = substr(line, parsed$col1 - 1L,
#           parsed$col1 - 1L)
#         non.space.before = re_matches(before.operator, rex(non_space))
#         not.exception = !(before.operator %in% c("!", ":",
#           "[", "("))
#         if (non.space.before && not.exception) {
#           Lint(filename = source_file$filename, line_number = parsed$line1,
#             column_number = parsed$col1, type = "style",
#             message = "Place a space before left parenthesis, except in a function call.",
#             line = line, linter = "spaces.left.parentheses.linter")
#         }
#       }
#     })
#   }
# 
#   function.left.parentheses.linter = function(source_file) {
#     lapply(lintr:::ids_with_token(source_file, "'('"),
#       function(id) {
# 
#         parsed = source_file$parsed_content[id, ]
#         ttb = which(source_file$parsed_content$line1 == parsed$line1 &
#             source_file$parsed_content$col1 < parsed$col1 &
#             source_file$parsed_content$terminal)
#         ttb = tail(ttb, n = 1)
#         last.type = source_file$parsed_content$token[ttb]
# 
#         is.function = length(last.type) %!=% 0L &&
#           (last.type %in% c("SYMBOL_FUNCTION_CALL", "FUNCTION", "'}'", "')'", "']'"))
#         # check whether this is a lambda expression; we want to allow e.g. function(x) (x - 1)^2
#         if (is.function && last.type == "')'") {
#           # parenvec: 1 for every '(', -1 for every ')', 0 otherwise
#           parenvec = c(1, -1, 0)[match(source_file$parsed_content$token, c("'('", "')'"), 3)]
#           parenlevel = cumsum(parenvec)
#           parenlevelcut = parenlevel[seq_len(ttb - 1)]
#           opening.paren.pos = max(which(parenlevelcut == parenlevel[ttb])) + 1
#           opparsed = source_file$parsed_content[opening.paren.pos, ]
# 
#           opttb = which(source_file$parsed_content$line1 == opparsed$line1 &
#               source_file$parsed_content$col1 < opparsed$col1 &
#               source_file$parsed_content$terminal)
#           opttb = tail(opttb, n = 1)
#           before.op.type = source_file$parsed_content$token[opttb]
#           if (length(before.op.type) %!=% 0L && before.op.type == "FUNCTION") {
#             is.function = FALSE
#           }
#         }
#         if (is.function) {
# 
#           line = source_file$lines[as.character(parsed$line1)]
# 
#           before.operator = substr(line, parsed$col1 - 1L, parsed$col1 - 1L)
# 
#           space.before = re_matches(before.operator, rex(space))
# 
#           if (space.before) {
#             Lint(
#               filename = source_file$filename,
#               line_number = parsed$line1,
#               column_number = parsed$col1,
#               type = "style",
#               message = "Remove spaces before the left parenthesis in a function call.",
#               line = line,
#               linter = "function_left_parentheses"
#             )
#           }
#         }
# 
#       })
#   }
# 
#   infix.spaces.linter = function(source_file) {
#     lapply(lintr:::ids_with_token(source_file, lintr:::infix_tokens, fun = `%in%`),
#       function(id) {
#         parsed = lintr:::with_id(source_file, id)
#         line = source_file$lines[as.character(parsed$line1)]
#         if (substr(line, parsed$col1, parsed$col2) == "^") {
#           return(NULL)
#         }
#         around.operator = substr(line, parsed$col1 - 1L,
#           parsed$col2 + 1L)
#         non.space.before = re_matches(around.operator, rex(start,
#           non_space))
#         newline.after = unname(nchar(line)) %==% parsed$col2
#         non.space.after = re_matches(around.operator, rex(non_space,
#           end))
#         if (non.space.before || (!newline.after && non.space.after)) {
#           is.infix = length(lintr:::siblings(source_file$parsed_content,
#             parsed$id, 1)) > 1L
#           start = end = parsed$col1
#           if (is.infix) {
#             if (non.space.before) {
#               start = parsed$col1 - 1L
#             }
#             if (non.space.after) {
#               end = parsed$col2 + 1L
#             }
#             Lint(filename = source_file$filename, line_number = parsed$line1,
#               column_number = parsed$col1, type = "style",
#               message = "Put spaces around all infix operators (except exponentiation).",
#               line = line, ranges = list(c(start, end)),
#               linter = "infix.spaces.linter")
#           }
#         }
#       })
#   }
# 
# 
#   loweralnum = rex::rex(one_of(lower, digit))
#   upperalnum = rex::rex(one_of(upper, digit))
# 
#   style.regexes = list(
#     "UpperCamelCase" = rex::rex(start, upper, zero_or_more(alnum), end),
#     "lowerCamelCase" = rex::rex(start, lower, zero_or_more(alnum), end),
#     "snake_case"     = rex::rex(start, one_or_more(loweralnum), zero_or_more("_", one_or_more(loweralnum)), end),
#     "dotted.case"    = rex::rex(start, one_or_more(loweralnum), zero_or_more(dot, one_or_more(loweralnum)), end),
#     "alllowercase"   = rex::rex(start, one_or_more(loweralnum), end),
#     "ALLUPPERCASE"   = rex::rex(start, one_or_more(upperalnum), end),
#     "functionCamel.case" = rex::rex(start, lower, zero_or_more(alnum), zero_or_more(dot, one_or_more(alnum)), end)
#   )
# 
#   # incorporate our own camelCase.withDots style.
#   matchesStyles = function(name, styles=names(style.regexes)) {
#     invalids = paste(styles[!styles %in% names(style.regexes)], collapse = ", ")
#     if (nzchar(invalids)) {
#       valids = paste(names(style.regexes), collapse = ", ")
#       stop(sprintf("Invalid style(s) requested: %s\nValid styles are: %s\n", invalids, valids))
#     }
#     name = re_substitutes(name, rex(start, one_or_more(dot)), "")  # remove leading dots
#     vapply(
#       style.regexes[styles],
#       re_matches,
#       logical(1L),
#       data = name
#     )
#   }
# 
#   object.naming.linter = lintr:::make_object_linter(function(source_file, token) {
#     sp = source_file$parsed_content
#     if (tail(c("", sp$token[sp$terminal & sp$id < token$id]), n = 1) == "'$'") {
#       # ignore list member names
#       return(NULL)
#     }
#     sp = head(sp[sp$terminal & sp$id > token$id, ], n = 2)
#     if (!sp$token[1] %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) {
#       # ignore if not an assignment.
#       # we check for LEFT_ASSIGN and EQ_ASSIGN since here we are LEFT_ASSIGN tolerant
#       return(NULL)
#     }
#     style = ifelse(sp$token[2] == "FUNCTION", "functionCamel.case", "dotted.case")
#     name = lintr:::unquote(token[["text"]])
#     if (nchar(name) <= 1) {
#       # allow single uppercase letter
#       return(NULL)
#     }
#     if (!matchesStyles(name, style)) {
#       lintr:::object_lint(source_file, token, sprintf("Variable or function name should be %s.",
#         style), "object_name_linter")
#     }
#   })
# 
#   T_and_F_symbol_linter = function(source_file) {
#     lapply(
#       lintr:::ids_with_token(source_file, "SYMBOL"),
#       function(id) {
#         token = lintr:::with_id(source_file, id)
#         symbol = re_matches(token[["text"]], rex(start, capture(or("T", "F")), end))[1L, 1L]
#         if (!is.na(symbol)) {
#           replacement = switch(symbol, "T" = "TRUE", "F" = "FALSE")
#           line_num = token[["line2"]]
#           start_col_num = token[["col1"]]
#           end_col_num = token[["col2"]]
#           Lint(
#             filename = source_file[["filename"]],
#             line_number = line_num,
#             column_number = end_col_num + 1L,
#             type = "style",
#             message = sprintf("Use %s instead of the symbol %s.", replacement, symbol),
#             line = source_file[["lines"]][[as.character(line_num)]],
#             ranges = list(c(start_col_num, end_col_num)),
#             linter = "T_and_F_symbol_linter"
#           )
#         }
#       }
#     )
#   }
# 
#   unneeded_concatenation_linter = function(source_file) {
#     get_num_concat_args = function(token_num, tokens) {
#       # Detect the sequence "c" + "(" + optional constant + ")" and return a number:
#       #   -1 if not a concatenation call
#       #    0 if a concatenation without arguments
#       #    1 if a concatenation with a single constant argument
#       #    2 if a concatenation in other cases
#       open_paren_num = token_num + 1L
#       if (tokens[token_num, "text"] == "c" &&
#           tokens[open_paren_num, "token"] == "'('") {
#         token_args = get_tokens_in_parentheses(open_paren_num, tokens)
#         num_token_args = nrow(token_args)
#         if (!num_token_args) {
#           0L
#         } else if (num_token_args == 1L) {
#           if (token_args[1L, "token"] %in% c("STR_CONST", "NUM_CONST", "NULL_CONST")) {
#             1L
#           } else {
#             2L
#           }
#         } else {
#           2L
#         }
#       } else {
#         -1L
#       }
#     }
#     get_tokens_in_parentheses = function(open_paren_line_num, tokens) {
#       # Return the tokens enclosed by the opening parenthesis/bracket at the given line, or NA.
#       open_paren_token = tokens[open_paren_line_num, ]
#       open_paren_text = open_paren_token[["text"]]
#       close_paren_token = tail(get_sibling_tokens(open_paren_token, tokens), 1L)
#       close_paren_text = close_paren_token[["text"]]
#       close_paren_line_num = which(rownames(tokens) == rownames(close_paren_token))
#       if ((open_paren_text == "("  && close_paren_text ==  ")") ||
#           (open_paren_text == "{"  && close_paren_text ==  "}") ||
#           (open_paren_text == "["  && close_paren_text ==  "]") ||
#           (open_paren_text == "[[" && close_paren_text == "]]")) {
#         range = if (open_paren_line_num + 1L == close_paren_line_num) {
#           integer()
#         } else {
#           (open_paren_line_num + 1L):(close_paren_line_num - 1L)
#         }
#         tokens[range, ]
#       } else {
#         NA
#       }
#     }
#     get_sibling_tokens = function(child, tokens) {
#       # Get all siblings of the given child token (i.e. that have the same parent id)
#       tokens[tokens[, "parent"] == child[["parent"]], ]
#     }
#     filter_out_token_type = function(tokens, type) {
#       tokens[tokens[["token"]] != type, ]
#     }
#     tokens = source_file[["parsed_content"]] =
#       filter_out_token_type(source_file[["parsed_content"]], "expr")
# 
#     msg_empty = "Unneded concatenation without arguments. Replace the \"c\" call by NULL or vector()."
#     msg_const = "Unneded concatenation of a constant. Remove the \"c\" call."
#     lapply(
#       lintr:::ids_with_token(source_file, "SYMBOL_FUNCTION_CALL"),
#       function(token_num) {
#         num_args = get_num_concat_args(token_num, tokens)
#         if (num_args == 0L || num_args == 1L) {
#           token = lintr:::with_id(source_file, token_num)
#           start_col_num = token[["col1"]]
#           end_col_num = token[["col2"]]
#           line_num = token[["line1"]]
#           line = source_file[["lines"]][[as.character(line_num)]]
#           Lint(
#             filename = source_file[["filename"]],
#             line_number = line_num,
#             column_number = start_col_num,
#             type = "warn",
#             message = if (num_args) {msg_const} else {msg_empty},
#             line = line,
#             linter = "unneeded_concatenation_linter",
#             ranges = list(c(start_col_num, end_col_num))
#           )
#         }
#       }
#     )
#   }
# 
#   # note that this must be a *named* list (bug in lintr)
#   linters = list(
#     commas = lintr::commas_linter,
#     #  open.curly = open_curly_linter(),
#     #  closed.curly = closed_curly_linter(),
#     spaces.left.parentheses = spaces.left.parentheses.linter,
#     function.left.parentheses = function.left.parentheses.linter,
#     single.quotes = lintr::single_quotes_linter,
#     left.assign = left.assign.linter,
#     right.assign = right.assign.linter,
#     no.tab = lintr::no_tab_linter,
# 
#     T.and.F.symbol = T_and_F_symbol_linter,
#     # semicolon.terminator = lintr::semicolon_terminator_linter,
#     # seq = lintr::seq_linter,
#     unneeded.concatenation = unneeded_concatenation_linter,
# 
#     trailing.whitespace = lintr::trailing_whitespace_linter,
#     #todo.comment = lintr::todo_comment_linter(todo = "todo"), # is case-insensitive
#     spaces.inside = lintr::spaces_inside_linter,
#     infix.spaces = infix.spaces.linter#,
#     #object.naming = object.naming.linter
#     )
# } else {
#   # everything that uses `linters` should check `isLintrVersionOk` first, so the
#   # following should never be used. Make sure that it is an error if it IS used.
#   linters = list(error = "lintr package could not be loaded")
# }
