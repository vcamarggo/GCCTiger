#ifndef TIGER_TOKEN_H
#define TIGER_TOKEN_H

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "input.h"

#include <string>
#include <tr1/memory>

namespace Tiger
{

// TIGER_TOKEN(name, description)
// TIGER_TOKEN_KEYWORD(name, identifier)
//
// Keep TIGER_TOKEN_KEYWORD sorted

#define TIGER_TOKEN_LIST                                                        \
  TIGER_TOKEN (FIRST_TOKEN, "<first-token-marker>")                             \
  TIGER_TOKEN (END_OF_FILE, "end of file")                                      \
  TIGER_TOKEN (ASSIGN, ":=")                                                    \
  TIGER_TOKEN (ASTERISK, "*")                                                   \
  TIGER_TOKEN (COLON, ":")                                                      \
  TIGER_TOKEN (COMMA, ",")                                                      \
  TIGER_TOKEN (DIFFERENT, "<>")                                                 \
  TIGER_TOKEN (EQUAL, "=")                                                      \
  TIGER_TOKEN (LEFT_PAREN, "(")                                                 \
  TIGER_TOKEN (MINUS, "-")                                                      \
  TIGER_TOKEN (PLUS, "+")                                                       \
  TIGER_TOKEN (RIGHT_PAREN, ")")                                                \
  TIGER_TOKEN (SEMICOLON, ";")                                                  \
  TIGER_TOKEN (SLASH, "/")                                                      \
  TIGER_TOKEN (GREATER, ">")                                                    \
  TIGER_TOKEN (GREATER_OR_EQUAL, ">=")                                          \
  TIGER_TOKEN (LOWER, "<")                                                      \
  TIGER_TOKEN (LOWER_OR_EQUAL, "<=")                                            \
  TIGER_TOKEN (IDENTIFIER, "identifier")                                        \
  TIGER_TOKEN (INTEGER_LITERAL, "integer literal")                              \
  TIGER_TOKEN (STRING_LITERAL, "string literal")                                \
  TIGER_TOKEN (REAL_LITERAL, "real literal")                                    \
  TIGER_TOKEN (LEFT_SQUARE, "[")                                                \
  TIGER_TOKEN (RIGHT_SQUARE, "]")                                               \
  TIGER_TOKEN (AND, "&")                                                        \
  TIGER_TOKEN (OR, "|")                                                         \
  TIGER_TOKEN (DOT, ".")                                                        \
  TIGER_TOKEN (LEFT_BRACKET, "{")                                               \
  TIGER_TOKEN (RIGHT_BRACKET, "}")                                              \
                                                                                \
  TIGER_TOKEN_KEYWORD (ARRAY, "array")                                          \
  TIGER_TOKEN_KEYWORD (BREAK, "break")                                          \
  TIGER_TOKEN_KEYWORD (DO, "do")                                                \
  TIGER_TOKEN_KEYWORD (ELSE, "else")                                            \
  TIGER_TOKEN_KEYWORD (END, "end")                                              \
  TIGER_TOKEN_KEYWORD (FLOAT, "real")                                           \
  TIGER_TOKEN_KEYWORD (FOR, "for")                                              \
  TIGER_TOKEN_KEYWORD (FUNC, "function")                                    \
  TIGER_TOKEN_KEYWORD (IF, "if")                                                \
  TIGER_TOKEN_KEYWORD (IN, "in")                                                \
  TIGER_TOKEN_KEYWORD (INT, "int")                                              \
  TIGER_TOKEN_KEYWORD (LET, "let")                                              \
  TIGER_TOKEN_KEYWORD (NIL, "nil")                                              \
  TIGER_TOKEN_KEYWORD (OF, "of")                                                \
  TIGER_TOKEN_KEYWORD (STRING, "string")                                        \
  TIGER_TOKEN_KEYWORD (THEN, "then")                                            \
  TIGER_TOKEN_KEYWORD (TO, "to")                                                \
  TIGER_TOKEN_KEYWORD (TYPE, "type")                                            \
  TIGER_TOKEN_KEYWORD (VAR, "var")                                              \
  TIGER_TOKEN_KEYWORD (WHILE, "while")                                          \
  TIGER_TOKEN_KEYWORD (WRITE, "print")                                          \
                                                                                \
  TIGER_TOKEN (LAST_TOKEN, "<last-token-marker>")

enum /* class */ TokenId
{
#define TIGER_TOKEN(name, _) name,
#define TIGER_TOKEN_KEYWORD(x, y) TIGER_TOKEN (x, y)
  TIGER_TOKEN_LIST
#undef TIGER_TOKEN_KEYWORD
#undef TIGER_TOKEN
};

const char *get_token_description (TokenId tid);
const char *token_id_to_str (TokenId tid);

struct Token;
typedef std::tr1::shared_ptr<Token> TokenPtr;
typedef std::tr1::shared_ptr<const Token> const_TokenPtr;

struct Token
{
private:
  TokenId token_id;
  location_t locus;
  std::string *str;

  Token (TokenId token_id_, location_t locus_)
    : token_id (token_id_), locus (locus_), str (0)
  {
  }
  Token (TokenId token_id_, location_t locus_, const std::string& str_)
    : token_id (token_id_), locus (locus_), str (new std::string (str_))
  {
  }

  // No default initializer
  Token ();
  // Do not copy/assign tokens
  Token (const Token &);
  Token &operator=(const Token &);

public:
  ~Token () { delete str; }

  static TokenPtr
  make (TokenId token_id, location_t locus)
  {
    return TokenPtr(new Token (token_id, locus));
  }

  static TokenPtr
  make_identifier (location_t locus, const std::string& str)
  {
    return TokenPtr(new Token (IDENTIFIER, locus, str));
  }

  static TokenPtr
  make_integer (location_t locus, const std::string& str)
  {
    return TokenPtr(new Token (INTEGER_LITERAL, locus, str));
  }

  static TokenPtr
  make_real(location_t locus, const std::string& str)
  {
    return TokenPtr(new Token (REAL_LITERAL, locus, str));
  }

  static TokenPtr
  make_string (location_t locus, const std::string& str)
  {
    return TokenPtr(new Token (STRING_LITERAL, locus, str));
  }

  TokenId
  get_id () const
  {
    return token_id;
  }

  location_t
  get_locus () const
  {
    return locus;
  }

  const std::string &
  get_str () const
  {
    gcc_assert (str != NULL);
    return *str;
  }

  // diagnostics
  const char *
  get_token_description () const
  {
    return Tiger::get_token_description (token_id);
  }

  // debugging
  const char *
  token_id_to_str () const
  {
    return Tiger::token_id_to_str (token_id);
  }
};

}

#endif // TIGER_TOKEN_H
