#include "tiger/tiger-lexer.h"

#include "config.h"
#include "coretypes.h"
#include "input.h"
#include "diagnostic.h"
#include "safe-ctype.h"

#include <cstdlib>
#include <algorithm>
#include <iostream>
using namespace std;
//remover 11 e 12


namespace Tiger
{

Lexer::Lexer (const char *filename, FILE *input_)
  : input (input_), current_line (1), current_column (1), line_map (0),
    input_source (input), input_queue (input_source), token_source (this),
    token_queue (token_source)
{
  line_map = ::linemap_add (::line_table, ::LC_ENTER,
			    /* sysp */ 0, filename,
			    /* current_line */ 1);
}

Lexer::~Lexer ()
{
  ::linemap_add (::line_table, ::LC_LEAVE,
		 /* sysp */ 0,
		 /* filename */ NULL,
		 /* to_line */ 0);
}

location_t
Lexer::get_current_location ()
{
  return ::linemap_position_for_column (::line_table, current_column);
}

int
Lexer::peek_input (int n)
{
  return input_queue.peek (n);
}

int
Lexer::peek_input ()
{
  return peek_input (0);
}

void
Lexer::skip_input (int n)
{
  input_queue.skip (n);
}

void
Lexer::skip_input ()
{
  skip_input (0);
}

namespace
{

const std::string keyword_index[] = {
#define TIGER_TOKEN(x, y)
#define TIGER_TOKEN_KEYWORD(name, keyword) keyword,
  TIGER_TOKEN_LIST
#undef TIGER_TOKEN_KEYWORD
#undef TIGER_TOKEN
};

TokenId keyword_keys[] = {
#define TIGER_TOKEN(x, y)
#define TIGER_TOKEN_KEYWORD(name, keyword) name,
  TIGER_TOKEN_LIST
#undef TIGER_TOKEN_KEYWORD
#undef TIGER_TOKEN
};

const int num_keywords = sizeof (keyword_index) / sizeof (*keyword_index);
}

TokenId
Lexer::classify_keyword (const std::string &str)
{
  const std::string *last = keyword_index + num_keywords;
  const std::string *idx = std::lower_bound (keyword_index, last, str);
	//cout << "resultado " << idx - keyword_index; 	
	//num_key - idx 8   if ta com problema
  if (idx == last || str != *idx){
    return IDENTIFIER;
}  else
    {
      return keyword_keys[idx - keyword_index];
    }
}

TokenPtr
Lexer::build_token ()
{
  for (;;)
    {
      location_t loc = get_current_location ();
      int current_char = peek_input ();
      skip_input ();
	  int comment = 0;

      if (current_char == EOF)
	{
	  return Token::make (END_OF_FILE, loc);
	}

      switch (current_char)
	{
	// **************
	// * Whitespace *
	// **************
	case '\n':
	  current_line++;
	  current_column = 1;
	  linemap_line_start (::line_table, current_line, max_column_hint);
	  return Token::make (END_OF_LINE, loc);
	case ' ':
	  current_column++;
	  continue;
	case '\t':
	  // Width of a tab is not well defined, let's assume 8 for now
	  current_column += 4;
	  continue;

	// ***************
	// * Punctuation *
	// ***************
	case '/':
          if (peek_input () == '*') {
	      comment++;
              skip_input();
              current_column++;
            while(peek_input() != EOF) {
		if(peek_input() == '/'){
			skip_input();
			current_column++;
			if(peek_input() == '*'){
				skip_input();
				current_column++;
				comment++;
			}
		}else if(peek_input() == '*'){
		    skip_input();
		    current_column++;
		    if(peek_input() == '/'){
			current_column++;
			skip_input();
			comment--;
			if(comment==0){
				current_column++;
				break;
			}
    		    }
                }
                current_column++;
                skip_input();
	     }
	  }
          else{
	      current_column++;
	      return Token::make (SLASH, loc);
      	}
	continue;
	case ':':
	  if (peek_input () == '=')
	    {
	      skip_input ();
	      current_column += 2;

	      return Token::make (ASSIGN, loc);
	    }
	  else
	    {
	      current_column++;
	      return Token::make (COLON, loc);
	    }
	  break;
	case '*':
	  current_column++;
	  return Token::make (ASTERISK, loc);
	case ',':
	  current_column++;
	  return Token::make (COMMA, loc);
	case '=':
	  current_column++;
	  return Token::make (EQUAL, loc);
	case '{':
	  current_column++;
	  return Token::make (LEFT_BRACKET, loc);
	case '}':
	  current_column++;
	  return Token::make (RIGHT_BRACKET, loc);
	case '(':
	  current_column++;
	  return Token::make (LEFT_PAREN, loc);
	case '-':
	  current_column++;
	  return Token::make (MINUS, loc);
	case '+':
	  current_column++;
	  return Token::make (PLUS, loc);
	case ')':
	  current_column++;
	  return Token::make (RIGHT_PAREN, loc);
	case '.':
	    if (!ISDIGIT(peek_input ())){
		current_column++;
		return Token::make (DOT, loc);
	    }
	case ';':
	  current_column++;
	  return Token::make (SEMICOLON, loc);
	case '<':
	  if (peek_input () == '=')
	    {
	      skip_input ();
	      current_column += 2;

	      return Token::make (LOWER_OR_EQUAL, loc);
	    }
	  else if (peek_input () == '>')
	    {
	      skip_input ();
	      current_column += 2;

	      return Token::make (DIFFERENT, loc);
	    }
	  else
	    {
	      current_column++;
	      return Token::make (LOWER, loc);
	    }
	  break;
	case '>':
	  if (peek_input () == '=')
	    {
	      skip_input ();
	      current_column += 2;

	      return Token::make (GREATER_OR_EQUAL, loc);
	    }
	  else
	    {
	      current_column++;
	      return Token::make (GREATER, loc);
	    }
	  break;
	case '[':
	  current_column++;
	  return Token::make (LEFT_SQUARE, loc);
	case ']':
	  current_column++;
	  return Token::make (RIGHT_SQUARE, loc);
	case '&':
	  current_column++;
	  return Token::make (AND, loc);
	case '|':
	  current_column++;
	  return Token::make (OR, loc);
	
	}

      // ***************************
      // * Identifiers or keywords *
      // ***************************
      if (ISALPHA (current_char))
	{
	  std::string str;
	  str.reserve (16); // some sensible default
	  str += current_char;

	  int length = 1;
	  current_char = peek_input ();
	  while (ISALPHA (current_char) || ISDIGIT (current_char)
		 || current_char == '_')
	    {
	      length++;

	      str += current_char;
	      skip_input ();
	      current_char = peek_input ();
	    }

	  current_column += length;

	  TokenId keyword = classify_keyword (str);
	  if (keyword == IDENTIFIER)
	    {
	      return Token::make_identifier (loc, str);
	    }
	  else
	    {
	      return Token::make (keyword, loc);
	    }
	}

      // ****************************
      // * Integer or real literals *
      // ****************************
      if (ISDIGIT (current_char)  || current_char == '.')
	{
	  std::string str;
	  str.reserve (16);
	  str += current_char;

	  bool is_real = (current_char == '.');

	  int length = 1;
	  current_char = peek_input ();
	  while (ISDIGIT (current_char) || (!is_real && current_char == '.'))
	    {
	      length++;

	      is_real = is_real || (current_char == '.');

	      str += current_char;
	      skip_input ();
	      current_char = peek_input ();
	    }

	  current_column += length;

	  if (is_real)
	    {
	      return Token::make_real (loc, str);
	    }
	  else
	    {
	      return Token::make_integer (loc, str);
	    }
	}

      // *******************
      // * String literals *
      // *******************
      if (current_char == '"')
	{
	  std::string str;
	  str.reserve (16); // some sensible default

	  int length = 1;
	  current_char = peek_input ();
	  while (current_char != '\n' && current_char != '"')
	    {
	      length++;

	      str += current_char;
	      skip_input ();
	      current_char = peek_input ();
	    }

	  current_column += length;

	  if (current_char == '\n')
	    {
	      error_at (get_current_location (), "unended string literal");
	    }
	  else if (current_char == '"')
	    {
	      skip_input ();
	    }
	  else
	    {
	      gcc_unreachable ();
	    }

	  return Token::make_string (loc, str);
	}

      // Martians
      error_at (loc, "unexpected character '%x'", current_char);
      current_column++;
    }
}

const_TokenPtr
Lexer::peek_token (int n)
{
  return token_queue.peek (n);
}

const_TokenPtr
Lexer::peek_token ()
{
  return peek_token (0);
}

void
Lexer::skip_token (int n)
{
  token_queue.skip (n);
}

void
Lexer::skip_token ()
{
  skip_token (0);
}
}
