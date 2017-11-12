#include <iostream>
#include <memory>

#include "tiger/tiger-parser.h"
#include "tiger/tiger-lexer.h"
#include "tiger/tiger-tree.h"
#include "tiger/tiger-symbol.h"
#include "tiger/tiger-symbol-mapping.h"
#include "tiger/tiger-scope.h"


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tree.h"
#include "tree-iterator.h"
#include "input.h"
#include "diagnostic.h"
#include "stringpool.h"
#include "cgraph.h"
#include "gimplify.h"
#include "gimple-expr.h"
#include "convert.h"
#include "print-tree.h"
#include "stor-layout.h"
#include "fold-const.h"
#include <iostream>

#define MAIN_PAREN_RULES 0
#define OTHER_PAREN_RULES 1

using namespace std;
namespace Tiger
{

struct Parser
{
private:
  void skip_after_end ();
  void skip_after_end_var ();
  void skip_after_colon ();
  void skip_next_declaration_in ();

  bool skip_token (TokenId);
  const_TokenPtr expect_token (TokenId);
  void unexpected_token (const_TokenPtr);

  /* Expression parsing*/
  int left_binding_power (const_TokenPtr tok);
  Tree null_denotation (const_TokenPtr tok);
  Tree left_denotation (const_TokenPtr tok, Tree left);

  Tree parse_exp (int right_binding_power);

  Tree coerce_binary_arithmetic (const_TokenPtr tok, Tree *left, Tree *right);
  bool check_logical_operands (const_TokenPtr tok, Tree left, Tree right);

  Tree get_puts_addr ();
  Tree get_printf_addr ();
  Tree get_getchar_addr ();
  Tree get_flush_addr ();
  Tree get_ord_addr ();
  Tree get_chr_addr ();
  Tree get_size_addr ();
  Tree get_substring_addr ();
  Tree get_strcmp_addr ();
  Tree get_concat_addr ();
  Tree get_not_addr ();
  Tree get_exit_addr ();

  Tree build_label_decl (const char *name, location_t loc);
  Tree build_let_exp (Tree let_exp);
  Tree build_if_exp (Tree bool_expr, Tree then_part, Tree else_part);
  Tree build_while_exp (Tree bool_expr, Tree while_body);
  Tree build_for_exp (SymbolPtr ind_var, Tree lower_bound, Tree upper_bound,
			    Tree for_body_exp_list);

  const char *print_type (Tree type);
  Tree retrieve_type (string type);

  TreeExpList &get_current_exp_list ();

  void enter_scope ();

  struct TreeSymbolMapping
  {
    Tree bind_expr;
    Tree block;
  };

  TreeSymbolMapping leave_scope ();

  SymbolPtr query_type (const std::string &name, location_t loc);
  SymbolPtr query_variable (const std::string &name, location_t loc);
  SymbolPtr query_integer_variable (const std::string &name, location_t loc);

  string parse_exp_seq (bool (Parser::*done) (), int level);
  void parse_descriptor_seq (bool (Parser::*done) ());

  bool done_end ();
  bool done_in ();
  bool done_right_paren ();
  bool done_end_or_else ();
  bool done_end_of_file ();

  typedef Tree (Parser::*BinaryHandler) (const_TokenPtr, Tree);
  BinaryHandler get_binary_handler (TokenId id);

#define BINARY_HANDLER_LIST                                                    \
  BINARY_HANDLER (plus, PLUS)                                                  \
  BINARY_HANDLER (minus, MINUS)                                                \
  BINARY_HANDLER (mult, ASTERISK)                                              \
  BINARY_HANDLER (div, SLASH)                                                  \
                                                                               \
  BINARY_HANDLER (equal, EQUAL)                                                \
  BINARY_HANDLER (different, DIFFERENT)                                        \
  BINARY_HANDLER (lower_than, LOWER)                                           \
  BINARY_HANDLER (lower_equal, LOWER_OR_EQUAL)                                 \
  BINARY_HANDLER (greater_than, GREATER)                                       \
  BINARY_HANDLER (greater_equal, GREATER_OR_EQUAL)                             \
                                                                               \
  BINARY_HANDLER (logical_and, AND)                                            \
  BINARY_HANDLER (logical_or, OR)                                              \
                                                                               \
  BINARY_HANDLER (array_ref, LEFT_SQUARE)                                      \
                                                                               \
  BINARY_HANDLER (field_ref, DOT)

#define BINARY_HANDLER(name, _)                                                \
  Tree binary_##name (const_TokenPtr tok, Tree left);
  BINARY_HANDLER_LIST
#undef BINARY_HANDLER

public:
  Parser (Lexer &lexer_) : lexer (lexer_), puts_fn (), printf_fn (), getchar_fn (), flush_fn (),
  ord_fn (), chr_fn (), size_fn (), substring_fn (), strcmp_fn (), not_fn (), concat_fn (), exit_fn()
  {
  }

  void parse_program ();


  Tree parse_variable_declaration ();
  Tree parse_type_declaration ();

  Tree parse_type ();
  /*Tree parse_record ();*/
  Tree parse_field_declaration (std::vector<std::string> &field_names);

  Tree parse_assignment_exp (Tree var);
  Tree parse_if_exp ();
  Tree parse_while_exp ();
  Tree parse_for_exp();
  Tree parse_function_exp();
  Tree parse_let_exp();

  Tree parse_function ();
  Tree parse_write_function ();
  Tree parse_read_function ();
  Tree parse_flush_function ();
  Tree parse_ord_function ();
  Tree parse_chr_function ();
  Tree parse_size_function ();
  Tree parse_substring_function ();
  Tree parse_strcmp_function ();
  Tree parse_not_function ();
  Tree parse_concat_function ();
  Tree parse_exit_function ();

  Tree parse_exp ();
  Tree parse_exp_naming_variable();
  Tree parse_boolean_exp ();
  Tree parse_integer_exp ();

private:
  Lexer &lexer;
  Scope scope;

  tree main_fndecl;

  Tree puts_fn;
  Tree printf_fn;
  Tree getchar_fn;
  Tree flush_fn;
  Tree ord_fn;
  Tree chr_fn;
  Tree size_fn;
  Tree substring_fn;
  Tree strcmp_fn;
  Tree not_fn;
  Tree concat_fn;
  Tree exit_fn;

  std::vector<TreeExpList> stack_exp_list;
  std::vector<TreeChain> stack_var_decl_chain;

  std::vector<BlockChain> stack_block_chain;
};


/* OK */
void
Parser::skip_after_colon()
{
  const_TokenPtr t = lexer.peek_token ();

  while (t->get_id () != Tiger::END_OF_FILE && t->get_id () != Tiger::COLON)
    {
      lexer.skip_token ();
      t = lexer.peek_token ();
    }

  if (t->get_id () == Tiger::COLON)
    lexer.skip_token ();
}

/* OK */
void
Parser::skip_next_declaration_in()
{
  const_TokenPtr t = lexer.peek_token ();

  while (t->get_id () != Tiger::VAR && t->get_id () != Tiger::TYPE && t->get_id () != Tiger::IN)
    {
      lexer.skip_token ();
      t = lexer.peek_token ();
    }

  if (t->get_id () != Tiger::IN)
    lexer.skip_token ();
}

void
Parser::skip_after_end_var()
{
  const_TokenPtr t = lexer.peek_token ();

  while (t->get_id () != Tiger::END_OF_FILE && t->get_id () != Tiger::END && t->get_id () != Tiger::VAR)
    {
      lexer.skip_token ();
      t = lexer.peek_token ();
    }

  if (t->get_id () == Tiger::VAR)
    lexer.skip_token ();
}

/* OK */
void
Parser::skip_after_end ()
{
  const_TokenPtr t = lexer.peek_token ();

  while (t->get_id () != Tiger::END_OF_FILE && t->get_id () != Tiger::END)
    {
      lexer.skip_token ();
      t = lexer.peek_token ();
    }

  if (t->get_id () == Tiger::END)
    lexer.skip_token ();
}

/* OK */
const_TokenPtr
Parser::expect_token(Tiger::TokenId token_id)
{
  const_TokenPtr t = lexer.peek_token ();
  if (t->get_id () == token_id)
    {
      lexer.skip_token ();
      return t;
    }
  else
    {
      error_at (t->get_locus (), "expecting %s but %s found\n",
		get_token_description (token_id), t->get_token_description ());
      return const_TokenPtr ();
    }
}

/* OK */
bool
Parser::skip_token(Tiger::TokenId token_id)
{
  return expect_token (token_id) != const_TokenPtr();
}

/* OK */
void
Parser::unexpected_token(const_TokenPtr t)
{
  ::error_at (t->get_locus (), "unexpected %s\n", t->get_token_description ());
}

void
Parser::parse_program()
{
  /* Built type of main "int (int, char**)"*/
  tree main_fndecl_type_param[] = {
    integer_type_node,					     /* int */
    build_pointer_type (build_pointer_type (char_type_node)) /* char** */
  };
  tree main_fndecl_type
    = build_function_type_array (integer_type_node, 2, main_fndecl_type_param);
  /* Create function declaration "int main(int, char**)"*/
  main_fndecl = build_fn_decl ("main", main_fndecl_type);

  /* Enter top level scope*/
  
  enter_scope ();
 
  /* program -> exp**/
  parse_exp_seq (&Parser::done_end_of_file, MAIN_PAREN_RULES);
  /* Append "return 0;"*/
  tree resdecl
    = build_decl (UNKNOWN_LOCATION, RESULT_DECL, NULL_TREE, integer_type_node);
  DECL_CONTEXT (resdecl) = main_fndecl;
  DECL_RESULT (main_fndecl) = resdecl;
  tree set_result
    = build2 (INIT_EXPR, void_type_node, DECL_RESULT (main_fndecl),
	      build_int_cst_type (integer_type_node, 0));
  tree return_exp = build1 (RETURN_EXPR, void_type_node, set_result);

  get_current_exp_list ().append (return_exp);
  /* Leave top level scope, get its binding exp and its main block*/
  TreeSymbolMapping main_tree_scope = leave_scope ();
  Tree main_block = main_tree_scope.block;

  /* Finish main function*/
  BLOCK_SUPERCONTEXT (main_block.get_tree ()) = main_fndecl;
  DECL_INITIAL (main_fndecl) = main_block.get_tree ();
  DECL_SAVED_TREE (main_fndecl) = main_tree_scope.bind_expr.get_tree ();

  DECL_EXTERNAL (main_fndecl) = 0;
  DECL_PRESERVE_P (main_fndecl) = 1;

  /* Convert from GENERIC to GIMPLE*/
  gimplify_function_tree (main_fndecl);

  /* Insert it into the graph*/
  cgraph_node::finalize_function (main_fndecl, true);

  main_fndecl = NULL_TREE;
}

/* OK */
bool
Parser::done_end_of_file()
{
  const_TokenPtr t = lexer.peek_token ();
  return (t->get_id () == Tiger::END_OF_FILE);
}

bool
Parser::done_end ()
{
  const_TokenPtr t = lexer.peek_token ();
  return (t->get_id () == Tiger::END || t->get_id () == Tiger::END_OF_FILE || t->get_id () == Tiger::RIGHT_PAREN );
}

bool
Parser::done_end_or_else()
{
  const_TokenPtr t = lexer.peek_token ();
  return (t->get_id () == Tiger::END || t->get_id () == Tiger::ELSE
	  || t->get_id () == Tiger::END_OF_FILE || t->get_id () == Tiger::RIGHT_PAREN );
}

bool
Parser::done_in()
{
  const_TokenPtr t = lexer.peek_token ();
  return (t->get_id () == Tiger::IN || t->get_id () == Tiger::END_OF_FILE || t->get_id () == Tiger::RIGHT_PAREN );
}

bool
Parser::done_right_paren()
{
  const_TokenPtr t = lexer.peek_token ();
  return (t->get_id () == Tiger::RIGHT_PAREN);
}

string
Parser::parse_exp_seq(bool (Parser::*done) (), int level)
{
  /* Parse exps until done and append to the current exp list;*/
  string lastTreeType;
  while (!(this->*done) ())
    {
      Tree exp = parse_exp ();
      lastTreeType = exp.get_exp_type ();
      get_current_exp_list ().append (exp);
      const_TokenPtr tok = lexer.peek_token ();
      if (tok->get_id () == Tiger::SEMICOLON){
          lexer.skip_token ();
      }else if(level == 0){ /* level 0 == main() */
          tok = lexer.peek_token ();
          if (tok->get_id () != Tiger::END_OF_FILE){
              error_at (tok->get_locus (), "expecting end of file but %s found\n", tok->get_token_description ());
          }
      }else break;
      
    }
 return lastTreeType;
}

void
Parser::parse_descriptor_seq(bool (Parser::*done) ())
{
  /* Parse exps until done and append to the current exp list; */
  const_TokenPtr t = lexer.peek_token ();
  while (!(this->*done) ())
    {
      Tree exp = parse_variable_declaration ();
      get_current_exp_list ().append (exp);
      if (exp.is_error ())
          break;
      t = lexer.peek_token ();
    }
}

void
Parser::enter_scope()
{
  scope.push_scope ();

  TreeExpList exp_list;
  stack_exp_list.push_back (exp_list);

  stack_var_decl_chain.push_back (TreeChain ());
  stack_block_chain.push_back (BlockChain ());
}

Parser::TreeSymbolMapping
Parser::leave_scope ()
{
  TreeExpList current_exp_list = get_current_exp_list ();
  stack_exp_list.pop_back ();

  TreeChain var_decl_chain = stack_var_decl_chain.back ();
  stack_var_decl_chain.pop_back ();

  BlockChain subblocks = stack_block_chain.back ();
  stack_block_chain.pop_back ();

  tree new_block
    = build_block (var_decl_chain.first.get_tree (),
		   subblocks.first.get_tree (),
		   /* supercontext */ NULL_TREE, /* chain */ NULL_TREE);

  /* Add the new block to the current chain of blocks (if any) */
  if (!stack_block_chain.empty ())
    {
      stack_block_chain.back ().append (new_block);
    }

  /* Set the subblocks to have the new block as their parent */
  for (tree it = subblocks.first.get_tree (); it != NULL_TREE;
       it = BLOCK_CHAIN (it))
    BLOCK_SUPERCONTEXT (it) = new_block;
        
  tree bind_expr
    = build3 (BIND_EXPR, void_type_node, var_decl_chain.first.get_tree (),
	      current_exp_list.get_tree (), new_block);

  TreeSymbolMapping tree_scope;
  tree_scope.bind_expr = bind_expr;
  tree_scope.block = new_block;

  scope.pop_scope();

  return tree_scope;
}

TreeExpList &
Parser::get_current_exp_list ()
{
  return stack_exp_list.back ();
}


Tree
Parser::parse_let_exp()
{
  /* variable_declaration -> "var" identifier ":" type ";" */
	

  enter_scope ();
  parse_descriptor_seq (&Parser::done_in);
	
    if (!skip_token (Tiger::IN)){
      return Tree::error ();
    }

      parse_exp_seq (&Parser::done_end, OTHER_PAREN_RULES);
      TreeSymbolMapping let_scope = leave_scope ();
      Tree let_exp = let_scope.bind_expr;
      let_exp.set_exp_type ("void");
      
      if (let_exp.is_error ())
          return Tree::error ();      

    if (!skip_token (Tiger::END)){
      return Tree::error ();
    }

   return build_let_exp (let_exp);
}


Tree
Parser::build_let_exp (Tree let_exp)
{
  TreeExpList exp_list;
  exp_list.append (let_exp);  

  Tree retTree = exp_list.get_tree ();
  retTree.set_exp_type("void");
  return retTree;
}


Tree
Parser::parse_variable_declaration()
{
  /* variable_declaration -> "var" identifier ":" type ";"*/
  if (!skip_token (Tiger::VAR))
    {
      /*skip after var*/
      skip_after_end_var ();
      return Tree::error ();
    }

  const_TokenPtr identifier = expect_token (Tiger::IDENTIFIER);
  if (identifier == NULL)
    {
      skip_after_end_var ();
      return Tree::error ();
    }

  Tree type_tree;
  const_TokenPtr tok = lexer.peek_token ();
  bool typeNulo = true;
  if (tok->get_id () == Tiger::COLON)
    {
        typeNulo = false;
        skip_token (Tiger::COLON);
        type_tree = parse_type ();
    
    if (type_tree.is_error ()) {
      skip_after_end_var ();
      return Tree::error ();
    }
   } 

   const_TokenPtr assign_tok = expect_token (Tiger::ASSIGN);
   if (assign_tok == NULL)
    {
      return Tree::error ();
    }

  Tree expr = parse_exp ();
    if (expr.is_error ()) {
      skip_after_end_var ();
    return Tree::error ();
    }

	//aqui tem que ser sobreescrito valor da variavel
  if (scope.get_current_mapping ().get (identifier->get_str ()))
    {
      error_at (identifier->get_locus (),
		"name '%s' already declared in this scope",
		identifier->get_str ().c_str ());
      skip_next_declaration_in ();
      return Tree::error ();
    }
  SymbolPtr sym (new Symbol (Tiger::VARIABLE, identifier->get_str ()));
  scope.get_current_mapping ().insert (sym);

  sym = query_variable (identifier->get_str (), identifier->get_locus ());

  
  Tree decl;
  if(typeNulo){
     
  decl = build_decl (identifier->get_locus (), VAR_DECL,
			  get_identifier (sym->get_name ().c_str ()),
			  retrieve_type(expr.get_exp_type ()).get_tree ());
  } else {
    decl = build_decl (identifier->get_locus (), VAR_DECL,
			  get_identifier (sym->get_name ().c_str ()),
			  type_tree.get_tree ());
  }
  
  DECL_CONTEXT (decl.get_tree()) = main_fndecl;

  gcc_assert (!stack_var_decl_chain.empty ());
  stack_var_decl_chain.back ().append (decl);

  sym->set_tree_decl (decl);

  Tree retTree
    = build_tree (MODIFY_EXPR, assign_tok->get_locus (), void_type_node, decl, expr);

  return retTree;
}

namespace
{

bool
is_string_type(Tree type)
{
  gcc_assert (TYPE_P (type.get_tree ()));
  return type.get_tree_code () == POINTER_TYPE
	 && TYPE_MAIN_VARIANT (TREE_TYPE (type.get_tree ())) == char_type_node;
}

bool
is_array_type (Tree type)
{
  gcc_assert (TYPE_P (type.get_tree ()));
  return type.get_tree_code () == ARRAY_TYPE;
}

}

bool
is_record_type(Tree type)
{
  gcc_assert (TYPE_P (type.get_tree ()));
  return type.get_tree_code () == RECORD_TYPE;
}

const char *
Parser::print_type(Tree type)
{
  gcc_assert (TYPE_P (type.get_tree ()));

  if (type == integer_type_node)
    {
      return "int";
    }
  else if (type == float_type_node)
    {
      return "real";
    }
  else if (is_string_type (type))
    {
      return "string";
    }
  else if (is_array_type(type))
    {
      return "array";
    }
  if (type == void_type_node)
    {
      return "void";
    }
  else
    {
      return "<<unknown-type>>";
    }
}

Tree
Parser::retrieve_type(string type)
{
  if (type == "int" )
    {
      return integer_type_node;
    }
  else if (type == "float")
    {
      return float_type_node;
    }
  else if (type == "string")
    {
      return build_pointer_type (char_type_node);
    }
  else if (type == "void")
    {
      return void_type_node;
    }
   return NULL;
}

/*
Tree
Parser::parse_field_declaration (std::vector<std::string> &field_names)
{
   identifier ':' type ';'
  const_TokenPtr identifier = expect_token (Tiger::IDENTIFIER);
  if (identifier == NULL)
    {
      skip_after_end_of_line ();
      return Tree::error ();
    }

  skip_token (Tiger::COLON);

  Tree type = parse_type ();

  skip_token (Tiger::SEMICOLON);

  if (type.is_error ())
    return Tree::error ();

  if (std::find (field_names.begin (), field_names.end (),
		 identifier->get_str ())
      != field_names.end ())
    {
      error_at (identifier->get_locus (), "repeated field name");
      return Tree::error ();
    }
  field_names.push_back (identifier->get_str ());

  Tree field_decl
    = build_decl (identifier->get_locus (), FIELD_DECL,
		  get_identifier (identifier->get_str ().c_str ()),
		  type.get_tree());
  TREE_ADDRESSABLE (field_decl.get_tree ()) = 1;

  return field_decl;
}
*/

Tree
Parser::parse_type()
{
  /* type -> "int"
        | "float"
        | "bool"
        | IDENTIFIER
        | type '[' expr ']'
        | type '(' expr : expr ')'
        | "record" field-decl* "end"*/

  const_TokenPtr t = lexer.peek_token ();

  Tree type;

  switch (t->get_id ())
    {
    case Tiger::INT:
      lexer.skip_token ();
      type = integer_type_node;
      break;
    case Tiger::FLOAT:
      lexer.skip_token ();
      type = float_type_node;
      break;
   case Tiger::STRING:
      {
      lexer.skip_token ();
      type = build_pointer_type (char_type_node);
      break;
      }
    case Tiger::IDENTIFIER:
      {
	SymbolPtr s = query_type (t->get_str (), t->get_locus ());
        lexer.skip_token ();
	if (s == NULL)
	  type = Tree::error ();
        else
          type = TREE_TYPE (s->get_tree_decl ().get_tree ());
      }
      break;
    /*case Tiger::RECORD:
      type = parse_record ();
      break;*/
    default:
      unexpected_token (t);
      return Tree::error ();
      break;
    }

  typedef std::vector<std::pair<Tree, Tree> > Dimensions;
  Dimensions dimensions;

  t = lexer.peek_token ();
  while (t->get_id () == Tiger::LEFT_PAREN || t->get_id () == Tiger::LEFT_SQUARE)
    {
      lexer.skip_token ();

      Tree lower_bound, upper_bound;
      if (t->get_id () == Tiger::LEFT_SQUARE)
	{
	  Tree size = parse_integer_exp ();
	  skip_token (Tiger::RIGHT_SQUARE);

	  lower_bound = Tree (build_int_cst_type (integer_type_node, 0),
			      size.get_locus ());
	  upper_bound
	    = build_tree (MINUS_EXPR, size.get_locus (), integer_type_node,
			  size, build_int_cst (integer_type_node, 1));

	}
      else if (t->get_id () == Tiger::LEFT_PAREN)
	{
	  lower_bound = parse_integer_exp ();
	  skip_token (Tiger::COLON);

	  upper_bound = parse_integer_exp ();
	  skip_token (Tiger::RIGHT_PAREN);
	}
      else
	{
	  gcc_unreachable ();
	}

      dimensions.push_back (std::make_pair (lower_bound, upper_bound));
      t = lexer.peek_token ();
    }

  for (Dimensions::reverse_iterator it = dimensions.rbegin ();
       it != dimensions.rend (); it++)
    {
      it->first = Tree (fold (it->first.get_tree ()), it->first.get_locus ());
/*       if (it->first.get_tree_code () != INTEGER_CST)
 	{
 	  error_at (it->first.get_locus (), "is not an integer constant");
 	  break;
 	}*/
      it->second
	= Tree (fold (it->second.get_tree ()), it->second.get_locus ());
/*       if (it->second.get_tree_code () != INTEGER_CST)
 	{
 	  error_at (it->second.get_locus (), "is not an integer constant");
 	  break;
 	}*/

      if (!type.is_error ())
	{
	  Tree range_type
	    = build_range_type (integer_type_node, it->first.get_tree (),
				it->second.get_tree ());
	  type = build_array_type (type.get_tree (), range_type.get_tree ());
	}
    }

  return type;
}

SymbolPtr
Parser::query_type (const std::string &name, location_t loc)
{
  SymbolPtr sym = scope.lookup (name);
  if (sym == NULL)
    {
      error_at (loc, "type '%s' not declared in the current scope",
		name.c_str ());
    }
  else if (sym->get_kind () != Tiger::TYPENAME)
    {
      error_at (loc, "name '%s' is not a type", name.c_str ());
      sym = SymbolPtr();
    }
  return sym;
}

SymbolPtr
Parser::query_variable(const std::string &name, location_t loc)
{
  SymbolPtr sym = scope.lookup (name);
  if (sym == NULL)
    {
      error_at (loc, "variable '%s' not declared in the current scope",
		name.c_str ());
    }
  else if (sym->get_kind () != Tiger::VARIABLE)
    {
      error_at (loc, "name '%s' is not a variable", name.c_str ());
      sym = SymbolPtr();
    }
  return sym;
}

SymbolPtr
Parser::query_integer_variable (const std::string &name, location_t loc)
{
  SymbolPtr sym = query_variable (name, loc);
  if (sym != NULL)
    {
      Tree var_decl = sym->get_tree_decl ();
      gcc_assert (!var_decl.is_null ());
      
      if (var_decl.get_type () != integer_type_node)
	{
	  error_at (loc, "variable '%s' does not have integer type",
		    name.c_str ());
	  sym = SymbolPtr();
	}
    }

  return sym;
}

Tree
Parser::parse_assignment_exp(Tree var)
{
  /* assignment_exp -> exp ":=" exp ";"*/
  Tree variable =  var;

  if (variable.is_error ())
    return Tree::error ();

  const_TokenPtr assign_tok = expect_token (Tiger::ASSIGN);
  if (assign_tok == NULL)
    {
      return Tree::error ();
    }

  const_TokenPtr first_of_expr = lexer.peek_token ();

  Tree expr = parse_exp ();
  if (expr.is_error ())
    return Tree::error ();

  if (variable.get_type () != expr.get_type ())
    {
      error_at (first_of_expr->get_locus (),
		"cannot assign value of type %s to a variable of type %s",
		print_type (expr.get_type ()),
		print_type (variable.get_type ()));
      return Tree::error ();
    }

  Tree assign_expr = build_tree (MODIFY_EXPR, assign_tok->get_locus (),
				void_type_node, variable, expr);

  return assign_expr;
}

Tree
Parser::build_label_decl (const char *name, location_t loc)
{
  tree t = build_decl (loc, LABEL_DECL, get_identifier (name), void_type_node);

  gcc_assert (main_fndecl != NULL_TREE);
  DECL_CONTEXT (t) = main_fndecl;

  return t;
}

Tree
Parser::build_if_exp (Tree bool_expr, Tree then_part, Tree else_part)
{

  if (bool_expr.is_error ())
    return Tree::error ();

  Tree then_label_decl = build_label_decl ("then", then_part.get_locus ());

  Tree else_label_decl;
  if (!else_part.is_null ())
    else_label_decl = build_label_decl ("else", else_part.get_locus ());

  Tree endif_label_decl = build_label_decl ("end_if", then_part.get_locus ());

  Tree goto_then = build_tree (GOTO_EXPR, bool_expr.get_locus (),
			       void_type_node, then_label_decl, "build goto then");
  Tree goto_endif = build_tree (GOTO_EXPR, bool_expr.get_locus (),
				void_type_node, endif_label_decl, "build goto endif");

  Tree goto_else_or_endif;
  if (!else_part.is_null ())
    goto_else_or_endif = build_tree (GOTO_EXPR, bool_expr.get_locus (),
				     void_type_node, else_label_decl, "build goto else or endif");
  else
    goto_else_or_endif = goto_endif;

  TreeExpList exp_list;

  Tree cond_expr
    = build_tree (COND_EXPR, bool_expr.get_locus (), void_type_node, bool_expr,
		  goto_then, goto_else_or_endif, "build cond expr");
  exp_list.append (cond_expr);

  Tree then_label_expr = build_tree (LABEL_EXPR, then_part.get_locus (),
				     void_type_node, then_label_decl, "build then");
  exp_list.append (then_label_expr);

  exp_list.append (then_part);

  if (!else_part.is_null ())
    {
      /* Make sure after then part has been executed we go to the end if*/
      exp_list.append (goto_endif);

      Tree else_label_expr = build_tree (LABEL_EXPR, else_part.get_locus (),
					 void_type_node, else_label_decl, "build else label");
      exp_list.append (else_label_expr);

      exp_list.append (else_part);
    }

  /* FIXME - location*/
  Tree endif_label_expr = build_tree (LABEL_EXPR, UNKNOWN_LOCATION,
				      void_type_node, 
 					endif_label_decl, then_part.get_exp_type());
  exp_list.append (endif_label_expr);

   Tree retTree = exp_list.get_tree ();
   retTree.set_exp_type(then_part.get_exp_type());
  return retTree;
}

Tree
Parser::parse_if_exp()
{
	
  Tree expr = parse_boolean_exp ();

   if (expr.is_error ()) {
      skip_after_end ();
    return Tree::error ();
    }

  skip_token (Tiger::THEN);
  
  const_TokenPtr tokThen = lexer.peek_token ();
  enter_scope ();
  string typeThen = parse_exp_seq (&Parser::done_end_or_else, OTHER_PAREN_RULES);
  TreeSymbolMapping then_tree_scope = leave_scope ();
  Tree then_exp = then_tree_scope.bind_expr;
  then_exp.set_exp_type (typeThen);
  
    if (then_exp.is_error ()) {
      skip_after_end ();
    return Tree::error ();
    }

  Tree else_exp;
  string typeElse;
  const_TokenPtr tok = lexer.peek_token ();
  if (tok->get_id () == Tiger::ELSE)
    {
      /* Consume 'else'*/
      skip_token (Tiger::ELSE);
      
      const_TokenPtr last_of_expr = lexer.peek_token ();

      enter_scope ();
      typeElse = parse_exp_seq (&Parser::done_end_or_else, OTHER_PAREN_RULES);
      TreeSymbolMapping else_tree_scope = leave_scope ();
      else_exp = else_tree_scope.bind_expr;
      else_exp.set_exp_type (typeElse);

		if (else_exp.is_error ()) {
		    skip_after_end ();
		    return Tree::error ();
		}

		if(else_exp.get_exp_type() != then_exp.get_exp_type()){
			error_at (last_of_expr->get_locus (),
			"then exp type %s and else exp type %s differs",
			then_exp.get_exp_type().c_str(), else_exp.get_exp_type().c_str());			
			skip_after_end ();
			return Tree::error ();
		}
         return build_if_exp (expr, then_exp, else_exp);
   } if (then_exp.get_exp_type() == "void" || then_exp.get_exp_type() == "") {
         return build_if_exp (expr, then_exp, else_exp);
   }
	error_at (tokThen->get_locus (),
	"then body must no return value, but it returned %s type",
	then_exp.get_exp_type().c_str());			
	skip_after_end ();
	return Tree::error ();

}

Tree
Parser::build_while_exp(Tree bool_expr, Tree while_body)
{
  if (bool_expr.is_error ())
    return Tree::error ();

  TreeExpList exp_list;

  Tree while_check_label_decl
    = build_label_decl ("while_check", bool_expr.get_locus ());

  Tree while_check_label_expr
    = build_tree (LABEL_EXPR, bool_expr.get_locus (), void_type_node,
		  while_check_label_decl);
  exp_list.append (while_check_label_expr);

  Tree while_body_label_decl
    = build_label_decl ("while_body", while_body.get_locus ());
  Tree end_of_while_label_decl
    /* FIXME - location*/
    = build_label_decl ("end_of_while", UNKNOWN_LOCATION);

  Tree cond_expr
    = build_tree (COND_EXPR, bool_expr.get_locus (), void_type_node, bool_expr,
		  build_tree (GOTO_EXPR, bool_expr.get_locus (), void_type_node,
			      while_body_label_decl),
		  build_tree (GOTO_EXPR, bool_expr.get_locus (), void_type_node,
			      end_of_while_label_decl));
  exp_list.append (cond_expr);

  Tree while_body_label_expr
    = build_tree (LABEL_EXPR, while_body.get_locus (), void_type_node,
		  while_body_label_decl);
  exp_list.append (while_body_label_expr);

  exp_list.append (while_body);

  /* FIXME - location*/
  Tree goto_check = build_tree (GOTO_EXPR, UNKNOWN_LOCATION, void_type_node,
				while_check_label_decl);
  exp_list.append (goto_check);

  /* FIXME - location*/
  Tree end_of_while_label_expr
    = build_tree (LABEL_EXPR, UNKNOWN_LOCATION, void_type_node,
		  end_of_while_label_decl);
  exp_list.append (end_of_while_label_expr);

   Tree retTree = exp_list.get_tree ();
   retTree.set_exp_type("void");
  return retTree;

}

Tree
Parser::parse_while_exp()
{

  Tree conditional_expr = parse_boolean_exp ();


		if (conditional_expr.is_error ()) {
		    skip_after_end ();
		    return Tree::error ();
		}

  if (!skip_token (Tiger::DO))
    {
      skip_after_end ();
      return Tree::error ();
    }

  const_TokenPtr t = lexer.peek_token ();  

  enter_scope ();
  string typeWhile = parse_exp_seq (&Parser::done_end, OTHER_PAREN_RULES);
  TreeSymbolMapping while_body_tree_scope = leave_scope ();
  
  Tree while_body_exp = while_body_tree_scope.bind_expr;
  while_body_exp.set_exp_type(typeWhile);
  

		if (while_body_exp.is_error ()) {
		    skip_after_end ();
		    return Tree::error ();
		}

	if(while_body_exp.get_exp_type() == "void" || while_body_exp.get_exp_type() == ""){
		return build_while_exp (conditional_expr, while_body_exp);
	}

	error_at (t->get_locus (),
	"while body must no return value, but it returned %s type",
	while_body_exp.get_exp_type().c_str());			
	skip_after_end ();
	return Tree::error ();  
}

Tree
Parser::build_for_exp (SymbolPtr ind_var, Tree lower_bound,
			     Tree upper_bound, Tree for_body_exp_list)
{
  if (ind_var == NULL)
    return Tree::error ();
  Tree ind_var_decl = ind_var->get_tree_decl ();

  /* Lower*/
  if (lower_bound.is_error ())
    return Tree::error ();

  /* Upper*/
  if (upper_bound.is_error ())
    return Tree::error ();

  /* ind_var := lower;*/
  TreeExpList exp_list;

  Tree init_ind_var = build_tree (MODIFY_EXPR, /* FIXME */ UNKNOWN_LOCATION,
				  void_type_node, ind_var_decl, lower_bound);
  exp_list.append (init_ind_var);

  /* ind_var < upper*/
  Tree while_condition
    = build_tree (LT_EXPR, upper_bound.get_locus (), integer_type_node,
		  ind_var_decl, upper_bound);

  /* for-body
   ind_var := ind_var + 1*/
  Tree incr_ind_var
    = build_tree (MODIFY_EXPR, /* FIXME */ UNKNOWN_LOCATION, void_type_node,
		  ind_var_decl,
		  build_tree (PLUS_EXPR, UNKNOWN_LOCATION, integer_type_node,
			      ind_var_decl,
			      build_int_cst_type (::integer_type_node, 1)));

  /* Wrap as a exp list*/
  TreeExpList for_exp_list = for_body_exp_list;
  for_exp_list.append (incr_ind_var);

  /* construct the associated while exp*/
  Tree while_exp
    = build_while_exp (while_condition, for_exp_list.get_tree ());
  exp_list.append (while_exp);

  return exp_list.get_tree ();
}

Tree
Parser::parse_for_exp()
{
  enter_scope ();
  const_TokenPtr identifier = expect_token (Tiger::IDENTIFIER);
  if (identifier == NULL)
    {
      skip_after_end ();
      return Tree::error ();
    }

/*    SymbolPtr sym (new Symbol (Tiger::VARIABLE, identifier->get_str ()));
          scope.get_current_mapping ().insert (sym);

	  sym = query_variable (identifier->get_str (), identifier->get_locus ());

  
	  Tree decl;
	    decl = build_decl (identifier->get_locus (), VAR_DECL,
			  get_identifier (sym->get_name ().c_str ()),
          integer_type_node);
  
	  DECL_CONTEXT (decl.get_tree()) = main_fndecl;

	  gcc_assert (!stack_var_decl_chain.empty ());
	  stack_var_decl_chain.back ().append (decl);

	  sym->set_tree_decl (decl);

*/
  if (!skip_token (Tiger::ASSIGN))
    {
      skip_after_end ();
      return Tree::error ();
    }

  Tree lower_bound = parse_integer_exp ();

  if (!skip_token (Tiger::TO))
    {
      skip_after_end ();
      return Tree::error ();
    }

  Tree upper_bound = parse_integer_exp ();

  if (!skip_token (Tiger::DO))
    {
      skip_after_end ();
      return Tree::error ();
    }

  const_TokenPtr t = lexer.peek_token ();  

  string typeFor =  parse_exp_seq (&Parser::done_end, OTHER_PAREN_RULES);



	  SymbolPtr ind_var
	    = query_integer_variable (identifier->get_str (), identifier->get_locus ());

         TreeSymbolMapping for_body_tree_scope = leave_scope ();
         Tree for_body_exp = for_body_tree_scope.bind_expr;
         for_body_exp.set_exp_type(typeFor);

		if (for_body_exp.is_error ()) {
		    skip_after_end ();
		    return Tree::error ();
		}
     
	if(for_body_exp.get_exp_type() == "void" || for_body_exp.get_exp_type() == ""){
	  return build_for_exp (ind_var, lower_bound, upper_bound, for_body_exp);
	}

	error_at (t->get_locus (),
	"for body must no return value, but it returned %s type",
	for_body_exp.get_exp_type().c_str());			
	skip_after_end ();
	return Tree::error (); 
}

Tree
Parser::get_not_addr ()
{
  if (not_fn.is_null ())
    {
      tree fndecl_type_param[] = {
	build_pointer_type (
	  build_qualified_type (char_type_node,
				TYPE_QUAL_CONST)) /* const char* */
      };
      tree fndecl_type
	= build_function_type_array (integer_type_node, 1, fndecl_type_param);

      tree not_fn_decl = build_fn_decl ("nao", fndecl_type);
      DECL_EXTERNAL (not_fn_decl) = 1;

      not_fn
	= build1 (ADDR_EXPR, build_pointer_type (fndecl_type), not_fn_decl);
    }

  return not_fn;
}

Tree
Parser::parse_not_function ()
{
  
 if (!skip_token (Tiger::LEFT_PAREN))
    {
      return Tree::error ();
    }
  const_TokenPtr first_of_expr = lexer.peek_token ();
  Tree expr = parse_exp ();
  
   if (expr.is_error ())
    return Tree::error ();

   if (!skip_token (Tiger::RIGHT_PAREN))
    {
      return Tree::error ();
    }
  

   if (expr.get_type () == integer_type_node){
      tree args[] = {expr.get_tree ()};

      Tree not_fn = get_not_addr ();

      tree stmt
	= build_call_array_loc (first_of_expr->get_locus (), integer_type_node,
				not_fn.get_tree (), 1, args);
      return stmt;
    } else {
      error_at (first_of_expr->get_locus (),
		"type int is expected not operand, but value is of type %s",
		print_type (expr.get_type ()));
      return Tree::error ();
    }
  
  gcc_unreachable ();
}


Tree
Parser::get_exit_addr ()
{
  if (exit_fn.is_null ())
    {
      tree fndecl_type_param[] = {
	build_pointer_type (
	  build_qualified_type (char_type_node,
				TYPE_QUAL_CONST)) /* const char* */
      };
      tree fndecl_type
	= build_function_type_array (void_type_node, 1, fndecl_type_param);

      tree exit_fn_decl = build_fn_decl ("exit", fndecl_type);
      DECL_EXTERNAL (exit_fn_decl) = 1;

      exit_fn
	= build1 (ADDR_EXPR, build_pointer_type (fndecl_type), exit_fn_decl);
    }

  return exit_fn;
}

Tree
Parser::parse_exit_function ()
{
  
 if (!skip_token (Tiger::LEFT_PAREN))
    {
      return Tree::error ();
    }
  const_TokenPtr first_of_expr = lexer.peek_token ();
  Tree expr = parse_exp ();
  
   if (expr.is_error ())
    return Tree::error ();

   if (!skip_token (Tiger::RIGHT_PAREN))
    {
      return Tree::error ();
    }

   if (expr.get_type () == integer_type_node){
      tree args[] = {expr.get_tree ()};

      Tree exit_fn = get_exit_addr ();

      tree stmt
	= build_call_array_loc (first_of_expr->get_locus (), void_type_node,
				exit_fn.get_tree (), 1, args);
      return stmt;
    } else {
      error_at (first_of_expr->get_locus (),
		"type int is expected exit operand, but value is of type %s",
		print_type (expr.get_type ()));
      return Tree::error ();
    }
  
  gcc_unreachable ();
}

Tree
Parser::get_chr_addr ()
{
  if (chr_fn.is_null ())
    {
      tree fndecl_type_param[] = {
	build_pointer_type (
	  build_qualified_type (char_type_node,
				TYPE_QUAL_CONST)) /* const char* */
      };
      tree fndecl_type
	= build_function_type_array (build_pointer_type (char_type_node), 1,
					     fndecl_type_param);

      tree chr_fn_decl = build_fn_decl ("inttochar", fndecl_type);
      DECL_EXTERNAL (chr_fn_decl) = 1;

      chr_fn
	= build1 (ADDR_EXPR, build_pointer_type (fndecl_type), chr_fn_decl);
    }

  return chr_fn;
}

Tree
Parser::parse_chr_function ()
{
  
 if (!skip_token (Tiger::LEFT_PAREN))
    {
      return Tree::error ();
    }
  const_TokenPtr first_of_expr = lexer.peek_token ();
  Tree expr = parse_exp ();
  
   if (expr.is_error ())
    return Tree::error ();

   if (!skip_token (Tiger::RIGHT_PAREN))
    {
      return Tree::error ();
    }

   if (expr.get_type () == integer_type_node){
      tree args[] = {expr.get_tree ()};

      Tree chr_fn = get_chr_addr ();

      tree stmt
	= build_call_array_loc (first_of_expr->get_locus (), build_pointer_type (char_type_node),
				chr_fn.get_tree (), 1, args);
      return stmt;
    } else {
      error_at (first_of_expr->get_locus (),
		"type int is expected chr operand, but value is of type %s",
		print_type (expr.get_type ()));
      return Tree::error ();
    }
  
  gcc_unreachable ();
}

Tree
Parser::get_substring_addr ()
{
  if (substring_fn.is_null ())
    {
      tree fndecl_type_param[] = {
	build_pointer_type (
	  build_qualified_type (char_type_node,
				TYPE_QUAL_CONST)) /* const char* */
      };
      tree fndecl_type
	= build_varargs_function_type_array (build_pointer_type (char_type_node), 1,
					     fndecl_type_param);

      tree substring_fn_decl = build_fn_decl ("subpalavra", fndecl_type);
      DECL_EXTERNAL (substring_fn_decl) = 1;

      substring_fn
	= build1 (ADDR_EXPR, build_pointer_type (fndecl_type), substring_fn_decl);
    }

  return substring_fn;
}

Tree
Parser::parse_substring_function ()
{
 
 if (!skip_token (Tiger::LEFT_PAREN))
    {
      return Tree::error ();
    }
  const_TokenPtr first_of_expr = lexer.peek_token ();
  Tree expr1 = parse_exp ();
  
   if (expr1.is_error ())
    return Tree::error ();


   if (!skip_token (Tiger::COMMA))
    {
      return Tree::error ();
    }

  const_TokenPtr second_of_expr = lexer.peek_token ();
   Tree expr2 = parse_exp ();

  if (expr2.is_error ())
    return Tree::error ();
  
  if (!skip_token (Tiger::COMMA))
    {
      return Tree::error ();
    }

  const_TokenPtr third_of_expr = lexer.peek_token ();
   Tree expr3 = parse_exp ();

   if (expr3.is_error ())
    return Tree::error ();

   if (!skip_token (Tiger::RIGHT_PAREN))
    {
      return Tree::error ();
    }

  if (!is_string_type (expr1.get_type ())){
     error_at (first_of_expr->get_locus (),
		"type string is expected at first substring operand, but value is of type %s",
		print_type (expr1.get_type ()));
      return Tree::error ();
  } else if(expr2.get_type () != integer_type_node){
     error_at (second_of_expr->get_locus (),
		"type int is expected at second substring operand, but value is of type %s",
		print_type (expr2.get_type ()));
      return Tree::error ();
    } else if(expr3.get_type () != integer_type_node){
     error_at (third_of_expr->get_locus (),
		"type int is expected at third substring operand, but value is of type %s",
		print_type (expr3.get_type ()));
      return Tree::error ();
    } else {
      tree args[] = {expr1.get_tree (), expr2.get_tree (), expr3.get_tree ()};

      Tree substring_fn = get_substring_addr ();

      tree stmt
	= build_call_array_loc (first_of_expr->get_locus (), build_pointer_type (char_type_node),
				substring_fn.get_tree (), 3, args);
      return stmt;
    }
  
  gcc_unreachable ();
}



Tree
Parser::get_concat_addr ()
{
  if (concat_fn.is_null ())
    {
      tree fndecl_type_param[] = {
	build_pointer_type (
	  build_qualified_type (char_type_node,
				TYPE_QUAL_CONST)) /* const char* */
      };
      tree fndecl_type
	= build_varargs_function_type_array (build_pointer_type (char_type_node), 1,
					     fndecl_type_param);

      tree concat_fn_decl = build_fn_decl ("concatena", fndecl_type);
      DECL_EXTERNAL (concat_fn_decl) = 1;

      concat_fn
	= build1 (ADDR_EXPR, build_pointer_type (fndecl_type), concat_fn_decl);
    }

  return concat_fn;
}

Tree
Parser::parse_concat_function ()
{
 
 if (!skip_token (Tiger::LEFT_PAREN))
    {
      return Tree::error ();
    }
  const_TokenPtr first_of_expr = lexer.peek_token ();
  Tree expr1 = parse_exp ();
  
   if (expr1.is_error ())
    return Tree::error ();


   if (!skip_token (Tiger::COMMA))
    {
      return Tree::error ();
    }

  const_TokenPtr second_of_expr = lexer.peek_token ();
   Tree expr2 = parse_exp ();
  
   if (expr2.is_error ())
    return Tree::error ();

   if (!skip_token (Tiger::RIGHT_PAREN))
    {
      return Tree::error ();
    }

  if (!is_string_type (expr1.get_type ())){
     error_at (first_of_expr->get_locus (),
		"type string is expected at first concat operand, but value is of type %s",
		print_type (expr1.get_type ()));
      return Tree::error ();
  } else if(!is_string_type (expr2.get_type ())){
     error_at (second_of_expr->get_locus (),
		"type string is expected at second concat operand, but value is of type %s",
		print_type (expr2.get_type ()));
      return Tree::error ();
    } else {
      tree args[] = {expr1.get_tree (), expr2.get_tree ()};

      Tree concat_fn = get_concat_addr ();

      tree stmt
	= build_call_array_loc (first_of_expr->get_locus (), build_pointer_type (char_type_node),
				concat_fn.get_tree (), 2, args);
      return stmt;
    }
  
  gcc_unreachable ();
}

Tree
Parser::get_strcmp_addr ()
{
  if (strcmp_fn.is_null ())
    {
      tree fndecl_type_param[] = {
	build_pointer_type (
	  build_qualified_type (char_type_node,
				TYPE_QUAL_CONST)) /* const char* */
      };
      tree fndecl_type
	= build_varargs_function_type_array (integer_type_node, 1,
					     fndecl_type_param);

      tree strcmp_fn_decl = build_fn_decl ("stringIgual", fndecl_type);
      DECL_EXTERNAL (strcmp_fn_decl) = 1;

      strcmp_fn
	= build1 (ADDR_EXPR, build_pointer_type (fndecl_type), strcmp_fn_decl);
    }

  return strcmp_fn;
}

Tree
Parser::parse_strcmp_function ()
{
 
 if (!skip_token (Tiger::LEFT_PAREN))
    {
      return Tree::error ();
    }
  const_TokenPtr first_of_expr = lexer.peek_token ();
  Tree expr1 = parse_exp ();
  
   if (expr1.is_error ())
    return Tree::error ();


   if (!skip_token (Tiger::COMMA))
    {
      return Tree::error ();
    }

  const_TokenPtr second_of_expr = lexer.peek_token ();
   Tree expr2 = parse_exp ();
  
   if (expr2.is_error ())
    return Tree::error ();

   if (!skip_token (Tiger::RIGHT_PAREN))
    {
      return Tree::error ();
    }

  if (!is_string_type (expr1.get_type ())){
     error_at (first_of_expr->get_locus (),
		"type string is expected at first strcmp operand, but value is of type %s",
		print_type (expr1.get_type ()));
      return Tree::error ();
  } else if(!is_string_type (expr2.get_type ())){
     error_at (second_of_expr->get_locus (),
		"type string is expected at second strcmp operand, but value is of type %s",
		print_type (expr2.get_type ()));
      return Tree::error ();
    } else {
      tree args[] = {expr1.get_tree (), expr2.get_tree ()};

      Tree strcmp_fn = get_strcmp_addr ();

      tree stmt
	= build_call_array_loc (first_of_expr->get_locus (), integer_type_node,
				strcmp_fn.get_tree (), 2, args);
      return stmt;
    }
  
  gcc_unreachable ();
}


Tree
Parser::get_size_addr ()
{
  if (size_fn.is_null ())
    {
      tree fndecl_type_param[] = {
	build_pointer_type (
	  build_qualified_type (char_type_node,
				TYPE_QUAL_CONST)) /* const char* */
      };
      tree fndecl_type
	= build_varargs_function_type_array (integer_type_node, 1,
					     fndecl_type_param);

      tree size_fn_decl = build_fn_decl ("tamanho", fndecl_type);
      DECL_EXTERNAL (size_fn_decl) = 1;

      size_fn
	= build1 (ADDR_EXPR, build_pointer_type (fndecl_type), size_fn_decl);
    }

  return size_fn;
}

Tree
Parser::parse_size_function ()
{
 
 if (!skip_token (Tiger::LEFT_PAREN))
    {
      return Tree::error ();
    }
  const_TokenPtr first_of_expr = lexer.peek_token ();
  Tree expr = parse_exp ();
  
   if (expr.is_error ())
    return Tree::error ();

   if (!skip_token (Tiger::RIGHT_PAREN))
    {
      return Tree::error ();
    }

  if (is_string_type (expr.get_type ())){
      tree args[] = {expr.get_tree ()};

      Tree size_fn = get_size_addr ();

      tree stmt
	= build_call_array_loc (first_of_expr->get_locus (), integer_type_node,
				size_fn.get_tree (), 1, args);
      return stmt;
    } else {
      error_at (first_of_expr->get_locus (),
		"type string is expected size operand, but value is of type %s",
		print_type (expr.get_type ()));
      return Tree::error ();
    }
  
  gcc_unreachable ();
}

Tree
Parser::get_ord_addr ()
{
  if (ord_fn.is_null ())
    {
      tree fndecl_type_param[] = {
	build_pointer_type (
	  build_qualified_type (char_type_node,
				TYPE_QUAL_CONST)) /* const char* */
      };
      tree fndecl_type
	= build_varargs_function_type_array (build_pointer_type (char_type_node), 1,
					     fndecl_type_param);

      tree ord_fn_decl = build_fn_decl ("chartoint", fndecl_type);
      DECL_EXTERNAL (ord_fn_decl) = 1;

      ord_fn
	= build1 (ADDR_EXPR, build_pointer_type (fndecl_type), ord_fn_decl);
    }

  return ord_fn;
}


Tree
Parser::parse_ord_function()
{

 if (!skip_token (Tiger::LEFT_PAREN))
    {
      return Tree::error ();
    }
  const_TokenPtr first_of_expr = lexer.peek_token ();
  Tree expr = parse_exp ();
  
   if (expr.is_error ())
    return Tree::error ();

   if (!skip_token (Tiger::RIGHT_PAREN))
    {
      return Tree::error ();
    }

  if (is_string_type (expr.get_type ())){
      tree args[] = {expr.get_tree ()};

      Tree ord_fn = get_ord_addr ();

      tree stmt
	= build_call_array_loc (first_of_expr->get_locus (), integer_type_node,
				ord_fn.get_tree (), 1, args);
      return stmt;
    } else {
      error_at (first_of_expr->get_locus (),
		"type string is expected ord operand, but value is of type %s",
		print_type (expr.get_type ()));
      return Tree::error ();
    }
  
  gcc_unreachable ();
}


Tree
Parser::get_getchar_addr ()
{
  if (getchar_fn.is_null ())
    {
      tree fndecl_type_param[] = {
	build_pointer_type (
	  build_qualified_type (char_type_node,
				TYPE_QUAL_CONST)) /* const char* */
      };
      tree fndecl_type
	= build_varargs_function_type_array (build_pointer_type (char_type_node), 0,
					     fndecl_type_param);

      tree getchar_fn_decl = build_fn_decl ("pegachar", fndecl_type);
      DECL_EXTERNAL (getchar_fn_decl) = 1;

      getchar_fn
	= build1 (ADDR_EXPR, build_pointer_type (fndecl_type), getchar_fn_decl);
    }

  return getchar_fn;
}

Tree
Parser::parse_read_function ()
{
  const_TokenPtr first_of_expr = lexer.peek_token ();
  if (!skip_token (Tiger::LEFT_PAREN))
    {
      return Tree::error ();
    }

   if (!skip_token (Tiger::RIGHT_PAREN))
    {
      return Tree::error ();
    }

  tree args[]= {};

  Tree getchar_fn = get_getchar_addr ();

  tree stmt
    = build_call_array_loc (first_of_expr->get_locus (), build_pointer_type (char_type_node),
			    getchar_fn.get_tree (), 0, args);

  return stmt;
}

Tree
Parser::get_puts_addr ()
{
  if (puts_fn.is_null ())
    {
      tree fndecl_type_param[] = {
	build_pointer_type (
	  build_qualified_type (char_type_node,
				TYPE_QUAL_CONST)) /* const char* */
      };
      tree fndecl_type
	= build_function_type_array (void_type_node, 1, fndecl_type_param);

      tree puts_fn_decl = build_fn_decl ("printa", fndecl_type);
      DECL_EXTERNAL (puts_fn_decl) = 1;

      puts_fn
	= build1 (ADDR_EXPR, build_pointer_type (fndecl_type), puts_fn_decl);
    }

  return puts_fn;
}

Tree
Parser::get_printf_addr ()
{
  if (printf_fn.is_null ())
    {
      tree fndecl_type_param[] = {
	build_pointer_type (
	  build_qualified_type (char_type_node,
				TYPE_QUAL_CONST)) /* const char* */
      };
      tree fndecl_type
	= build_varargs_function_type_array (integer_type_node, 1,
					     fndecl_type_param);

      tree printf_fn_decl = build_fn_decl ("printf", fndecl_type);
      DECL_EXTERNAL (printf_fn_decl) = 1;

      printf_fn
	= build1 (ADDR_EXPR, build_pointer_type (fndecl_type), printf_fn_decl);
    }

  return printf_fn;
}

Tree
Parser::parse_write_function()
{

 if (!skip_token (Tiger::LEFT_PAREN))
    {
      return Tree::error ();
    }
  const_TokenPtr first_of_expr = lexer.peek_token ();
  Tree expr = parse_exp ();
   if (expr.is_error ())
    return Tree::error ();

   if (!skip_token (Tiger::RIGHT_PAREN))
    {
      return Tree::error ();
    }

 if (expr.get_type () == integer_type_node)
    {
      const char *format_integer = "%d\n";
      tree args[]
	= {build_string_literal (strlen (format_integer) + 1, format_integer),
	   expr.get_tree ()};

      Tree printf_fn = get_printf_addr ();

      tree stmt
	= build_call_array_loc (first_of_expr->get_locus (), integer_type_node,
				printf_fn.get_tree (), 2, args);

      return stmt;
    }  else if (expr.get_type () == float_type_node)
    {
      const char *format_float = "%f\n";
      tree args[]
	= {build_string_literal (strlen (format_float) + 1, format_float),
	   convert (double_type_node, expr.get_tree ())};

      Tree printf_fn = get_printf_addr ();

      tree stmt
	= build_call_array_loc (first_of_expr->get_locus (), integer_type_node,
				printf_fn.get_tree (), 2, args);

      return stmt;
    } else if (is_string_type (expr.get_type ())){
      tree args[] = {expr.get_tree ()};

      Tree puts_fn = get_puts_addr ();

      tree stmt
	= build_call_array_loc (first_of_expr->get_locus (), void_type_node,
				puts_fn.get_tree (), 1, args);
      return stmt;
    } else {
      error_at (first_of_expr->get_locus (),
		"a primitive type is expected print operand, but value is of type %s",
		print_type (expr.get_type ()));
      return Tree::error ();
    } 
  gcc_unreachable ();
}

Tree
Parser::parse_exp() 
{
  return parse_exp (/* right_binding_power */ 0);
}


/* This is a Pratt parser*/
Tree
Parser::parse_exp(int right_binding_power)
{
  const_TokenPtr current_token = lexer.peek_token ();
  lexer.skip_token ();
 
  Tree expr = null_denotation (current_token);
  if (expr.is_error ())
    return Tree::error ();

  while (right_binding_power < left_binding_power (lexer.peek_token ()))
    {
      current_token = lexer.peek_token();
      lexer.skip_token ();

      expr = left_denotation (current_token, expr);
      if (expr.is_error ())
	return Tree::error ();
    }
  return expr;
}

namespace
{
enum binding_powers
{
  /* Highest priority*/
  LBP_HIGHEST = 100,

  LBP_DOT = 90,

  LBP_ARRAY_REF = 80,

  LBP_UNARY_MINUS = 55, 
  LBP_UNARY_PLUS =50,

  LBP_MUL = 40,
  LBP_DIV = LBP_MUL,

  LBP_PLUS = 30,
  LBP_MINUS = LBP_PLUS,

  LBP_EQUAL = 20,
  LBP_DIFFERENT = LBP_EQUAL,
  LBP_LOWER_THAN = LBP_EQUAL,
  LBP_LOWER_EQUAL = LBP_EQUAL,
  LBP_GREATER_THAN = LBP_EQUAL,
  LBP_GREATER_EQUAL = LBP_EQUAL,

  LBP_LOGICAL_AND = 10,
  LBP_LOGICAL_OR = 5,

  /* Lowest priority*/
  LBP_LOWEST = 0,
};
}

/* This implements priorities*/
int
Parser::left_binding_power(const_TokenPtr token)
{
  switch (token->get_id ())
    {
    case Tiger::DOT:
      return LBP_DOT;
    case Tiger::LEFT_SQUARE:
      return LBP_ARRAY_REF;
    case Tiger::ASTERISK:
      return LBP_MUL;
    case Tiger::SLASH:
      return LBP_DIV;
    case Tiger::PLUS:
      return LBP_PLUS;
    case Tiger::MINUS:
      return LBP_MINUS;
    case Tiger::EQUAL:
      return LBP_EQUAL;
    case Tiger::DIFFERENT:
      return LBP_DIFFERENT;
    case Tiger::GREATER:
      return LBP_GREATER_THAN;
    case Tiger::GREATER_OR_EQUAL:
      return LBP_GREATER_EQUAL;
    case Tiger::LOWER:
      return LBP_LOWER_THAN;
    case Tiger::LOWER_OR_EQUAL:
      return LBP_LOWER_EQUAL;
    case Tiger::OR:
      return LBP_LOGICAL_OR;
    case Tiger::AND:
      return LBP_LOGICAL_AND;
    /* Anything that cannot appear after a left operand
     is considered a terminator*/
    default:
      return LBP_LOWEST;
    }
}

/* This is invoked when a token (including prefix operands) is found at a
 "prefix" position*/
Tree
Parser::null_denotation(const_TokenPtr tok)
{
  switch (tok->get_id ())
    {
    case Tiger::IDENTIFIER:
      {
	SymbolPtr s = query_variable (tok->get_str (), tok->get_locus ());
	if (s == NULL)
	  return Tree::error ();
        if(lexer.peek_token ()->get_id () == Tiger::ASSIGN){
              return parse_assignment_exp (Tree (s->get_tree_decl (), tok->get_locus ()));
        }else{
              return Tree (s->get_tree_decl (), tok->get_locus ());
        }
	
      }
    case Tiger::INTEGER_LITERAL:
      /* FIXME : check ranges */
      return Tree (build_int_cst_type (integer_type_node,
				       atoi (tok->get_str ().c_str ())),
		   tok->get_locus (), "int");
      break;
     case Tiger::REAL_LITERAL:
      {
	REAL_VALUE_TYPE real_value;
	real_from_string3 (&real_value, tok->get_str ().c_str (),
			   TYPE_MODE (float_type_node));

	return Tree (build_real (float_type_node, real_value),
		     tok->get_locus (), "float");
      }
      break;
    case Tiger::STRING_LITERAL:
      {
	std::string str = tok->get_str ();
	const char *c_str = str.c_str ();
	return Tree (build_string_literal (::strlen (c_str) + 1, c_str),
		     tok->get_locus (), "string");
      }
      break;
    case Tiger::LEFT_PAREN:
      {
	 enter_scope ();
 	 parse_exp_seq (&Parser::done_right_paren, OTHER_PAREN_RULES);
	 TreeSymbolMapping paren_body_tree_scope = leave_scope ();
  	Tree paren_body_exp = paren_body_tree_scope.bind_expr;

        if (paren_body_exp.is_error ())
          return Tree::error ();  
  
	tok = lexer.peek_token ();
	if (tok->get_id () != Tiger::RIGHT_PAREN)
	  error_at (tok->get_locus (), "expecting ) but %s found\n",
		    tok->get_token_description ());
	else
	  lexer.skip_token ();

	return Tree (paren_body_exp, tok->get_locus (), paren_body_exp.get_exp_type());
      }
    case Tiger::PLUS:
      {
	Tree expr = parse_exp (LBP_UNARY_PLUS);
	if (expr.is_error ())
	  return Tree::error ();
	if (expr.get_type () != integer_type_node
	    || expr.get_type () != float_type_node)
	  {
	    error_at (tok->get_locus (),
		      "operand of unary plus must be int or real but it is %s",
		      print_type (expr.get_type ()));
	    return Tree::error ();
	  } 
	return Tree (expr, tok->get_locus ());
      }
    case Tiger::LET:
      return parse_let_exp ();
    case Tiger::IF:
      return parse_if_exp ();
    case Tiger::FOR:
      return parse_for_exp ();
    case Tiger::WHILE:
      return parse_while_exp();
    case Tiger::READ:
      {
      Tree retTree = parse_read_function ();
      retTree.set_exp_type("string");
      return retTree;
      }
      break;
    case Tiger::WRITE:
      {
      Tree retTree = parse_write_function ();
      retTree.set_exp_type("void");
      return retTree;
      }
      break;
  case Tiger::ORD:
      {
      Tree retTree = parse_ord_function ();
      retTree.set_exp_type("int");
      return retTree;
      }
      break;
  case Tiger::CHR:
      {
      Tree retTree = parse_chr_function ();
      retTree.set_exp_type("string");
      return retTree;
      }
      break;
   case Tiger::CONCAT:
      {
      Tree retTree = parse_concat_function ();
      retTree.set_exp_type("string");
      return retTree;
      }
      break;
   case Tiger::SUBSTRING:
      {
      Tree retTree = parse_substring_function ();
      retTree.set_exp_type("string");
      return retTree;
      }
      break;
  case Tiger::STRCMP:
      {
      Tree retTree = parse_strcmp_function ();
      retTree.set_exp_type("int");
      return retTree;
      }
      break;
   case Tiger::NOT:
      {
      Tree retTree = parse_not_function ();
      retTree.set_exp_type("int");
      return retTree;
      }
      break;
   case Tiger::EXIT:
      {
      Tree retTree = parse_exit_function ();
      retTree.set_exp_type("void");
      return retTree;
      }
      break;
   case Tiger::SIZE:
      {
      Tree retTree = parse_size_function ();
      retTree.set_exp_type("int");
      return retTree;
      }
      break;
    case Tiger::MINUS:
      {
	Tree expr = parse_exp (LBP_UNARY_MINUS);
	if (expr.is_error ())
	  return Tree::error ();

	if (expr.get_type () != integer_type_node
	    || expr.get_type () != float_type_node)
	  {
	    error_at (
	      tok->get_locus (),
	      "operand of unary minus must be int or real but it is %s",
	      print_type (expr.get_type ()));
	    return Tree::error ();
	  }

	expr
	  = build_tree (NEGATE_EXPR, tok->get_locus (), expr.get_type (), expr);
	return expr;
      }
    break;
    default:
      unexpected_token (tok);
      return Tree::error ();
    }
}

Tree
Parser::coerce_binary_arithmetic(const_TokenPtr tok, Tree *left, Tree *right)
{
  Tree left_type = left->get_type ();
  Tree right_type = right->get_type ();

  if (left_type.is_error () || right_type.is_error ())
    return Tree::error ();

  if (left_type == right_type)
    {
      if (left_type == integer_type_node || left_type == float_type_node)
	{
	  return left_type;
	}
    }

  error_at (tok->get_locus (),
	    "invalid operands of type %s and %s for operator %s",
	    print_type (left_type), print_type (right_type),
	    tok->get_token_description ());
  return Tree::error ();
}

Parser::BinaryHandler
Parser::get_binary_handler (TokenId id)
{
  switch (id)
    {
#define BINARY_HANDLER(name, token_id)                                         \
  case Tiger::token_id:                                                         \
    return &Parser::binary_##name;
      BINARY_HANDLER_LIST
#undef BINARY_HANDLER
    default:
      return NULL;
    }
}

Tree
Parser::binary_plus(const_TokenPtr tok, Tree left)
{
  Tree right = parse_exp (LBP_PLUS);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (PLUS_EXPR, tok->get_locus (), tree_type, left, right, print_type(left.get_type()));
}

Tree
Parser::binary_minus(const_TokenPtr tok, Tree left)
{
  Tree right = parse_exp (LBP_MINUS);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (MINUS_EXPR, tok->get_locus (), tree_type, left, right, print_type(left.get_type()));
}

Tree
Parser::binary_mult(const_TokenPtr tok, Tree left)
{
  Tree right = parse_exp (LBP_MUL);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (MULT_EXPR, tok->get_locus (), tree_type, left, right, print_type(left.get_type()));
}

Tree
Parser::binary_div(const_TokenPtr tok, Tree left)
{
  Tree right = parse_exp (LBP_DIV);
  if (right.is_error ())
    return Tree::error ();

  if (left.get_type () == integer_type_node
      && right.get_type () == integer_type_node)
    {
      /* Integer division (truncating, like in C)*/
      return build_tree (TRUNC_DIV_EXPR, tok->get_locus (), integer_type_node,
			 left, right, print_type(left.get_type()));
    }
  else
    {
      /* Real division*/
      Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
      if (tree_type.is_error ())
	return Tree::error ();

      gcc_assert (tree_type == float_type_node);

      return build_tree (RDIV_EXPR, tok->get_locus (), tree_type, left, right, print_type(left.get_type()));
    }
}


Tree
Parser::binary_equal(const_TokenPtr tok, Tree left)
{
  Tree right = parse_exp (LBP_EQUAL);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (EQ_EXPR, tok->get_locus (), integer_type_node, left,
		     right, "int");
}

Tree
Parser::binary_different(const_TokenPtr tok, Tree left)
{
  Tree right = parse_exp (LBP_DIFFERENT);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (NE_EXPR, tok->get_locus (), integer_type_node, left,
		     right, "int");
}

Tree
Parser::binary_lower_than(const_TokenPtr tok, Tree left)
{
  Tree right = parse_exp (LBP_LOWER_THAN);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (LT_EXPR, tok->get_locus (), integer_type_node, left,
		     right, "int");
}

Tree
Parser::binary_lower_equal(const_TokenPtr tok, Tree left)
{
  Tree right = parse_exp (LBP_LOWER_EQUAL);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (LE_EXPR, tok->get_locus (), integer_type_node, left,
		     right, "int");
}

Tree
Parser::binary_greater_than(const_TokenPtr tok, Tree left)
{
  Tree right = parse_exp (LBP_GREATER_THAN);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (GT_EXPR, tok->get_locus (), integer_type_node, left,
		     right, "int");
}

Tree
Parser::binary_greater_equal(const_TokenPtr tok, Tree left)
{
  Tree right = parse_exp (LBP_GREATER_EQUAL);
  if (right.is_error ())
    return Tree::error ();

  Tree tree_type = coerce_binary_arithmetic (tok, &left, &right);
  if (tree_type.is_error ())
    return Tree::error ();

  return build_tree (GE_EXPR, tok->get_locus (), integer_type_node, left,
		     right, "int");
}

bool
Parser::check_logical_operands(const_TokenPtr tok, Tree left, Tree right)
{
  if (left.get_type () != integer_type_node
      || right.get_type () != integer_type_node)
    {
      error_at (
	tok->get_locus (),
	"operands of operator %s must be integer but they are %s and %s\n",
	tok->get_token_description (), print_type (left.get_type ()),
	print_type (right.get_type ()));
      return false;
    }

  return true;
}

Tree
Parser::binary_logical_and(const_TokenPtr tok, Tree left)
{
  Tree right = parse_exp (LBP_LOGICAL_AND);
  if (right.is_error ())
    return Tree::error ();

  if (!check_logical_operands (tok, left, right))
    return Tree::error ();

  return build_tree (TRUTH_ANDIF_EXPR, tok->get_locus (), integer_type_node,
		     left, right, "int");
}

Tree
Parser::binary_logical_or(const_TokenPtr tok, Tree left)
{
  Tree right = parse_exp (LBP_LOGICAL_OR);
  if (right.is_error ())
    return Tree::error ();

  if (!check_logical_operands (tok, left, right))
    return Tree::error ();

  return build_tree (TRUTH_ORIF_EXPR, tok->get_locus (), integer_type_node,
		     left, right, "int");
}

Tree
Parser::binary_array_ref(const const_TokenPtr tok, Tree left)
{
  Tree right = parse_integer_exp ();
  if (right.is_error ())
    return Tree::error ();

  if (!skip_token (Tiger::RIGHT_SQUARE))
    return Tree::error ();

  if (!is_array_type (left.get_type ()))
    {
      error_at (left.get_locus(), "does not have array type");
      return Tree::error ();
    }

  Tree element_type = TREE_TYPE(left.get_type().get_tree());

  return build_tree (ARRAY_REF, tok->get_locus (), element_type, left, right, Tree(), Tree());
}

Tree
Parser::binary_field_ref(const const_TokenPtr tok, Tree left)
{
  const_TokenPtr identifier = expect_token (Tiger::IDENTIFIER);
  if (identifier == NULL)
    {
      return Tree::error ();
    }

  if (!is_record_type (left.get_type ()))
    {
      error_at (left.get_locus (), "does not have record type");
      return Tree::error ();
    }

  Tree field_decl = TYPE_FIELDS (left.get_type ().get_tree ());
  while (!field_decl.is_null ())
    {
      Tree decl_name = DECL_NAME (field_decl.get_tree ());
      const char *field_name = IDENTIFIER_POINTER (decl_name.get_tree ());

      if (field_name == identifier->get_str ())
	break;

      field_decl = TREE_CHAIN (field_decl.get_tree ());
    }

  if (field_decl.is_null ())
    {
      error_at (left.get_locus (),
		"record type does not have a field named '%s'",
		identifier->get_str ().c_str ());
      return Tree::error ();
    }

  return build_tree (COMPONENT_REF, tok->get_locus (),
		     TREE_TYPE (field_decl.get_tree ()), left, field_decl,
		     Tree ());
}

/* This is invoked when a token (likely an operand) is found at a (likely
 infix) non-prefix position*/
Tree
Parser::left_denotation(const_TokenPtr tok, Tree left)
{
  BinaryHandler binary_handler = get_binary_handler (tok->get_id ());
  if (binary_handler == NULL)
    {
      unexpected_token (tok);
      return Tree::error ();
    }

  return (this->*binary_handler) (tok, left);
}

Tree
Parser::parse_boolean_exp()
{
  Tree expr = parse_exp ();
  if (expr.is_error ())
    return expr;
  if (expr.get_type () != integer_type_node)
    {
      error_at (expr.get_locus (),
		"expected exp of integer type but its type is %s",
		print_type (expr.get_type ()));
      return Tree::error ();
    }
  return expr;
}

Tree
Parser::parse_integer_exp()
{
  Tree expr = parse_exp ();
  if (expr.is_error ())
    return expr;

  if (expr.get_type () != integer_type_node)
    {
      error_at (expr.get_locus (),
		"expected exp of integer type but its type is %s",
		print_type (expr.get_type ()));
      return Tree::error ();
    }
  return expr;
}

Tree
Parser::parse_exp_naming_variable()
{
  Tree expr = parse_exp ();
  if (expr.is_error ())
    return expr;
  // talvez mudar aqui
  if (expr.get_tree_code () != MODIFY_EXPR && expr.get_tree_code () != ARRAY_REF
      && expr.get_tree_code () != COMPONENT_REF)
    {
      error_at (expr.get_locus (),
		"does not designate a variable, array element or field");
      return Tree::error ();
    }
  return expr;
}

}

/* ------------------------------------------------------
 ------------------------------------------------------
 ------------------------------------------------------*/

static void tiger_parse_file (const char *filename);

void
tiger_parse_files (int num_files, const char **files)
{
  for (int i = 0; i < num_files; i++)
    {
      tiger_parse_file (files[i]);
    }
}

static void
tiger_parse_file (const char *filename)
{
  /* FIXME: handle stdin "-"*/
  FILE *file = fopen (filename, "r");
  if (file == NULL)
    {
      fatal_error (UNKNOWN_LOCATION, "cannot open filename %s: %m", filename);
    }

  Tiger::Lexer lexer (filename, file);
  Tiger::Parser parser (lexer);

  parser.parse_program ();

  fclose (file);
}
