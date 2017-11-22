#ifndef TIGER_FUNCTION_H
#define TIGER_FUNCTION_H

#include "tiger/tiger-tree.h"

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"

#include <string>
#include <vector>
#include <tr1/memory>

namespace Tiger
{

enum /* class */ FunctionKind
{
  FUNCTION
};

struct Function
{
public:
  Function (FunctionKind kind, const std::string &name_, tree return_type_node_, std::vector<tree> params_type_node_) : kind(kind), name (name_), return_type_node(return_type_node_), params_type_node(params_type_node_), decl (error_mark_node)
  {
    gcc_assert (name.size () > 0);
  }

  FunctionKind get_kind () const
  {
    return kind;
  }

  std::string get_name () const
  {
    return name;
  }

  int get_nr_args () const
  {
    return params_type_node.size ();
  }

  tree get_param (int i) const
  {
    return params_type_node[i];
  }

  std::vector<tree> get_params_type_node () const
  {
    return params_type_node;
  }

  tree get_return_type_node () const
  {
    return return_type_node;
  }

  void set_tree_decl (Tree decl_)
  {
    gcc_assert (kind == FUNCTION && decl_.get_tree_code() == FUNCTION_DECL);
    decl = decl_;
  }

  Tree get_tree_decl () const
  {
    return decl;
  }

private:
  FunctionKind kind;
  std::string name;
  tree return_type_node;
  std::vector<tree>  params_type_node;
  Tree decl;
};

typedef std::tr1::shared_ptr<Function> FunctionPtr;
typedef std::tr1::shared_ptr<const Function> const_FunctionPtr;

}

#endif // TIGER_FUNCTION_H
