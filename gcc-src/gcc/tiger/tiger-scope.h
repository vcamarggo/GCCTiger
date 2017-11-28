#ifndef TIGER_SCOPE_H
#define TIGER_SCOPE_H

#include "tiger-symbol-mapping.h"
#include "tiger-function-mapping.h"
#include <tr1/memory>
#include <vector>

namespace Tiger
{

struct Scope
{
public:
  SymbolMapping &
  get_current_mapping () {
    gcc_assert (!map_stack.empty ());
    return map_stack.back ();
  }

  FunctionMapping &
  get_current_function_mapping () {
    gcc_assert (!map_stack_function.empty ());
    return map_stack_function.back ();
  }


  void push_scope ();
  void pop_scope ();

  Scope ();

  SymbolPtr lookup (const std::string &str);
  FunctionPtr lookupFunction (const std::string &str);

private:
  typedef std::vector<SymbolMapping> MapStack;
  typedef std::vector<FunctionMapping> MapStackFunction;
  MapStack map_stack;
  MapStackFunction map_stack_function;
};

}

#endif // TIGER_SCOPE_H
