#include "tiger-scope.h"

namespace Tiger
{

Scope::Scope ()
{
}

void
Scope::push_scope ()
{
  map_stack.push_back (SymbolMapping());
  map_stack_function.push_back (FunctionMapping());
}

void
Scope::pop_scope ()
{
  gcc_assert (!map_stack.empty());
  gcc_assert (!map_stack_function.empty());
  map_stack.pop_back ();
  map_stack_function.pop_back ();
}

SymbolPtr
Scope::lookup (const std::string &str)
{
  for (MapStack::reverse_iterator map = map_stack.rbegin ();
       map != map_stack.rend (); map++)
    {
      if (SymbolPtr sym = map->get (str))
	{
	  return sym;
	}
    }
  return SymbolPtr();
}

FunctionPtr
Scope::lookupFunction (const std::string &str)
{
  for (MapStackFunction::reverse_iterator map = map_stack_function.rbegin ();
       map != map_stack_function.rend (); map++)
    {
      if (FunctionPtr sym = map->get (str))
	{
	  return sym;
	}
    }
  return FunctionPtr();
}
}
