#include <utility>
#include <sstream>

#include "tiger-symbol-mapping.h"

#include "config.h"
#include "system.h"

namespace Tiger
{

void
SymbolMapping::insert (SymbolPtr s)
{
  gcc_assert (s != NULL);
  std::pair<Map::iterator, bool> p
    = map.insert (std::make_pair (s->get_name (), s));

  gcc_assert (p.second);
}

void
SymbolMapping::remove (SymbolPtr s)
{
  map.erase (s->get_name ());
}

SymbolPtr
SymbolMapping::get (const std::string &str) const
{
  Map::const_iterator it = map.find (str);
  if (it != map.end ())
    {
      return it->second;
    }
  return SymbolPtr();
}

}

