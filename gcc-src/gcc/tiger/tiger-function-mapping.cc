#include <utility>
#include <sstream>

#include "tiger-function-mapping.h"

#include "config.h"
#include "system.h"
#include <list>
#include <string>
#include <tr1/memory>

using namespace std;

namespace Tiger
{

void
FunctionMapping::insert (FunctionPtr s) {
  gcc_assert (s != NULL);
  std::pair<Map::iterator, bool> p
    = map.insert (std::make_pair (s->get_name (), s));

  gcc_assert (p.second);
}

void
FunctionMapping::remove (FunctionPtr s) {
  map.erase (s->get_name ());
}

FunctionPtr
FunctionMapping::get (const std::string &str) const
{
  Map::const_iterator it = map.find (str);
  if (it != map.end ()) {
      return it->second;
    }
  return FunctionPtr();
}

}

