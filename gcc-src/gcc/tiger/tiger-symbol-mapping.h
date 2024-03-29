#ifndef TIGER_SYMBOL_MAPPING_H
#define TIGER_SYMBOL_MAPPING_H

#include "tiger/tiger-symbol.h"
#include <tr1/memory>
#include <map>
#include <list>
#include <string>

using namespace std;

namespace Tiger
{

struct SymbolMapping
{
public:

  void insert (SymbolPtr s);
  void remove (SymbolPtr s);
  SymbolPtr get (const std::string &str) const;

private:

  typedef std::map<std::string, SymbolPtr > Map;
  Map map;
};

}

#endif // TIGER_SYMBOL_MAPPING_H
