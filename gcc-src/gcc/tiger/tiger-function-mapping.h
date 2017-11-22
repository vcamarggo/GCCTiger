#ifndef TIGER_FUNCTION_MAPPING_H
#define TIGER_FUNCTION_MAPPING_H

#include "tiger/tiger-function.h"
#include <tr1/memory>
#include <map>
#include <list>
#include <string>

using namespace std;

namespace Tiger
{

struct FunctionMapping
{
public:

  void insert (FunctionPtr s);
  void remove (FunctionPtr s);
  FunctionPtr get (const std::string &str) const;

private:

  typedef std::map<std::string, FunctionPtr > Map;
  Map map;
};

}

#endif // TIGER_FUNCTION_MAPPING_H
