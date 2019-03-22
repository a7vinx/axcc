#ifndef _AXCC_ERROR_HH_
#define _AXCC_ERROR_HH_

#include <string>
#include <cstdlib>
#include "token.hh"

namespace axcc {

void Error(const std::string& msg);
void Error(const std::string& msg, const SourceLoc& loc);

void Warning(const std::string& msg);
void Warning(const std::string& msg, const SourceLoc& loc);

void TurnOnColorOutput();
void TurnOffColorOutput();

std::size_t ErrorCount();

}
#endif
