#ifndef _AXCC_ERROR_HH_
#define _AXCC_ERROR_HH_

#include <iostream>
#include <string>

#include "token.hh"

#define COLOR_RED    "\033[31m"
#define COLOR_YELLOW "\033[33m"
#define COLOR_RESET  "\033[0m"

namespace axcc {

inline void Error(const std::string& msg) {
    std::cerr << COLOR_RED << "Error: " << COLOR_RESET << msg << std::endl;
}

inline void Warning(const std::string& msg) {
    std::cerr << COLOR_YELLOW << "Warning: " << COLOR_RESET << msg << std::endl;
}

inline void Error(const std::string& msg, const SourceLocation& loc) {
    std::cerr << COLOR_RED << "Error: " << COLOR_RESET << msg << '\n';
    std::cerr << LocStr(loc) << std::endl;
}

inline void Warning(const std::string& msg, const SourceLocation& loc) {
    std::cerr << COLOR_YELLOW << "Warning: " << COLOR_RESET << msg << '\n';
    std::cerr << LocStr(loc) << std::endl;
}

}
#endif
