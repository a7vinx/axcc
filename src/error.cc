#include <iostream>
#include "error.hh"

namespace axcc {

namespace {

std::string color_red = "\033[31m";
std::string color_yellow = "\033[33m";
std::string color_bold = "\033[1m";
std::string color_reset = "\033[0m";
std::size_t err_count = 0;

} // unnamed namespace

void Error(const std::string& msg) {
    ++err_count;
    std::cerr << color_red << "Error: ";
    std::cerr << color_reset << color_bold << msg << color_reset << std::endl;
}

void Error(const std::string& msg, const SourceLoc& loc) {
    ++err_count;
    std::cerr << color_red << "Error: ";
    std::cerr << color_reset << color_bold << msg << color_reset << '\n';
    std::cerr << LocStr(loc) << std::endl;
}

void Warning(const std::string& msg) {
    std::cerr << color_yellow << "Warning: ";
    std::cerr << color_reset << color_bold << msg << color_reset << std::endl;
}

void Warning(const std::string& msg, const SourceLoc& loc) {
    std::cerr << color_yellow << "Warning: ";
    std::cerr << color_reset << color_bold << msg << color_reset << '\n';
    std::cerr << LocStr(loc) << std::endl;
}

void TurnOnColorOutput() {
    color_red = "\033[31m";
    color_yellow = "\033[33m";
    color_bold = "\033[1m";
    color_reset = "\033[0m";
}

void TurnOffColorOutput() {
    color_red = "";
    color_yellow = "";
    color_bold = "";
    color_reset = "";
}

std::size_t ErrorCount() {
    return err_count;
}

}
