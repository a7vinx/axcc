#ifndef _AXCC_PREPROCESSER_HH_
#define _AXCC_PREPROCESSER_HH_

#include <string>
#include <vector>
#include <list>
#include <map>
#include <memory>

#include "token.hh"

namespace axcc {

class Preprocessor {
public:
    Preprocessor(TokenSequence& ts,
                 std::map<std::string, std::string>& files,
                 const std::vector<std::string>& header_paths);
    Preprocessor(const Preprocessor&) = delete;
    Preprocessor(Preprocessor&&) = delete;
    Preprocessor& operator=(const Preprocessor&) = delete;
    Preprocessor& operator=(Preprocessor&&) = delete;
    virtual ~Preprocessor();

    void Preprocess();

private:
    class Macro;
    class PPConditions;

    // Return false if the identifier of the macro waiting to be added has
    // already been added before, while the adding action will still be
    // performed.
    bool AddMacro(const Macro& macro);
    void RmMacro(const std::string& ident);
    void AddHeaderPath(const std::string& path) {
        header_paths_.push_back(path); }
    void AddInitMacros();
    void AddInitHeaderPaths();

    TokenSequence& ts_;
    std::map<std::string, std::string>& files_;
    std::vector<std::string> header_paths_;
    // Due to the incomplete type of Macro class, this map can neither be
    // specialized with Macro class nor be initialized using member initializer.
    std::map<std::string, std::unique_ptr<Macro>> macros_;
    std::unique_ptr<PPConditions> conditions_{};
};

}
#endif
