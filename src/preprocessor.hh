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
                 const std::list<std::string>& header_paths);
    Preprocessor(const Preprocessor&) = delete;
    Preprocessor(Preprocessor&&) = delete;
    Preprocessor& operator=(const Preprocessor&) = delete;
    Preprocessor& operator=(Preprocessor&&) = delete;
    virtual ~Preprocessor();

    void Preprocess();

private:
    class Macro;
    class PPConditions;

    // The adding action will always be performed unless the new definition is
    // effectively the same as the old one.
    // Return true if it is a redefinition. Note that if the new definition is
    // effectively the same, it will return false.
    bool AddMacro(const Macro& macro);
    void RmMacro(const std::string& ident);
    bool HasMacro(const std::string& ident) const {
        return macros_.find(ident) != macros_.cend(); }
    const Macro* GetMacro(const std::string& ident) const;
    void AddHeaderPath(const std::string& path) {
        header_paths_.push_back(path); }
    // Return the path of the found file and return an empty string if not found.
    std::string FindHeader(const std::string& fname,
                           bool include_cur_path,
                           const std::string& cur_path);
    void AddInitMacros();
    void AddInitHeaderPaths();

    TokenSequence& ts_;
    std::map<std::string, std::string>& files_;
    std::list<std::string> header_paths_;
    // Due to the incomplete type of Macro class, this map can neither be
    // specialized with Macro class nor be initialized using member initializer.
    std::map<std::string, std::unique_ptr<Macro>> macros_;
    std::unique_ptr<PPConditions> conditionsp_;
};

}
#endif
