#ifndef _AXCC_ENCODING_HH_
#define _AXCC_ENCODING_HH_

#include <locale>
#include <codecvt>

namespace axcc {

enum class EncKind {
    kUtf8,
    kUtf16,
    kUtf32
};

inline std::u16string Utf8ToUtf16(const std::string& str) {
    std::wstring_convert<
        std::codecvt_utf8_utf16<char16_t>, char16_t> cvt_utf8_utf16;
    return cvt_utf8_utf16.from_bytes(str);
}

inline std::u32string Utf8ToUtf32(const std::string& str) {
    std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> cvt_utf8_utf32;
    return cvt_utf8_utf32.from_bytes(str);
}

inline std::string Ucs4ToUtf8(unsigned int ucn) {
    std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> cvt_ucs4_utf8;
    return cvt_ucs4_utf8.to_bytes(static_cast<char32_t>(ucn));
}

inline std::u16string Ucs4ToUtf16(unsigned int ucn) {
    return Utf8ToUtf16(Ucs4ToUtf8(ucn));
}

inline std::u32string Ucs4ToUtf32(unsigned int ucn) {
    return Utf8ToUtf32(Ucs4ToUtf8(ucn));
}

}
#endif
