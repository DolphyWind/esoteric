#ifndef __UTIL_HPP__
#define __UTIL_HPP__

#include <string>
#include <Common.hpp>

namespace Util
{

value_t ternary_to_number(std::string ternary);
std::string number_to_ternary(value_t num);
bool isTernaryDigit(char c);
std::string ternary_to_olus(const std::string& ternary);

}

#endif // !__UTIL_HPP__
