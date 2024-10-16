#ifndef __TOKEN_HPP__
#define __TOKEN_HPP__
#include "TokenType.hpp"
#include <string>

struct Token
{
    TokenType tokenType;
    std::string lexeme;

    Token(TokenType tokenType, const std::string& lexeme);
};

#endif // !__TOKEN_HPP__
