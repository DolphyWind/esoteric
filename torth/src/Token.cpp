#include "Token.hpp"
#include "TokenType.hpp"

Token::Token(TokenType _tokenType, const std::string& _lexeme):
    tokenType(_tokenType), lexeme(_lexeme)
{}
