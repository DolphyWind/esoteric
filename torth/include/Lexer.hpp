#ifndef __LEXER_HPP__
#define __LEXER_HPP__
#include "Token.hpp"
#include "TokenType.hpp"
#include <unordered_map>
#include <vector>

class Lexer
{
public:
    Lexer(const std::string& source);

    std::vector<Token> scanTokens();
private:
    bool isAtEnd() const;
    void scanToken();
    char advance();
    bool match(char c);
    char peek();
    char next();
    void ternary(char current);
    void keyword(char current);
    bool isAllowedInKeyword(char c);
    bool isSpace(char c);

    const std::string m_source;
    std::unordered_map<std::string, TokenType> m_keywords;
    std::vector<Token> m_tokens;

    int current = 0;
};

#endif // !__LEXER_HPP__
