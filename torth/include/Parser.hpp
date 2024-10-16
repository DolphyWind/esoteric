#ifndef __PARSER_HPP__
#define __PARSER_HPP__

#include "Token.hpp"
#include "TokenType.hpp"
#include <memory>
#include <Statement.hpp>
#include <vector>


class Parser
{
public:
    Parser(const std::vector<Token>& source);

    std::vector<std::shared_ptr<Stmt::Stmt>> parse();
private:
    std::shared_ptr<Stmt::Stmt> parseStmt();
    std::shared_ptr<Stmt::If> parseIf();
    std::shared_ptr<Stmt::While> parseWhile();
    std::shared_ptr<Stmt::Number> parseNumber();
    std::shared_ptr<Stmt::Colon> parseColon();
    bool isAtEnd() const;
    Token& peek();
    Token& prev();
    Token& advance();
    void consume(TokenType tokenType, const std::string& message);

    std::vector<Token> m_tokens;
    int current = 0;
    static Token EOF_TOKEN;
};

#endif // !__PARSER_HPP__
