#include "Statement.hpp"
#include "TokenType.hpp"
#include "Util.hpp"
#include <Parser.hpp>
#include <format>
#include <memory>
#include <stdexcept>
#include <string>

Token Parser::EOF_TOKEN{TokenType::EOF_TOKEN, ""};

Parser::Parser(const std::vector<Token>& tokens):
    m_tokens(tokens)
{}

std::vector<std::shared_ptr<Stmt::Stmt>> Parser::parse()
{
    std::vector<std::shared_ptr<Stmt::Stmt>> statements;

    while(!isAtEnd())
    {
        auto stmt = parseStmt();
        if(!stmt) break;

        statements.push_back(stmt);
    }

    return statements;
}

std::shared_ptr<Stmt::Stmt> Parser::parseStmt()
{
    if(!isAtEnd())
    {
        Token& token = advance();
        switch (token.tokenType)
        {
            case TokenType::WORD:
                return std::make_shared<Stmt::Word>(token);
                break;
            case TokenType::TERNARY_NUMBER:
                return parseNumber();
                break;
            case TokenType::STRING:
                return std::make_shared<Stmt::String>(token);
                break;
            case TokenType::IF:
                return parseIf();
                break;
            case TokenType::WHILE:
                return parseWhile();
                break;
            case TokenType::COLON:
                return parseColon();
                break;
            case TokenType::EOF_TOKEN:
                break;
            default:
                break;
        }
    }

    return nullptr;
}

std::shared_ptr<Stmt::If> Parser::parseIf()
{
    Stmt::block_t thenBlock;
    Stmt::block_t elseBlock;

    while(peek().tokenType != TokenType::THEN && peek().tokenType != TokenType::ELSE && !isAtEnd())
    {
        auto ptr = parseStmt();
        if(ptr) thenBlock.push_back(ptr);
    }

    if(isAtEnd()) throw std::runtime_error("Unterminated if statement!");

    if(peek().tokenType == TokenType::ELSE)
    {
        while(peek().tokenType != TokenType::THEN && !isAtEnd())
        {
            auto ptr = parseStmt();
            if(ptr) elseBlock.push_back(ptr);
        }
        if(isAtEnd()) throw std::runtime_error("Unterminated if statement!");
    }
    consume(TokenType::THEN, "Unterminated if statement!");

    return std::make_shared<Stmt::If>(
        Stmt::Block( std::move(thenBlock) ),
        Stmt::Block( std::move(elseBlock) )
    );
}

std::shared_ptr<Stmt::While> Parser::parseWhile()
{
    Stmt::block_t block;
    while(peek().tokenType != TokenType::THEN && !isAtEnd())
    {
        block.push_back(parseStmt());
    }
    if(isAtEnd()) throw std::runtime_error("Unterminated while statement!");
    consume(TokenType::THEN, "Unterminated while statement!");

    return std::make_shared<Stmt::While>(std::move(block));
}

std::shared_ptr<Stmt::Number> Parser::parseNumber()
{
    Token& numToken = prev();
    return std::make_shared<Stmt::Number>(Util::ternary_to_number(numToken.lexeme));
}

std::shared_ptr<Stmt::Colon> Parser::parseColon()
{
    Token word = advance();
    if(word.tokenType != TokenType::WORD) throw std::runtime_error(std::format("Not a word: \"{}\"", word.lexeme));
    
    Stmt::block_t statements;
    while(peek().tokenType != TokenType::SEMICOLON && !isAtEnd())
    {
        if(peek().tokenType == TokenType::COLON) throw std::runtime_error("Cannot nest colon statements!");
        statements.push_back(parseStmt());
    }
    consume(TokenType::SEMICOLON, "Unterminated colon statement!");

    return std::make_shared<Stmt::Colon>(word, Stmt::Block(std::move(statements)) );
}

bool Parser::isAtEnd() const
{
    return current >= m_tokens.size();
}

Token& Parser::peek()
{
    if(isAtEnd()) return Parser::EOF_TOKEN;
    return m_tokens[current];
}

Token& Parser::prev()
{
    if(current == 0) return EOF_TOKEN;
    if(isAtEnd()) return m_tokens[m_tokens.size() - 1];
    return m_tokens[current - 1];
}

Token& Parser::advance()
{
    Token& currentToken = peek();
    current ++;
    return currentToken;
}

void Parser::consume(TokenType tokenType, const std::string& message)
{
    if(advance().tokenType != tokenType) throw std::runtime_error(message);
}
