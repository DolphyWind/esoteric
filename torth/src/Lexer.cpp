#include "Token.hpp"
#include "TokenType.hpp"
#include <Lexer.hpp>
#include <boost/algorithm/string/classification.hpp>
#include <cctype>
#include <format>
#include <stdexcept>
#include <vector>
#include <Util.hpp>

Lexer::Lexer(const std::string& source):
    m_source(source)
{
    m_keywords["IF"] = TokenType::IF;
    m_keywords["ELSE"] = TokenType::ELSE;
    m_keywords["THEN"] = TokenType::THEN;
    m_keywords["WHILE"] = TokenType::WHILE;
}

std::vector<Token> Lexer::scanTokens()
{
    while(!isAtEnd())
    {
        scanToken();
    }
    
    m_tokens.emplace_back(TokenType::EOF_TOKEN, "");
    return m_tokens;
}

bool Lexer::isAtEnd() const
{
    return current >= m_source.length();
}

void Lexer::scanToken()
{
    char current = advance();

    switch (current)
    {
        case '.':
        case ',':
        case '+':
        case '*':
        case '/':
        case '%':
        case '^':
            m_tokens.emplace_back(TokenType::WORD, std::string(1, current));
            break;
        case ':':
            m_tokens.emplace_back(TokenType::COLON, ":");
            break;
        case ';':
            m_tokens.emplace_back(TokenType::SEMICOLON, ";");
            break;
        case '-':
        {
            if(Util::isTernaryDigit(peek()))
            {
                ternary(current);
                break;
            }
            m_tokens.emplace_back(TokenType::WORD, "-");
            break;
        }
        case '<':
        {
            if(match('='))
            {
                m_tokens.emplace_back(TokenType::WORD, "<=");
                break;
            }
            m_tokens.emplace_back(TokenType::WORD, "<");
            break;
        }
        case '=':
        {
            if(!match('=')) throw std::runtime_error("Invalid character '='");

            m_tokens.emplace_back(TokenType::WORD, "==");
            break;
        }
        case '!':
        {
            if(!match('=')) throw std::runtime_error("Invalid character'!'");

            m_tokens.emplace_back(TokenType::WORD, "!=");
            break;
        }
        case '>':
        {
            if(match('='))
            {
                m_tokens.emplace_back(TokenType::WORD, ">=");
                break;
            }
            m_tokens.emplace_back(TokenType::WORD, ">");
            break;
        }
        case '\"':
        {
            std::string str;
            while(peek() != '\"' && !isAtEnd())
            {
                char c = advance();
                if(c == '\n')
                {
                    throw std::runtime_error("Multi-line strings are disallowed!");
                }
                if(c == '\\')
                {
                    c = advance();
                    if(c == '\"') str.push_back('\"');
                    if(c == 'n') str.push_back('\n');
                    if(c == 'r') str.push_back('\r');
                    if(c == 't') str.push_back('\t');
                    if(c == '\\') str.push_back('\\');
                    if(c == '\'') str.push_back('\'');
                }
                else
                {
                    str.push_back(c);
                }
            }

            if(isAtEnd())
            {
                throw std::runtime_error("Unclosed string!");
            }
            advance();
            m_tokens.emplace_back(TokenType::STRING, str);
            break;
        }
        case '(':
        {
            while(peek() != ')')
            {
                advance();
            }
            advance();
            break;
        }
        case ' ':
        case '\t':
        case '\n':
            return;
        default:
        {
            if(Util::isTernaryDigit(current))
            {
                ternary(current);
            }
            else if(std::isalpha(current) || current == '_')
            {
                keyword(current);
            }
            else throw std::runtime_error(std::format("Unrecognized character '{}'", current));
        }
    }
    char next = peek();
    if(!isAtEnd() && !isSpace(next)) throw std::runtime_error(std::format("No space before \"{}\"", next));
    advance();
}

char Lexer::advance()
{
    return m_source[current++];
}

bool Lexer::match(char c)
{
    if(isAtEnd()) return false;
    if(peek() != c) return false;

    current++;
    return true;
}

char Lexer::peek()
{
    if(isAtEnd()) return '\0';
    return m_source[current];
}

char Lexer::next()
{
    if(current + 1 >= m_source.length()) return '\0';
    return m_source[current + 1];
}

void Lexer::ternary(char current)
{
    bool isSign = (current == '-' || current == '+');

    std::string numStr;
    numStr.push_back(current);

    while(Util::isTernaryDigit(peek()))
    {
        numStr.push_back(advance());
    }

    if(!isSpace(peek())) throw std::runtime_error(std::format("Unrecognized ternary digit \"{}\"", peek()));
    m_tokens.emplace_back(TokenType::TERNARY_NUMBER, numStr);
}

void Lexer::keyword(char current)
{
    std::string word;
    word.push_back(current);
    while(isAllowedInKeyword(peek()))
    {
        word.push_back(advance());
    }

    TokenType tokenType = TokenType::WORD;

    if(m_keywords.contains(word))
    {
        tokenType = m_keywords[word];
    }

    m_tokens.emplace_back(tokenType, word);
}

bool Lexer::isAllowedInKeyword(char c)
{
    return std::isalnum(c) || (c == '_') || (c == '-');
}

bool Lexer::isSpace(char c)
{
    return boost::algorithm::is_any_of(" \n\t")(c) || c == '\0';
}

