#ifndef __TOKEN_TYPE_HPP__
#define __TOKEN_TYPE_HPP__

enum class TokenType
{
    IF, ELSE, THEN, WHILE, WORD,
    COLON, SEMICOLON,

    TERNARY_NUMBER, STRING, EOF_TOKEN
};

#endif // !__TOKEN_TYPE_HPP__
