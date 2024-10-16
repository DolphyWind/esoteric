#ifndef __STATEMENT_HPP__
#define __STATEMENT_HPP__

#include "Token.hpp"
#include <boost/algorithm/string/replace.hpp>
#include <stack>
#include <string>
#include <vector>
#include <memory>
#include <boost/algorithm/string.hpp>
#include <Common.hpp>

class Interpreter;

namespace Stmt
{

struct Stmt
{
    Stmt() {}
    virtual void run(Interpreter& interpreter, std::vector<value_t>& memory) const = 0;
    virtual std::string olusify(const Interpreter& interpreter) const = 0;

    virtual ~Stmt() {}
};

typedef std::vector<std::shared_ptr<Stmt>> block_t;

struct Block : public Stmt, public block_t
{
    explicit Block(const block_t& _block);
    
    virtual void run(Interpreter& interpreter, std::vector<value_t>& memory) const override final;
    virtual std::string olusify(const Interpreter& interpreter) const override final;
};

struct If : public Stmt
{
    Block ifBlock;
    Block elseBlock;

    If(const Block& _ifBlock, const Block& _elseBlock);

    virtual void run(Interpreter& interpreter, std::vector<value_t>& memory) const override final;
    virtual std::string olusify(const Interpreter& interpreter) const override final;
};

struct While : public Stmt
{
    Block block;

    explicit While(const block_t& _block);

    virtual void run(Interpreter& interpreter, std::vector<value_t>& memory) const override final;
    virtual std::string olusify(const Interpreter& interpreter) const override final;
};

struct Number : public Stmt
{
    value_t number;
    explicit Number(value_t num);

    virtual void run(Interpreter& interpreter, std::vector<value_t>& memory) const override final;
    virtual std::string olusify(const Interpreter& interpreter) const override final;
};

struct Word : public Stmt
{
    Token word;
    explicit Word(const Token& _word);

    virtual void run(Interpreter& interpreter, std::vector<value_t>& memory) const override final;
    virtual std::string olusify(const Interpreter& interpreter) const override final;
};

struct String : public Stmt
{
    Token string;

    explicit String(const Token& _string);

    virtual void run(Interpreter& interpreter, std::vector<value_t>& memory) const override final;
    virtual std::string olusify(const Interpreter& interpreter) const override final;
};

struct Colon : public Stmt
{
    Token word;
    Block statements;

    Colon(const Token& _word, const block_t& _statements);

    virtual void run(Interpreter& interpreter, std::vector<value_t>& memory) const override final;
    virtual std::string olusify(const Interpreter& interpreter) const override final;
};

}
#endif // !__STATEMENT_HPP__
