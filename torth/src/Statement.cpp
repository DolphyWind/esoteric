#include <boost/algorithm/string/replace.hpp>
#include <iostream>

#include <Interpreter.hpp>
#include <Statement.hpp>
#include <Util.hpp>

namespace Stmt
{

Block::Block(const block_t& _block):
    block_t(_block)
{}

void Block::run(Interpreter& interpreter, std::vector<value_t>& memory) const
{
    for(const auto& stmt : *this)
    {
        stmt->run(interpreter, memory);
    }
}

std::string Block::olusify(const Interpreter& interpreter) const
{
    std::string out;
    for(const auto& stmt : *this)
    {
        out += stmt->olusify(interpreter);
    }
    return out;
}

/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////

If::If(const Block& _ifBlock, const Block& _elseBlock):
    ifBlock(_ifBlock), elseBlock(_elseBlock)
{}

void If::run(Interpreter& interpreter, std::vector<value_t>& memory) const
{
    value_t top = interpreter.popStack();
    interpreter.pushStack(top);

    if(top != 0)
    {
        ifBlock.run(interpreter, memory);
    }
    else
    {
        elseBlock.run(interpreter, memory);
    }
}

std::string If::olusify(const Interpreter& interpreter) const
{
    if(elseBlock.size() != 0)
    {
        return std::format("olus2o0O {}olus2o0o {}olus2oO0 ", ifBlock.olusify(interpreter), elseBlock.olusify(interpreter));
    }
    return std::format("olus2o0O {}olus2oO0 ", ifBlock.olusify(interpreter));
}

/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////

While::While(const block_t& _block):
    block(_block)
{}

void While::run(Interpreter& interpreter, std::vector<value_t>& memory) const
{
    value_t top = interpreter.popStack();
    interpreter.pushStack(top);

    while(top != 0)
    {
        block.run(interpreter, memory);
        
        top = interpreter.popStack();
        interpreter.pushStack(top);
    }
}

std::string While::olusify(const Interpreter& interpreter) const
{
    return std::format("olus2oOO {}olus2oO0 ", block.olusify(interpreter));
}

/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////

Number::Number(value_t num):
    number(num)
{}

void Number::run(Interpreter& interpreter, std::vector<value_t>& memory) const
{
    interpreter.pushStack(number);
}

std::string Number::olusify(const Interpreter& interpreter) const
{
    std::string ternary = Util::number_to_ternary(number);
    bool isNegative = ternary.starts_with('-');
    std::string exclText = "0lus2000!";

    if(isNegative)
    {
        ternary = ternary.substr(1, ternary.size() - 1);
        exclText = "0lus2ooo!";
    }

    return std::format("{} {} olus2000! ", exclText, Util::ternary_to_olus(ternary));
}

/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////

Word::Word(const Token& _word):
    word(_word)
{}

void Word::run(Interpreter& interpreter, std::vector<value_t>& memory) const
{
    interpreter.executeWord(word.lexeme);
}

std::string Word::olusify(const Interpreter& interpreter) const
{
    if(interpreter.getOlusWords().contains(word.lexeme))
    {
        return interpreter.getOlusWords().at(word.lexeme) + " ";
    }
    return word.lexeme + " ";
}

/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////

String::String(const Token& _string):
    string(_string)
{}

void String::run(Interpreter& interpreter, std::vector<value_t>& memory) const
{
    std::cout << string.lexeme;
}

std::string String::olusify(const Interpreter& interpreter) const
{
    namespace ba = boost::algorithm;
    std::string str = string.lexeme;
    ba::replace_all(str, "\\", "\\\\");
    ba::replace_all(str, "\"", "\\\"");
    ba::replace_all(str, "\n", "\\n");
    ba::replace_all(str, "\r", "\\r");
    ba::replace_all(str, "\t", "\\t");
    ba::replace_all(str, "\'", "\\\'");

    return "\"" + str + "\" ";
}

/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////

Colon::Colon(const Token& _word, const block_t& _statements):
    word(_word), statements(_statements)
{}

void Colon::run(Interpreter& interpreter, std::vector<value_t>& memory) const
{
    auto& stmts = statements;
    interpreter.addWord(word.lexeme, [stmts, &interpreter, &memory](){
        stmts.run(interpreter, memory);
    });
}

std::string Colon::olusify(const Interpreter& interpreter) const
{
    return std::format("olus2oOo {} {}olus2oo0 ", word.lexeme, statements.olusify(interpreter));
}

}
