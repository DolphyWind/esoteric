#include "Lexer.hpp"
#include "Parser.hpp"
#include "Statement.hpp"
#include "Util.hpp"
#include <Interpreter.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/algorithm/string/split.hpp>
#include <cmath>
#include <sstream>
#include <stdexcept>
#include <string>
#include <iostream>
#include <vector>
#include <format>
#include <Statement.hpp>

Interpreter::Interpreter()
{
    addWord(".", [this](){
        value_t top = popStack();
        std::cout << Util::number_to_ternary(top) << std::flush;
    });
    addWord(",", [this](){
        value_t num;
        std::string line;

        std::getline(std::cin, line);
        num = Util::ternary_to_number(line);
        pushStack(num);
    });
    addWord("+", [this](){
        pushStack(popStack() + popStack());
    });
    addWord("-", [this](){
        value_t first = popStack();
        value_t second = popStack();
        pushStack(second - first);
    });
    addWord("*", [this](){
        pushStack(popStack() * popStack());
    });
    addWord("/", [this](){
        value_t first = popStack();
        value_t second = popStack();
        pushStack(second / first);
    });
    addWord("%", [this](){
        value_t first = popStack();
        value_t second = popStack();
        pushStack(second % first);
    });
    addWord("REVERSE", [this](){
        std::vector<value_t> other_stack;
        while(!m_stack.empty())
        {
            other_stack.push_back(popStack());
        }
        m_stack = std::move(other_stack);
    });
    addWord("<", [this](){
        value_t first = popStack();
        value_t second = popStack();
        pushStack(second < first);
    });
    addWord("<=", [this](){
        value_t first = popStack();
        value_t second = popStack();
        pushStack(second <= first);
    });
    addWord("==", [this](){
        value_t first = popStack();
        value_t second = popStack();
        pushStack(second == first);
    });
    addWord("!=", [this](){
        value_t first = popStack();
        value_t second = popStack();
        pushStack(second != first);
    });
    addWord(">=", [this](){
        value_t first = popStack();
        value_t second = popStack();
        pushStack(second >= first);
    });
    addWord(">", [this](){
        value_t first = popStack();
        value_t second = popStack();
        pushStack(second > first);
    });
    addWord("DUP", [this](){
        value_t top = popStack();
        pushStack(top);
        pushStack(top);
    });
    addWord("SWAP", [this](){
        value_t first = popStack();
        value_t second = popStack();
        pushStack(first);
        pushStack(second);
    });
    addWord("DROP", [this](){
        popStack();
    });
    addWord("ROT", [this](){
        value_t A = popStack();
        value_t B = popStack();
        value_t C = popStack();

        pushStack(A);
        pushStack(C);
        pushStack(B);
    });
    addWord("DEPTH", [this](){
        pushStack(m_stack.size());
    });

    addWord("GET", [this](){
        value_t index = popStack();
        value_t value = getStack(index);
        pushStack(value);
    });

    addWord("SET", [this](){
        value_t value = popStack();
        value_t index = popStack();
        setStack(index, value);
    });

    m_olusWords = {
        {".", "olus2000"},
        {",", "olus200O"},
        {"+", "olus200o"},
        {"-", "olus20O0"},
        {"*", "olus20OO"},
        {"/", "olus20Oo"},
        {"%", "olus20o0"},
        {"<", "olus20oO"},
        {"<=", "olus20oo"},
        {"==", "olus2O00"},
        {"!=", "olus2O0O"},
        {">=", "olus2O0o"},
        {">", "olus2OO0"},
        {"REVERSE", "olus2OOO"},
        {"DUP", "olus2OOo"},
        {"SWAP", "olus2Oo0"},
        {"DROP", "olus2OoO"},
        {"ROT", "olus2Ooo"},
        {"DEPTH", "olus2o00"},
        {"IF", "olus2o0O"},
        {"ELSE", "olus2o0o"},
        {"THEN", "olus2oO0"},
        {"WHILE", "olus2oOO"},
        {":", "olus2oOo"},
        {";", "olus2oo0"},
        {"GET", "olus2ooO"},
        {"SET", "olus2ooo"},
    };
}

value_t Interpreter::popStack()
{
    if(m_stack.empty())
    {
        throw std::runtime_error("Stack is empty!");
    }
    value_t top = m_stack[m_stack.size() - 1];
    m_stack.pop_back();
    return top;
}

void Interpreter::pushStack(value_t value)
{
    m_stack.push_back(value);
}

value_t Interpreter::getStack(value_t index)
{
    value_t val;
    try
    {
        val = m_stack.at(index);
        m_stack.erase(m_stack.begin() + index);
    }
    catch (const std::out_of_range& e)
    {
        throw std::runtime_error(std::format("Invalid index: {}", index));
    }
    return val;
}

void Interpreter::setStack(value_t index, value_t value)
{
    if(index < 0 || index > m_stack.size())
    {
        throw std::runtime_error(std::format("Invalid index: {}", index));
    }
    m_stack.insert(m_stack.begin() + index, value);
}

void Interpreter::executeWord(const std::string& word)
{
    if(!m_words.contains(word))
    {
        throw std::runtime_error(std::format("Undefined word: {}", word));
    }
    m_words[word]();
}

void Interpreter::addWord(const std::string& name, const std::function<void()>& body)
{
    m_words[name] = body;
}

const Interpreter::olusdict_t& Interpreter::getOlusWords() const
{
    return m_olusWords;
}

Stmt::Block Interpreter::parse(const std::string& code) const
{
    Lexer lexer(code);
    std::vector<Token> tokens = lexer.scanTokens();

    Parser parser(tokens);
    return Stmt::Block(parser.parse());
}

void Interpreter::runPrompt()
{
    std::string line;

    while(true)
    {
        std::cout << ">>> " << std::flush;
        if(!std::getline(std::cin, line))
        {
            break;
        }
        run(line);
    }
}

void Interpreter::olusifyPrompt()
{
    std::string line;
    bool hadError = false;

    while(true)
    {
        try
        {
            std::cout << ">>> " << std::flush;
            if(!std::getline(std::cin, line))
            {
                break;
            }

            std::cout << olusify(line) << std::endl;
        }
        catch(const std::runtime_error& e)
        {
            std::cerr << "Error: " << e.what() << std::endl;
            hadError = true;
        }
    }
}

bool Interpreter::run(const std::string& code)
{
    try
    {
        execute(parse(code));
    }
    catch(const std::runtime_error& e)
    {
        std::cerr << "Error: " << e.what() << std::endl;
        return false;
    }
    return true;
}

std::string Interpreter::olusify(const std::string& code) const
{
    return parse(code).olusify(*this);
}

void Interpreter::execute(const Stmt::Block& block)
{
    block.run(*this, m_stack);
}
