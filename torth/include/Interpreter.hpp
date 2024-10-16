#ifndef __INTERPRETER_HPP__
#define __INTERPRETER_HPP__

#include <fstream>
#include <functional>
#include <stack>
#include <string>
#include <unordered_map>
#include <Statement.hpp>
#include <Common.hpp>


class Interpreter
{
public:
    typedef std::unordered_map<std::string, std::function<void()>> dict_t;
    typedef std::unordered_map<std::string, std::string> olusdict_t;

    Interpreter();

    void runPrompt();
    void olusifyPrompt();
    bool run(const std::string& code);
    std::string olusify(const std::string& code) const;

    // Stack-related operations
    value_t popStack();
    void pushStack(value_t value);
    value_t getStack(value_t index);
    void setStack(value_t index, value_t value);

    void executeWord(const std::string& word);
    void addWord(const std::string& name, const std::function<void()>& body);
    const olusdict_t& getOlusWords() const;
private:
    Stmt::Block parse(const std::string& line) const;
    void execute(const Stmt::Block& block);

    dict_t m_words;
    olusdict_t m_olusWords;
    std::vector<value_t> m_stack;
};

#endif // !__INTERPRETER_HPP__
