#include "Interpreter.hpp"
#include <boost/program_options.hpp>
#include <boost/program_options/detail/parsers.hpp>
#include <boost/program_options/options_description.hpp>
#include <boost/program_options/parsers.hpp>
#include <boost/program_options/positional_options.hpp>
#include <boost/program_options/variables_map.hpp>
#include <boost/type_traits/has_plus_assign.hpp>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>

namespace po = boost::program_options;

int main(int argc, char** argv)
{
    Interpreter interpreter;

    po::options_description desc("Torth interpreter, by DolphyWind");
    desc.add_options()
        ("help,h", "Print this message")
        ("olusify,o", "Transpile given code to olus2000 instead of running it")
        ("input,i", po::value<std::string>(), "Input file");

    po::positional_options_description p;
    p.add("input", 1);

    po::variables_map vm;
    try
    {
        po::store(po::command_line_parser(argc, argv).options(desc).positional(p).run(), vm);
        po::notify(vm);
    }
    catch (const po::error& e)
    {
        std::cerr << e.what() << std::endl;
        return EXIT_FAILURE;
    }

    if(vm.count("help"))
    {
        std::cout << desc << std::endl;
        return 0;
    }

    bool olusify = vm.count("olusify");
    bool interactive = !vm.count("input");
    
    if(interactive)
    {
        if(olusify)
        {
            interpreter.olusifyPrompt();
        }
        else
        {
            interpreter.runPrompt();
        }
        return 0;
    }

    std::string filename = vm["input"].as<std::string>();

    std::ifstream ifs(filename);
    if(!ifs)
    {
        std::cerr << "Unable to open file \"" << filename << "\"\n";
        return 1;
    }
    std::stringstream ss;
    ss << ifs.rdbuf();
    std::string code = ss.str();
    ifs.close();

    int ret = 0;
    if(olusify)
    {
        std::cout << interpreter.olusify(code) << std::endl;
    }
    else
    {
        ret = !interpreter.run(code);
    }
    return ret;
}
