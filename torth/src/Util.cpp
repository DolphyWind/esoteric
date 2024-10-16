#include <Util.hpp>
#include <algorithm>
#include <format>
#include <stdexcept>
#include <unordered_map>

int Util::ternary_to_number(std::string ternary)
{
    bool isSign = (ternary[0] == '-') || (ternary[0] == '+');
    bool isNegative = (ternary[0] == '-');

    if(isSign)
    {
        ternary = std::string(ternary.begin() + 1, ternary.end());
    }

    value_t mul = 1;
    value_t finalNumber = 0;
    for(auto rit = ternary.rbegin(); rit != ternary.rend(); ++rit)
    {
        if(!Util::isTernaryDigit(*rit))
        {
            throw std::runtime_error(std::format("Unrecognized ternary digit \"{}\"", *rit));
        }
        finalNumber += mul * (*rit - '0');
        mul *= 3;
    }
    if(isNegative) finalNumber *= -1;
    return finalNumber;
}

std::string Util::number_to_ternary(int num)
{
    if(num == 0) return "0";

    std::string finalStr;
    bool isNegative = false;
    if(num < 0)
    {
        isNegative = true;
        num *= -1;
    }

    while(num)
    {
        int rem = num % 3;
        finalStr.push_back('0'+rem);
        num /= 3;
    }

    std::reverse(finalStr.begin(), finalStr.end());
    if(isNegative)
    {
        finalStr = "-" + finalStr;
    }

    return finalStr;
}


bool Util::isTernaryDigit(char c)
{
    return (c == '0' || c == '1' || c == '2'); 
}

std::string Util::ternary_to_olus(const std::string& ternary)
{
    std::unordered_map<char, char> d = {
        {'0', '0'},
        {'1', 'O'},
        {'2', 'o'},
    };

    int pos = ternary.size() - 3;
    std::string out = "";

    while(pos >= 0)
    {
        std::string slice = ternary.substr(pos, 3);
        std::string tempStr = "";

        tempStr += "olus2";
        tempStr.push_back(d[slice[0]]);
        tempStr.push_back(d[slice[1]]);
        tempStr.push_back(d[slice[2]]);

        if(out.empty())
        {
            out = tempStr + out;
        }
        else 
        {
            out = tempStr + " " + out;
        }

        pos -= 3;
    }

    if(pos != -3)
    {
        std::string slice = ternary.substr(0, pos + 3);
        slice = std::string(-pos, '0') + slice;
        std::string tempStr = "";

        tempStr += "olus2";
        tempStr.push_back(d[slice[0]]);
        tempStr.push_back(d[slice[1]]);
        tempStr.push_back(d[slice[2]]);

        if(out.empty())
        {
            out = tempStr + out;
        }
        else 
        {
            out = tempStr + " " + out;
        }
    }

    return out;
}

