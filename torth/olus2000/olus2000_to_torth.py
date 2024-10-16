import sys

olus2000_dict = {
    'olus2000': '.',
    'olus200O': ',',
    'olus200o': '+',
    'olus20O0': '-',
    'olus20OO': '*',
    'olus20Oo': '/',
    'olus20o0': '%',
    'olus20oO': '<',
    'olus20oo': '<=',
    'olus2O00': '==',
    'olus2O0O': '!=',
    'olus2O0o': '>=',
    'olus2OO0': '>',
    'olus2OOO': 'REVERSE',
    'olus2OOo': 'DUP',
    'olus2Oo0': 'SWAP',
    'olus2OoO': 'DROP',
    'olus2Ooo': 'ROT',
    'olus2o00': 'DEPTH',
    'olus2o0O': 'IF',
    'olus2o0o': 'ELSE',
    'olus2oO0': 'THEN',
    'olus2oOO': 'WHILE',
    'olus2oOo': ':',
    'olus2oo0': ';',
    'olus2ooO': 'GET',
    'olus2ooo': 'SET',
}

def olus_to_ternary(olus):
    last_three = olus[-3:]
    d = {
        '0': 0,
        'O': 1,
        'o': 2
    }
    return str(d[last_three[0]]) + str(d[last_three[1]]) + str(d[last_three[2]])

def main():
    code = ""
    if len(sys.argv) == 1:
        code = input()
    else:
        file = sys.argv[1]
        with open(file, 'r') as f:
            code = f.read()
    code_as_list = []
    last = 0
    in_string = False

    s = []
    for i, c in enumerate(code):
        if c == '\"':
            code_as_list.append(code[last:i + int(in_string)])
            last = i + int(in_string)

            in_string = not in_string
            s = []
            continue
        s.append(c)

    if last == 0:
        code_as_list.append(code[last:])
    else:
        code_as_list.append(code[last+1:])

    fin = []
    for c in code_as_list:
        if c.startswith('\"'):
            fin.append(c)
        else:
            fin.extend(c.split())
    code = fin

    NUMBER_MODE = 0
    COMMENT_MODE = 1
    NORMAL_MODE = 2

    mode = NORMAL_MODE
    negative_number = False

    result = []
    num = ''
    for op in code:
        prev_mode = mode
        if op == '0lus2000!':
            mode = NUMBER_MODE
            negative_number = False
        elif op == '0lus2ooo!':
            mode = NUMBER_MODE
            negative_number = True
        elif op == 'Olus2000!':
            mode = COMMENT_MODE
        elif op == 'olus2000!':
            mode = NORMAL_MODE

        if mode != NUMBER_MODE and prev_mode == NUMBER_MODE:
            if negative_number:
                num = '-' + num
            result.append(num)
            num = ''

        if mode != prev_mode:
            continue

        if mode == NUMBER_MODE:
            num += olus_to_ternary(op)
        elif mode == NORMAL_MODE:
            try:
                result.append(olus2000_dict[op])
            except KeyError:
                result.append(op)

    if mode == NUMBER_MODE and num != '':
        result.append(num)
    print(' '.join(result))

if __name__ == "__main__":
    main()
